setwd("G:\\PGDBA\\Kaggle\\DSG2017\\data")

library(data.table)
train= fread("train.csv")
test= fread("test.csv")
test$sample_id=NULL

#combine train/test
all_data= dplyr::bind_rows(train, test)

#Change data types
all_data$platform_family= as.factor(all_data$platform_family)
all_data$platform_name= as.factor(all_data$platform_name)

#### convert ts_listen to get year, month, data ####
library(lubridate)
ts_vars<-function(df){
  df[,'ts_listen_dt']= as.Date(as.POSIXct(df[,ts_listen], origin="1970-01-01", tz="cet"))
  df[,'release_dt']= ymd(df[, release_date])
  df[,'ts_year']= year(df$ts_listen_dt)
  df[,'ts_month']= month(df$ts_listen_dt)
  df[,'ts_day']= day(df$ts_listen_dt)
  df[,'ts_wday']= wday(df$ts_listen_dt)
  return(df)
}

all_data= ts_vars(all_data)

# Extract year of release 
all_data$yr_release= stringr::str_sub(all_data$release_date, 1, 4)
all_data$yr_release= as.numeric(all_data$yr_release)

# weeks since song release
all_data$weeks_since_release= as.numeric(difftime(all_data$ts_listen_dt, all_data$release_dt, units='weeks'))
all_data$weeks_since_release= ifelse(all_data$weeks_since_release<0, 0, all_data$weeks_since_release) #change neg values to 0

train= all_data[1:nrow(train)]
test= all_data[(nrow(train)+1):nrow(all_data)]

# remove all timestamps < 2016 (815/total)
train= train[train$ts_year==2016,]

# Find % of unique songs & other freq. variables in user's history
df=train[is_listened==1,
         .(tot_songs_heard= length(media_id),
           cnt_unique_songs= length(unique(media_id)), 
           cnt_unique_artists= length(unique(artist_id))),
         by="user_id" ]

train= merge(train, df, by="user_id", all.x=T)
train$prop_unique_songs= train$cnt_unique_songs/ train$tot_songs_heard
test= merge(test, df, by="user_id", all.x=T)
test$prop_unique_songs= test$cnt_unique_songs/ test$tot_songs_heard

# Prior/ user: Define moving avg prior prob. of listening per user
#prior= (#of time user listened song till previous timestamp/ #of songs recommended uptil prev timestamp)
#ie at each timestep, consider the previous entire history to calculate the prior

train= train[order(user_id, ts_listen)]  #order data by (user, timestamp)
df=train[, .(csum= cumsum(is_listened), seq= seq_along(is_listened),ts_listen), by="user_id"]  

#csum: capture cumulative sum of times user listened to song 
#seq: running counter of number of songs recommended

df$prior= df[, .(prior=csum/seq)]  
prior_lag= df[, .(prior=shift(prior, n=1, fill=0.5 , type='lag')), by="user_id"] 
#Take lag-1 of prior (because prior var includes is_listened value of current timestep too, 
#pad prior of 1st ts of user with prob 0.5 

df$prior= prior_lag$prior
df$csum=NULL
df$seq=NULL
train= merge(train, df, by=c("user_id", "ts_listen"), all.x=T)  #merge prior prob. feature to train set

df= prior_lag[,lapply(.SD, tail,1), by="user_id", .SDcols="prior" ]  #find the latest (last ts) prior for each user
test= merge(test, df, by="user_id", all.x=T)  #update prior for users in the test set

'''
#Find top 10 genres (feature not useful as 48% songs belong to genre 0)
sort(prop.table(table(train$genre_id[train$is_listened==1])), decreasing=T)[1:10]
#0          7         10         25         27         14        734        297         77       2744 

##Number of genres per user
df=train[, .(listens=.N), by=c("user_id", "genre_id","is_listened"), ][is_listened==1]  #.N calculates count of listens by group

df1=df[, .(tot_genres=.N, tot_listens=sum(listens)), by="user_id"]  # calculate total genres heard, total songs heard by user

df= merge(df, df1, by="user_id" ,all.x = T)
#calculate genre preference prior based on (#of listens of a genre/# tot listens)
df$genre_prior_prob= df$listens/df$tot_listens  

#Filter prior prob. variable for top-10 genres
df= df[genre_id %in% c(0,7,10,25,27,14,734,297,77,2744)] 
df= df[, .SD, .SDcols=c(1,2,7)]  #select cols 1,2,7 for use

#create genre_id variables in train & test
library(reshape)
temp= melt.data.frame(data=df, id.vars = c("user_id", "genre_id"), variable_name = "genre_prior_prob")  #convert to long format
temp=cast(temp, user_id ~ genre_id, sum)  #convert to wide format (get genre ids as columns)
colnames(temp)= c("user_id", paste0("G_", c(0,7,10,25,27,14,734,297,77,2744)))

train= merge(train, temp, by="user_id", all.x = T)  #create genre id var in train
test= merge(test, temp, by="user_id", all.x = T)
'''

train[is.na(train)]=0
test[is.na(test)]=0


#### Create prev_heard function ####
# Input: <type> parameter, can be media_id/genre_id/album_id/artist_id
# Output: 1. whether user has heard <type> previously (0/1)
#         2. Days since <type> heard

prev_heard<-function(type){
  
  t1=paste0("df= train[, .(user_id, ts_listen,", type,")]")  #select relevant cols
  eval(parse(text=t1))
  
  df= merge(df, df, by=c("user_id", as.character(type)), allow.cartesian=T)  #self join to compare timestamps in next step
  df= df[ts_listen.x>ts_listen.y]  #filter rows where <type> was previously heard
  
  t2=paste0("df$days_since_", type," = (df$ts_listen.x- df$ts_listen.y)/86400")  #convert difference in listen_ts to days
  eval(parse(text=t2))
  
  #among all possible day_differences for a (user, type, ts) tuple pick the one with smallest day difference (get latest history)
  t3=paste0("df = df[, .(days_since_",type, "= min(days_since_",type,")), by=c('user_id','ts_listen.x', as.character(type))]")  
  eval(parse(text=t3))
  
  df[, paste0("prev_listened_", type)]= rep(1, nrow(df))  #create prev_listened_<type> indicator variable 
  names(df)[2]= "ts_listen"
  df= merge(train, df, by=c('user_id', 'ts_listen'), all.x=T ) 
  return(df)
}


train= prev_heard(type="media_id")  #creates prev_listened_media_id and days_since_media_id listened features
train= prev_heard(type="artist_id")
train= prev_heard(type="album_id")
#train= prev_heard(type="genre_id") #useless
train[is.na(train)]=0  #replace NA values of prev_listen function to 0

# check whether hashing is useful (to do)
# #combine prev_listened_<type> features to get hash. eg- 0101 => artist and genre previously heard
# train$prev_heard= 1000*train$prev_listened_media_id + 
#   100*train$prev_listened_artist_id+ 
#   10* train$prev_listened_album_id+
#   train$prev_listened_genre_id
# 
# cols= c("media_id", "artist_id", "album_id", "genre_id")  #remove prev_listened_<type> features
# for(co in cols){
#   eval(parse(text= paste0("train$prev_listened_",co, "= NULL")))
# }


#### Define function to get song/artist/album score by listen_type variable (listen_type=1: Flow recommended) ####
# score = prop. of times songs was listened to by month
# <type> : song/artist/album id
# ts: month of listening
# for each month of 2016, calculate % of times <type> was heard

get_score <-function(type){
  scores= data.table() #create empty df to hold scores
  for(ts in 1:12){
    t1= paste0("df=train[listen_type== 0 & ts_month <= ts , .(",type,"_cnt_lt0= .N, ",type,"_listened_lt0= sum(is_listened)), by='",type,"']")
    eval(parse(text=t1))
    t2= paste0("df$",type,"_score_lt0= df$",type,"_listened_lt0/df$",type,"_cnt_lt0")  #type score for month= ts
    eval(parse(text=t2))
    
    #same as above for listen_type=1
    t1= paste0("dff=train[listen_type== 1 & ts_month <= ts , .(",type,"_cnt_lt1= .N, ",type,"_listened_lt1= sum(is_listened)), by='",type,"']")
    eval(parse(text=t1))
    t2= paste0("dff$",type,"_score_lt1= dff$",type,"_listened_lt1/dff$",type,"_cnt_lt1")  #song score for listen_type=1
    eval(parse(text=t2))
    
    df=merge(df, dff, all.x = T, all.y = T, by=as.character(type))  #merge results
    
    #get overall popularity score (irrespective of listen_type)
    t3= paste0("dff=train[ts_month <= ts , .(",type,"_cnt_net= .N, ",type,"_listened_net= sum(is_listened)), by='",type,"']")
    eval(parse(text=t1))
    t4= paste0("dff$",type,"_score_net= dff$",type,"_listened_net/dff$",type,"_cnt_net")  #song net score
    eval(parse(text=t2))
    
    df= merge(dff, df, all.x = T, by=as.character(type))
    
    df$ts_month= rep(ts, nrow(df))  #tag month to score
    scores= bind_rows(scores, df)   #append scores of each month
  }
  train=merge(train, scores, by=c(as.character(type), "ts_month"), all.x=T)
  train[is.na(train)]=0
  return(train)
  
}

train= get_score('media_id')
train= get_score('artist_id')

#save train file
write.csv(train, "train_processed.csv", row.names = F)
