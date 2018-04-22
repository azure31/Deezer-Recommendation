setwd("G:\\PGDBA\\Kaggle\\DSG2017")

SUBMISSION_FILENAME= "Submission1.csv"

library(data.table)
train= fread("train.csv")
test= fread("test.csv")
test$sample_id=NULL

#combine train/test
all_data= dplyr::bind_rows(train, test)

#Change data types
all_data$platform_family= as.factor(all_data$platform_family)
all_data$platform_name= as.factor(all_data$platform_name)

########## User- Feature engineering ##########

#Extract year of release 
all_data$yr_release= stringr::str_sub(all_data$release_date, 1, 4)
all_data$yr_release= as.numeric(all_data$yr_release)

train= all_data[1:nrow(train)]
test= all_data[(nrow(train)+1):nrow(all_data)]


#Find % of unique songs & other freq. variables in user's history
df=train[is_listened==1,
         .(tot_songs_heard= length(media_id),
           cnt_unique_songs= length(unique(media_id)), 
           cnt_unique_artists= length(unique(artist_id))),
         by="user_id" ]

train= merge(train, df, by="user_id", all.x=T)
train$prop_unique_songs= train$cnt_unique_songs/ train$tot_songs_heard

test= merge(test, df, by="user_id", all.x=T)
test$prop_unique_songs= test$cnt_unique_songs/ test$tot_songs_heard

#Find user cluster (**TO DO)

#Define moving avg prior prob. of listening per user
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

#Find top 10 genres
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

train[is.na(train)]=0
test[is.na(test)]=0

#convert ts_listen to get year, month, data
library(lubridate)

#Define function to extract date,day, week, month
ts_vars<-function(df){
  df[,'ts_listen_dt']= as.Date(as.POSIXct(df[,ts_listen], origin="1970-01-01", tz="cet"))
  df[,'ts_year']= year(df$ts_listen_dt)
  df[,'ts_month']= month(df$ts_listen_dt)
  df[,'ts_day']= day(df$ts_listen_dt)
  df[,'ts_wday']= wday(df$ts_listen_dt)
  return(df)
}

train= ts_vars(train)

##Create prev_heard function 
##parameter type can be media_id/genre_id/album_id/artist_id
##Input: type parameter
##Output: 1. whether user has heard <type> previously (0/1)
##2. Days since <type> heard

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
train= prev_heard(type="genre_id")

#combine prev_listened_<type> features to get hash. eg- 0101 => artist and genre previously heard
train$prev_heard= 1000*train$prev_listened_media_id + 
                  100*train$prev_listened_artist_id+ 
                  10* train$prev_listened_album_id+
                  train$prev_listened_genre_id

train[is.na(train)]=0  #replace NA values of prev_listen function to 0

cols= c("media_id", "artist_id", "album_id", "genre_id")  #remove prev_listened_<type> features
for(co in cols){
  eval(parse(text= paste0("train$prev_listened_",co, "= NULL")))
  }


#Year distribution has noise
#1970    1980    1996    2001    2009    2010    2011    2012    2013    2014    2015    2016 
#249     379       1       2     115       7       7       1      19       3      29 7558022 
#Consider dropping rows where year is <2009 ?

#### User- feature engineering ####
#Maintain a separate user df for storing user-features

user= all_data[, .SD, .SDcols= c(12, 11, 14)]  
user= unique(user, by="user_id")
df= train[, .SD, .SDcols= c(1,17:31)]
df= unique(df, by="user_id")
user= merge(user, df, by="user_id", all.x=T)

write.csv(user, file="user_features.csv", row.names=F)


#### Item- feature engineering ####
#Maintain a separate song df for storing song-features

song= all_data[, .SD, .SDcols=c(3,4,13,1,9,16)]
song= unique(song, by="media_id")

#no. of unique listeners of song
df=train[is_listened==1, .(unique_user_cnt=length(unique(user_id))), by="media_id"]
song= merge(song, df, by="media_id", all.x=T)
song$unique_user_cnt[which(is.na(song$unique_user_cnt))]=0
song$media_duration = song$media_duration/60


#get song score
df=train[, .(song_rec_cnt= .N, song_listened= sum(is_listened)), by="media_id"]
song=merge(song, df, by="media_id", all.x=T)
song[is.na(song)]=0
song$song_score= song$song_listened/song$song_rec_cnt  ##song score

#get artist score (find cnt and %conversion of all songs of the artist)
df=train[, .(artist_rec_cnt= .N, artist_listened= sum(is_listened)), by="artist_id"]
df$artist_score= df$artist_listened/df$artist_rec_cnt
song= merge(song, df, by="artist_id", all.x=T)

#get album score (find cnt and %conversion of all songs of the album)
df=train[, .(album_rec_cnt= .N, album_listened= sum(is_listened)), by="album_id"]
df$album_score= df$album_listened/df$album_rec_cnt
song= merge(song, df, by="album_id", all.x=T)

#get genre score (find cnt and %conversion of all songs of the genre)
df=train[, .(genre_rec_cnt= .N, genre_listened= sum(is_listened)), by="genre_id"]
df$genre_score= df$genre_listened/df$genre_rec_cnt
song= merge(song, df, by="genre_id", all.x=T)

song[is.na(song)]=0
song= song[order(media_id)]
song$song_hash_id= 0: (nrow(song)-1)
write.csv(song, "item_features.csv", row.names=F)

############################################################################################

## Merge all song features into train/test ##
#cols= c() pick song features to be considered

train= merge(train, song[, .SD, .SDcols=cols], by="media_id", all.x=T) 
test= merge(test, song[, .SD, .SDcols=cols], by="media_id", all.x=T) 

#Imputation for missing values
train[is.na(train)]=0
test[is.na(test)]=0

##Random Forest

library(randomForest)

set.seed(131)
samples= sample(size=100000,x=1:nrow(train) ,replace=F)
X_train= train[samples[1:90000],]
X_val= train[samples[90001:100000]]

#fit model
fit_rf= randomForest(as.factor(is_listened)~.- user_id- genre_id - media_id- album_id- release_date - artist_id, data=X_train)
varImpPlot(fit_rf)  #find imp. predictors
print(fit_rf)

#Check performance on train
pred= predict(fit_rf, data= X_train)
caret::confusionMatrix(fit_rf$predicted, reference=train$is_listened[samples[1:90000]], positive='1')

#Check performance on val
pred= predict(fit_rf, newdata= X_val ,type="response")
caret::confusionMatrix(pred, reference=X_val$is_listened, positive='1')

#Generate predictions for test set
pred_test= predict(fit_rf, newdata= X_val ,type="response")

##### Generate submission  #####
submit= data.frame(sample_id= seq(0, 19917),is_listened= pred_test[,2] )
write.csv(submit, SUBMISSION_FILENAME, row.names = F)
