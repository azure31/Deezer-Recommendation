library(shiny)
library(ggplot2)

shinyServer(function(input, output){
  output$distPlot <-renderPlot({
    df=train[(media_id==input$mid & is_listened==1), .(cnt=.N) , by=ts_listen_dt]
    ggplot2::ggplot(df,aes(x=ts_listen_dt, y=cnt))+ geom_line()+ ggtitle("Song popularity over time")
    #+scale_x_date(limits=as.Date(c("2016-01-01", "2016-01-12"))) 
    })
  
  output$info <- renderPrint(unique(train[media_id==input$mid, .SD, .SDcols=c(1,4,13)], by="artist_id"))
  
  })