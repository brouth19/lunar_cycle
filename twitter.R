---
title: "Data"
output: html_document
---

#knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(twitteR)
library(ROAuth)
library(devtools)
library(httr)
library(jsonlite)
library(readr)
library(RDS)
library(lubridate)
library(magrittr)
library(rtweet)
library(httpuv)


# Download "cacert.pem" file
#download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#create an object "cred" that will save the authenticated object that we can use for later sessions
#cred <- OAuthFactory$new(consumerKey='xa1wWUs5shIvAcot57r7xhQZE',
  #   consumerSecret='AQFXM5ZQ2YMoq1qIhzlYsU5j7ASULzT8wwEw9He6upZVnMRMKw',
   #  requestURL='https://api.twitter.com/oauth/request_token',
  #   accessURL='https://api.twitter.com/oauth/access_token',
  #  authURL='https://api.twitter.com/oauth/authorize')
# Executing the next step generates an output --> To enable the connection, please direct your web browser to: <hyperlink> . Note:  You only need to do this part once

#cred$handshake(cainfo="cacert.pem")

  # save(cred, file="twitter authentication.Rdata")
   
  # load("twitter authentication.Rdata")
   
  consumer_key<-"xa1wWUs5shIvAcot57r7xhQZE"
  consumer_secret<-"AQFXM5ZQ2YMoq1qIhzlYsU5j7ASULzT8wwEw9He6upZVnMRMKw"
  access_token<-"805657646-BqiYCdamk9L8dlEvENPCmDhjXFNIv7RCLROdvREK"
  access_secret<-"s7sySayvq3Pvmj7Mg0aOeIxdBDAFR8H4sVJZb86Bj7Iyi"


  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)





no.of.tweets <- 10
  
  twitter_function<-function(hashtag) {
    
  tweets_w_hashtag <- searchTwitter(hashtag, 
                                   n=no.of.tweets, 
                                   lang="en", 
                                   since=as.char(Sys.Date()-1),
                                   until=as.char(Sys.Date()))
  
  hashtag_df<- tbl_df(map_df(tweets_w_hashtag, as.data.frame))
  hashtag_df%>%mutate(week=week(Sys.Date()))%>%
                        summarise(count=n())

    return(hashtag_df)
  }
  
  test<-twitter_function("#insomnia")
  write.csv(test,"test.csv")
  
if(Sys.Date() == "2017-11-20"){
  insomnia_tweets<-twitter_function('#insomnia')
  cantsleep_tweets<-twitter_function('#cantsleep')
  wideawake_tweets<-twitter_function('#wideawake')
  nosleep_tweets<-twitter_function('#nosleep')
} else{
  insomnia_one_day<-twitter_function('#insomnia')
  cantsleep_one_day<-twitter_function('#cantsleep')
  wideawake_one_day<-twitter_function('#wideawake')
  nosleep_one_day<-twitter_function('#nosleep')

  insomnia_tweets<- rbind(insomnia_tweets, insomnia_one_day) %>%
                      group_by(week)
  cantsleep_tweets<- rbind(cantsleep_tweets, cantsleep_one_day) %>%
                      group_by(week)
  wideawake_tweets<- rbind(wideawake_tweets, wideawake_one_day) %>%
                      group_by(week)
  nosleep_tweets<- rbind(nosleep_tweets, nosleep_one_day) %>%
                      group_by(week)
}
 
  test<-twitter_function("#insomnia")
  write.csv(test,"test.csv")

#master_table<-insomnia_tweets%>%full_join(cantsleep_tweets,by='week')%>%
 #                 full_join(wideawake_tweets,by='week')%>%
 #                 full_join(nosleep_tweets, by='week')%>%
 #                 mutate(insomnia_count = count.x, cantsleep_count = count.y, wideawake_count = count.x.x, nosleep_count = count.y.y)%>%
    #              summarise(week,totalcount=insomnia_count+cantsleep_count+wideawake_count+nosleep_count)




twitter_tokens <- create_token(app = "my_app",
    consumer_key = "xa1wWUs5shIvAcot57r7xhQZE", 
    consumer_secret = "AQFXM5ZQ2YMoq1qIhzlYsU5j7ASULzT8wwEw9He6upZVnMRMKw")


#one minute of tweets in the eastern time zone
eastern_time_zone <- stream_tweets(
  c(-82.5, 25, -70, 47),
  timeout = 60)
test_Data<-eastern_time_zone
write.csv(test_Data,  paste(getwd(),"/",Sys.Date(),".csv",sep="",collapse=NULL))
#effectively updating the total table to not be rewritten each time the script is run
#Date below has to change to whatever the first date is we plan to automate the script
if(Sys.Date() == "2017-11-20"){
  three_am_total <- eastern_time_zone%>%
                        mutate(week=week(Sys.Date()))%>%
                        summarise(late_night_count=n())
} else{
  three_am_one_day <- eastern_time_zone%>%
                        mutate(week=week(Sys.Date()))%>%
                        summarise(late_night_count=n())
  
  three_am_total <- rbind(three_am_total, three_am_one_day) %>%
                      group_by(week)
}




lunar_stats <- function(file_json){
    
    lunar_data<-fromJSON(file_json)
    lunar_data_frame<-as.data.frame(lunar_data)
    
   
  return(lunar_data_frame)
}

lunar_statistics<-lunar_stats('http://api.usno.navy.mil/moon/phase?year=2017')
lunar_statistics = lunar_statistics %>% select(date=phasedata.date,
                          phase=phasedata.phase,
                          time=phasedata.time)%>%
                     mutate(Date=as.Date((date),"%Y %b %d"), week = as.numeric(week(Date)))%>%
                     select(phase,Date,time,week)




#added three am tweets to the master table
master_table%>%full_join(lunar_statistics, by = 'week')%>%
               full_join(three_am_total, by = 'week')%>%
               group_by(phase)%>%
               summarise(total_hashtag_tweet_count = sum(totalcount,na.rm = TRUE), num_late_night_tweets = sum(late_night_count))

test_Data<-eastern_time_zone
write.csv(test_Data,  paste(getwd(),"/",Sys.time(),".csv",sep="",collapse=NULL))


