#full moon phase is 12/3-12/10
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


no.of.tweets <- 10000 #this was only 1000 for first night of data collection 11/26
  
  twitter_function<-function(hashtag) {
    
  tweets_w_hashtag <- searchTwitter(hashtag, 
                                   n=no.of.tweets, 
                                   lang="en", 
                                   #geocode = '82.5, 25, -70, 47',
                                   since=as.char(Sys.Date()-1),
                                   until=as.char(Sys.Date()))
  
  hashtag_df<- tbl_df(map_df(tweets_w_hashtag, as.data.frame))
  hashtag_df<-hashtag_df%>%mutate(week=week(Sys.Date()))%>%
                        group_by(week)%>%
                        summarise(count=n())

    return(hashtag_df)
  }
  insomnia_tweets<-twitter_function('#insomnia')
  cantsleep_tweets<-twitter_function('#cantsleep')
  wideawake_tweets<-twitter_function('#wideawake')
  nosleep_tweets<-twitter_function('#nosleep')
  
#twitter_tokens <- create_token(app = "my_app",
 #   consumer_key = "xa1wWUs5shIvAcot57r7xhQZE", 
  #  consumer_secret = "AQFXM5ZQ2YMoq1qIhzlYsU5j7ASULzT8wwEw9He6upZVnMRMKw")

# path of home directory
#home_directory <- path.expand("/Users/chasehenley")

## combine with name for token
#file_name <- file.path(home_directory, "twitter_tokens.rds")

## save token to home directory
#saveRDS(twitter_tokens, file = file_name)

## On my mac, the .Renviron text looks like this:
  #   TWITTER_PAT=/Users/mwk/twitter_token.rds

## assuming you followed the procodures to create "file_name"
##     from the previous code chunk, then the code below should
##     create and save your environment variable.
#cat(paste0("TWITTER_PAT=", file_name),
 #  file = file.path(home_directory, ".Renviron"),
  # append = TRUE)
#one minute of tweets in the eastern time zone
#eastern_time_zone <- stream_tweets(
 # c(-82.5, 25, -70, 47),
  #timeout = 60)


  three_am_total<- eastern_time_zone%>%
                      mutate(week=week(Sys.Date()))%>%
                      group_by(week)%>%
                       summarise(late_night_count=n())


lunar_stats <- function(file_json){
    
    lunar_data<-fromJSON(file_json)
    lunar_data_frame<-as.data.frame(lunar_data)
    
   
  return(lunar_data_frame)
}

lunar_statistics<-lunar_stats('http://api.usno.navy.mil/moon/phase?year=2017')
lunar_statistics<- lunar_statistics %>% select(date=phasedata.date,
                          phase=phasedata.phase,
                          time=phasedata.time)%>%
                     mutate(Date=as.Date((date),"%Y %b %d"), week = as.numeric(week(Date)))%>%
                     select(phase,Date,time,week)

master_table<-three_am_total%>%
  full_join(lunar_statistics, by = 'week')%>%
  group_by(phase)#%>%
#  summarise(total_hashtag_tweet_count = sum(totalcount,na.rm = TRUE), num_late_night_tweets = sum(late_night_count))
#added three am tweets to the master table

master_table<-master_table%>% full_join(insomnia_tweets,by='week') %>%full_join(cantsleep_tweets,by='week')%>%
  full_join(wideawake_tweets,by='week')%>%
  full_join(nosleep_tweets, by='week')%>%
  mutate(insomnia_count = count.x, cantsleep_count = count.y, wideawake_count = count.x.x, nosleep_count = count.y.y)%>%
  select(week,phase,Date, time, insomnia_count,cantsleep_count,wideawake_count,nosleep_count,late_night_count)%>%
  group_by(week)

total_count_tbl<-master_table%>%group_by(week)%>%
  summarise(totalcount=insomnia_count+cantsleep_count+wideawake_count+nosleep_count)

system('mkdir -p data') # create directory if not exists
date_time <- gsub(':', '-', Sys.time()) # clean up the datetime
date_time <- gsub(' ', '_', date_time)
file_name <- sprintf('data/tweets_%s.csv', date_time) # write a new file each time
write.csv(master_table, file_name, row.names=F)



if(Sys.Date()="2017-12-02")
  {
  aggregated_table <-read_csv("tweets_2017-11-26_08-01-12.csv")
  aggregated_table<-aggregated_table%>%head(1)%>%select(week,phase,insomnia_count,cantsleep_count,wideawake_count,nosleep_count,late_night_count)%>%
    summarise(week,phase,insomnia_count,cantsleep_count,wideawake_count,nosleep_count,late_night_count)
  }

#converts csv files to dataframes and then combines them
data_frame_merger<-function(csv){
 
  df<-read_csv(csv)
  
 df<-df%>%head(1)%>%select(week, phase,insomnia_count,cantsleep_count,wideawake_count,nosleep_count,late_night_count)%>%
   summarise(week,phase,insomnia_count,cantsleep_count,wideawake_count,nosleep_count,late_night_count)

  aggregated_table<-bind_rows(aggregated_table,df)
  
  aggregated_table<-aggregated_table%>%
    group_by(week)%>%
    summarise(insomnia_count= sum(insomnia_count),cantsleep_count=sum(cantsleep_count),
              wideawake_count=sum(wideawake_count),nosleep_count=sum(nosleep_count),late_night_count=sum(late_night_count))
  
  aggregated_table <-aggregated_table%>%
    head(1)%>%
    mutate(week = df$week, phase = df$phase)
  
 return(aggregated_table)
}

if(Sys.Date()="2017-12-02")
{
aggregated_table<-data_frame_merger("tweets_2017-11-27_08-01-14.csv")
aggregated_table<-data_frame_merger("tweets_2017-11-28_08-01-19.csv")
aggregated_table<-data_frame_merger("tweets_2017-11-29_08-01-15.csv")
aggregated_table<-data_frame_merger("tweets_2017-11-30_08-01-15.csv")
aggregated_table<-data_frame_merger("tweets_2017-12-01_08-01-14.csv")
}

date_Time <- gsub(':', '-', Sys.time()) # clean up the datetime
date_Time <- gsub(' ', '_', date_Time)
file_Name <- sprintf('data/tweets_%s.csv', date_Time) # write a new file each time
aggregated_table<-data_frame_merger(file_Name)


#calculated mean for each count category by phase in order to compute 2 sample t test for means
first_quarter <- aggregated_table %>%
              filter(phase == 'First Quarter') %>% 
              summarise(insomnia_count= sum(insomnia_count), insomnia_avg= mean(insomnia_count),
                        cantsleep_count=sum(cantsleep_count), cantsleep_avg=mean(cantsleep_count),
                        wideawake_count=sum(wideawake_count), wideawake_avg=mean(wideawake_count),
                        nosleep_count=sum(nosleep_count), nosleep_avg=mean(nosleep_count),
                        late_night_count=sum(late_night_count), late_night_avg=mean(late_night_count))

full_moon <- aggregated_table %>%
  filter(phase == 'Full Moon') %>% 
  summarise(insomnia_count= sum(insomnia_count), insomnia_avg= mean(insomnia_count),
            cantsleep_count=sum(cantsleep_count), cantsleep_avg=mean(cantsleep_count),
            wideawake_count=sum(wideawake_count), wideawake_avg=mean(wideawake_count),
            nosleep_count=sum(nosleep_count), nosleep_avg=mean(nosleep_count),
            late_night_count=sum(late_night_count), late_night_avg=mean(late_night_count))

last_quarter <- aggregated_table %>%
  filter(phase == 'Last Quarter') %>% 
  summarise(insomnia_count= sum(insomnia_count), insomnia_avg= mean(insomnia_count),
            cantsleep_count=sum(cantsleep_count), cantsleep_avg=mean(cantsleep_count),
            wideawake_count=sum(wideawake_count), wideawake_avg=mean(wideawake_count),
            nosleep_count=sum(nosleep_count), nosleep_avg=mean(nosleep_count),
            late_night_count=sum(late_night_count), late_night_avg=mean(late_night_count))

new_moon <- aggregated_table %>%
  filter(phase == 'New Moon') %>% 
  summarise(insomnia_count= sum(insomnia_count), insomnia_avg= mean(insomnia_count),
            cantsleep_count=sum(cantsleep_count), cantsleep_avg=mean(cantsleep_count),
            wideawake_count=sum(wideawake_count), wideawake_avg=mean(wideawake_count),
            nosleep_count=sum(nosleep_count), nosleep_avg=mean(nosleep_count),
            late_night_count=sum(late_night_count), late_night_avg=mean(late_night_count))

#insomnia_avg
ifelse(t.test(full_moon$insomnia_avg ~ first_quarter$insomnia_avg) <= .1, print("Significant: full_moon$insomnia_avg ~ first_quarter$insomnia_avg"),print())
ifelse(t.test(full_moon$insomnia_avg ~ new_moon$insomnia_avg) <= .1, print("Significant: full_moon$insomnia_avg ~ new_moon$insomnia_avg"),print())
ifelse(t.test(full_moon$insomnia_avg ~ last_quarter$insomnia_avg) <= .1, print("Significant: full_moon$insomnia_avg ~ last_quarter$insomnia_avg"),print())

#cantsleep_avg
ifelse(t.test(full_moon$cantsleep_avg ~ first_quarter$cantsleep_avg) <= .1, print("Significant: full_moon$cantsleep_avg ~ first_quarter$cantsleep_avg"),print())
ifelse(t.test(full_moon$cantsleep_avg ~ new_moon$cantsleep_avg) <= .1, print("Significant: full_moon$cantsleep_avg ~ new_moon$cantsleep_avg"),print())
ifelse(t.test(full_moon$cantsleep_avg ~ last_quarter$cantsleep_avg) <= .1, print("Significant: full_moon$cantsleep_avg ~ last_quarter$cantsleep_avg"),print())

#wideawake_avg
ifelse(t.test(full_moon$wideawake_avg ~ first_quarter$wideawake_avg) <= .1, print("Significant: full_moon$wideawake_avg ~ first_quarter$wideawake_avg"),print())
ifelse(t.test(full_moon$wideawake_avg ~ new_moon$wideawake_avg)<= .1, print("Significant: full_moon$wideawake_avg ~ new_moon$wideawake_avg"),print())
ifelse(t.test(full_moon$wideawake_avg ~ last_quarter$wideawake_avg) <= .1, print("Significant: full_moon$wideawake_avg ~ last_quarter$wideawake_avg"),print())

#nosleep_avg
ifelse(t.test(full_moon$nosleep_avg ~ first_quarter$nosleep_avg) <= .1, print("Significant: full_moon$nosleep_avg ~ first_quarter$nosleep_avg"),print())
ifelse(t.test(full_moon$nosleep_avg ~ new_moon$nosleep_avg)<= .1, print("Significant: full_moon$nosleep_avg ~ new_moon$nosleep_avg"),print())
ifelse(t.test(full_moon$nosleep_avg ~ last_quarter$nosleep_avg) <= .1, print("Significant: full_moon$nosleep_avg ~ last_quarter$nosleep_avg"),print())

#late_night_avg
ifelse(t.test(full_moon$late_night_avg ~ first_quarter$late_night_avg) <= .1, print("Significant: full_moon$late_night_avg ~ first_quarter$late_night_avg"),print())
ifelse(t.test(full_moon$late_night_avg ~ new_moon$late_night_avg)<= .1, print("Significant: full_moon$late_night_avg ~ new_moon$late_night_avg"),print())
ifelse(t.test(full_moon$late_night_avg ~ last_quarter$late_night_avg) <= .1, print("Significant: full_moon$late_night_avg ~ last_quarter$late_night_avg"),print())

visual_df<-aggregated_table %>%
  group_by(phase) %>% 
  summarise(phase,insomnia_count= sum(insomnia_count), insomnia_avg= mean(insomnia_count),
            cantsleep_count=sum(cantsleep_count), cantsleep_avg=mean(cantsleep_count),
            wideawake_count=sum(wideawake_count), wideawake_avg=mean(wideawake_count),
            nosleep_count=sum(nosleep_count), nosleep_avg=mean(nosleep_count),
            late_night_count=sum(late_night_count), late_night_avg=mean(late_night_count))

#plots
visual_df%>%ggplot(aes(x=phase,y=insomnia_avg))+geom_bar(stat="identity")+ggtitle("Average Number of Tweets per Night with #insomnia")+xlab("Phase")+ylab("Average Number of Tweets")
visual_df%>%ggplot(aes(x=phase,y=canstsleep_avg))+geom_bar(stat="identity")+ggtitle("Average Number of Tweets per Night with #cantsleep")+xlab("Phase")+ylab("Average Number of Tweets")
visual_df%>%ggplot(aes(x=phase,y=wideawake_avg))+geom_bar(stat="identity")+ggtitle("Average Number of Tweets per Night with #wideawake")+xlab("Phase")+ylab("Average Number of Tweets")
visual_df%>%ggplot(aes(x=phase,y=nosleep_avg))+geom_bar(stat="identity")+ggtitle("Average Number of Tweets per Night with #nosleep")+xlab("Phase")+ylab("Average Number of Tweets")
visual_df%>%ggplot(aes(x=phase,y=late_night_avg))+geom_bar(stat="identity")+ggtitle("Average Number of Tweets per Night at 3 am")+xlab("Phase")+ylab("Average Number of Tweets")


