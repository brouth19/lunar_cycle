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


 #set up your own authorization to the Twitter API
 # consumer_key<-
 # consumer_secret<-
 # access_token<-
 # access_secret<-


  #setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

  #for efficiency purposes 10,000 should be the max
  no.of.tweets <- 10000 
  
  twitter_function<-function(hashtag) {
    
  tweets_w_hashtag <- searchTwitter(hashtag, 
                                   n=no.of.tweets, 
                                   lang="en", 
                                   since=as.char(Sys.Date()-1),
                                   until=as.char(Sys.Date()))
  
  hashtag_df<- tbl_df(map_df(tweets_w_hashtag, as.data.frame))
  hashtag_df<-hashtag_df%>%mutate(week=week(Sys.Date()))%>%
                        group_by(week)%>%
                        summarise(count=n())

    return(hashtag_df)
  }
  insomnia_tweets<-twitter_function('#insomnia') #does a search for most recent tweets with this hashtag
  cantsleep_tweets<-twitter_function('#cantsleep')
  wideawake_tweets<-twitter_function('#wideawake')
  nosleep_tweets<-twitter_function('#nosleep')

  #live stream 60 seconds of tweets on the East Coast  
eastern_time_zone <- stream_tweets(
  c(-82.5, 25, -70, 47),
  timeout = 60)


three_am_total<- eastern_time_zone%>%
                      mutate(week=week(Sys.Date()))%>%
                      group_by(week)%>%
                       summarise(late_night_count=n())


lunar_stats <- function(file_json){
    
    lunar_data<-fromJSON(file_json)
    lunar_data_frame<-as.data.frame(lunar_data)
    
   
  return(lunar_data_frame)
}

#reads in lunar calender
lunar_statistics<-lunar_stats('http://api.usno.navy.mil/moon/phase?year=2017')
lunar_statistics<-lunar_statistics %>% select(date=phasedata.date,
                          phase=phasedata.phase,
                          time=phasedata.time)%>%
                     mutate(Date=as.Date((date),"%Y %b %d"), week = as.numeric(week(Date)))%>%
                     select(phase,Date,time,week)

master_table<-three_am_total%>%
  full_join(lunar_statistics, by = 'week')%>%
  group_by(phase)

#added three am tweets to the master table

master_table<-master_table%>% full_join(insomnia_tweets,by='week') %>%full_join(cantsleep_tweets,by='week')%>%
  full_join(wideawake_tweets,by='week')%>%
  full_join(nosleep_tweets, by='week')%>%
  mutate(insomnia_count = count.x, cantsleep_count = count.y, wideawake_count = count.x.x, nosleep_count = count.y.y)%>%
  select(week,phase,Date, time, insomnia_count,cantsleep_count,wideawake_count,nosleep_count,late_night_count)%>%
  group_by(week)

system('mkdir -p data') # create directory if not exists
date_time <- gsub(':', '-', Sys.time()) # clean up the datetime
date_time <- gsub(' ', '_', date_time)
file_name <- sprintf('data/tweets_%s.csv', date_time) # write a new file each time
write.csv(master_table, file_name, row.names=F)

#initializes a data frame with columns and rows but values set to 0
aggregated_table <-read_csv("data/tweets_2017-11-27_08-01-14.csv")
aggregated_table[1,5:9] = 0
aggregated_table<-aggregated_table%>%head(1)%>%select(week,phase,insomnia_count,cantsleep_count,wideawake_count,nosleep_count,late_night_count)%>%
    summarise(week,phase,insomnia_count,cantsleep_count,wideawake_count,nosleep_count,late_night_count)


#converts csv files to dataframes and then combines them
data_frame_merger<-function(csv){
 
 df<-read_csv(csv)
  
 df<-df%>%head(1)%>%select(week, phase,insomnia_count,cantsleep_count,wideawake_count,nosleep_count,late_night_count)%>%
   summarise(week,phase,insomnia_count,cantsleep_count,wideawake_count,nosleep_count,late_night_count)

  aggregated_table<-bind_rows(aggregated_table,df)
  
  aggregated_table<-aggregated_table%>% select(week,phase, insomnia_count,cantsleep_count,wideawake_count,nosleep_count,late_night_count) %>%
    group_by(week, phase)%>%
    summarise(insomnia_count= sum(insomnia_count),cantsleep_count=sum(cantsleep_count),
              wideawake_count=sum(wideawake_count),nosleep_count=sum(nosleep_count),late_night_count=sum(late_night_count))
  
  aggregated_table <- aggregated_table %>% filter(!is.na(phase))
 return(aggregated_table)
}

aggregated_table<-data_frame_merger(file_name)



#calculated mean for each count category by phase in order to compute 2 sample t test for means
first_quarter <- aggregated_table %>%
            filter(phase == 'First Quarter') %>%
            group_by(week) %>%
            summarise(insomnia_count= sum(insomnia_count), insomnia_avg=(insomnia_count)/7,
            cantsleep_count=sum(cantsleep_count), cantsleep_avg=(cantsleep_count)/7,
            wideawake_count=sum(wideawake_count), wideawake_avg=(wideawake_count)/7,
            nosleep_count=sum(nosleep_count), nosleep_avg=(nosleep_count)/7,
            late_night_count=sum(late_night_count), late_night_avg=(late_night_count)/7)

  
full_moon <- aggregated_table %>%
  filter(phase == 'Full Moon') %>% 
  group_by(week) %>%
  summarise(insomnia_count= sum(insomnia_count), insomnia_avg=(insomnia_count)/7,
            cantsleep_count=sum(cantsleep_count), cantsleep_avg=(cantsleep_count)/7,
            wideawake_count=sum(wideawake_count), wideawake_avg=(wideawake_count)/7,
            nosleep_count=sum(nosleep_count), nosleep_avg=(nosleep_count)/7,
            late_night_count=sum(late_night_count), late_night_avg=(late_night_count)/7)

#Did not have time to collect data on this phase
last_quarter <- aggregated_table %>%
  filter(phase == 'Last Quarter') %>% 
  group_by(week) %>%
  summarise(insomnia_count= sum(insomnia_count), insomnia_avg= (insomnia_count)/7,
            cantsleep_count=sum(cantsleep_count), cantsleep_avg=(cantsleep_count)/7,
            wideawake_count=sum(wideawake_count), wideawake_avg=(wideawake_count)/7,
            nosleep_count=sum(nosleep_count), nosleep_avg=(nosleep_count)/7,
            late_night_count=sum(late_night_count), late_night_avg=(late_night_count)/7)

#Did not have time to collect data on this phase
new_moon <- aggregated_table %>%
  filter(phase == 'New Moon') %>% 
  group_by(week) %>%
  summarise(insomnia_count= sum(insomnia_count), insomnia_avg= (insomnia_count)/7,
            cantsleep_count=sum(cantsleep_count), cantsleep_avg=(cantsleep_count)/7,
            wideawake_count=sum(wideawake_count), wideawake_avg=(wideawake_count)/7,
            nosleep_count=sum(nosleep_count), nosleep_avg=(nosleep_count)/7,
            late_night_count=sum(late_night_count), late_night_avg=(late_night_count)/7)

#insomnia_avg
t.test(full_moon$insomnia_avg,first_quarter$insomnia_avg,alternative="two.sided") 
t.test(full_moon$insomnia_avg,new_moon$insomnia_avg,alternative="two.sided") 
t.test(full_moon$insomnia_avg,last_quarter$insomnia_avg,alternative="two.sided") 

#cantsleep_avg
t.test(full_moon$cantsleep_avg,first_quarter$cantsleep_avg,alternative="two.sided") 
t.test(full_moon$cantsleep_avg, new_moon$cantsleep_avg,alternative="two.sided")
t.test(full_moon$cantsleep_avg, last_quarter$cantsleep_avg,alternative="two.sided") 

#wideawake_avg
t.test(full_moon$wideawake_avg, first_quarter$wideawake_avg,alternative="two.sided") 
t.test(full_moon$wideawake_avg, new_moon$wideawake_avg,alternative="two.sided")
t.test(full_moon$wideawake_avg, last_quarter$wideawake_avg,alternative="two.sided") 

#nosleep_avg
t.test(full_moon$nosleep_avg, first_quarter$nosleep_avg,alternative="two.sided") 
t.test(full_moon$nosleep_avg, new_moon$nosleep_avg,alternative="two.sided") 
t.test(full_moon$nosleep_avg, last_quarter$nosleep_avg,alternative="two.sided") 

#late_night_avg
t.test(full_moon$late_night_avg, first_quarter$late_night_avg,alternative="two.sided") 
t.test(full_moon$late_night_avg, new_moon$late_night_avg,alternative="two.sided")
t.test(full_moon$late_night_avg, last_quarter$late_night_avg,alternative="two.sided") 

visual_df<-aggregated_table %>%
  group_by(phase) %>% 
  summarise(insomnia_count= sum(insomnia_count), insomnia_avg= mean(insomnia_count),
            cantsleep_count=sum(cantsleep_count), cantsleep_avg=mean(cantsleep_count),
            wideawake_count=sum(wideawake_count), wideawake_avg=mean(wideawake_count),
            nosleep_count=sum(nosleep_count), nosleep_avg=mean(nosleep_count),
            late_night_count=sum(late_night_count), late_night_avg=mean(late_night_count))

#plots
visual_df%>%ggplot(aes(x=phase,y=insomnia_avg))+geom_bar(stat="identity")+ggtitle("Average Number of Tweets per Night with #insomnia")+xlab("Phase")+ylab("Average Number of Tweets")
visual_df%>%ggplot(aes(x=phase,y=cantsleep_avg))+geom_bar(stat="identity")+ggtitle("Average Number of Tweets per Night with #cantsleep")+xlab("Phase")+ylab("Average Number of Tweets")
visual_df%>%ggplot(aes(x=phase,y=wideawake_avg))+geom_bar(stat="identity")+ggtitle("Average Number of Tweets per Night with #wideawake")+xlab("Phase")+ylab("Average Number of Tweets")
visual_df%>%ggplot(aes(x=phase,y=nosleep_avg))+geom_bar(stat="identity")+ggtitle("Average Number of Tweets per Night with #nosleep")+xlab("Phase")+ylab("Average Number of Tweets")
visual_df%>%ggplot(aes(x=phase,y=late_night_avg))+geom_bar(stat="identity")+ggtitle("Average Number of Tweets per Night at 3 am")+xlab("Phase")+ylab("Average Number of Tweets")


