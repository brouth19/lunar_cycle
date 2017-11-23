#----------------
# NOTES:
#
# crontab configuration
# * * * * * cd /FULLPATH/lunar_cycle/crontab_test/ && /usr/local/bin/Rscript daily_twitter_script.R
#
# 1. You'll need to make a crontab entry with this information.
# 2. You will need to edit the FULLPATH to be correct on your machine. It should start with `/Users/` 
# 3. This should write a new csv every minute in lunar_cycle/crontab_test/data/
# 4. If you have trouble check `which RScript` and replace the Rscript path if necessary.
#----------------

# get your data from twitter -------------
#   i'll just generate some random data to simulate this
n <- rpois(1,40)
twitter_df <- data.frame( id = 1:n, value = runif(n, 0, 1))

# write result to disk --------------------
system('mkdir -p data') # create directory if not exists
date_time <- gsub(':', '-', Sys.time()) # clean up the datetime
date_time <- gsub(' ', '_', date_time)
file_name <- sprintf('data/tweets_%s.csv', date_time) # write a new file each time
write.csv(twitter_df, file_name, row.names=F)
