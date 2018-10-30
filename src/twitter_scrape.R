# To scrape twitter for the prop 11 hashtags, then save them in csv

prop11_tweets = function(){

#### Setup ####
library(rtweet)
source("data/twitter_cred.R")

# setwd()

token = create_token(app = "EpsianTweepyAPI", consumer_key = .var_consumer_key, consumer_secret = .var_consumer_secret, access_token = .var_access_token, access_secret = .var_access_secret)

#### Scrape ####

prop11 = search_tweets(q = "#prop11", n = 6000, type = "mixed", include_rts = FALSE)
yes11 = search_tweets(q = "#YESon11 OR #yesonprop11", n = 6000, type = "mixed", include_rts = FALSE)
no11 = search_tweets(q = "#NoOn11 OR #noonprop11", n = 6000, type = "mixed", include_rts = FALSE)


# Merge DF
tweets = rbind(no11, yes11, prop11)

#### Save ####
saveRDS(tweets, file = paste0("data/tweets/tweets_df_", strftime(Sys.time(), format = "%H_%M"), ".rda"))

}

