# To tag the support for prop 11 by tweet text

#### Setup ####
library(caret)
library(tm)

#### in/out ####
# read classifier
twitter.model = readRDS("data/tweet_train.rda")

# Read Teets
tweets = list.files(path = "data/tweets/", full.names = TRUE)
tweets = lapply(tweets, readRDS)
tweets = do.call(rbind, tweets)

# Clean Tweets
tweets = tweets[!duplicated(tweets$text),] # remove duplicates
tweets = tweets[!grepl("Florida", tweets$text, ignore.case = TRUE),] # Clear tweets talking about florida
tweets = tweets[!grepl("amendment", tweets$text, ignore.case = TRUE),]
tweets = tweets[!grepl("jacksonville", tweets$text, ignore.case = TRUE),]
tweets$text = iconv(tweets$text, from = "UTF-8", to = "ASCII", sub = "") # clear out unicode
tweets$text = tolower(tweets$text)

# remove urls
tweets$text = stringr::str_remove_all(tweets$text, "((http[s]?|ftp):\\/)?\\/?([^:\\/\\s]+)((\\/\\w+)*\\/)([\\w\\-\\.]+[^#?\\s]+)(.*)?(#[\\w\\-]+)?")

#### DTM ####
# Prepare for TM
tweets2 = data.frame("doc_id" = 1:nrow(tweets), "text" = tweets$text, stringsAsFactors = FALSE)
tweets = cbind(tweets2, tweets[, -which(colnames(tweets) %in% c("text"))])
rm(tweets2)

# Make Corpus
tweet_cor = VCorpus(DataframeSource(tweets))

# Make DTM
tweets_dtm = DocumentTermMatrix(tweet_cor, control = list(
  "weighting" = "weightTfIdf",
  "tolower" = TRUE,
  "removePunctuation" = TRUE,
  "stopwords" = TRUE
))

tweets_dtm = as.data.frame(as.matrix(tweets_dtm), stringsAsFactors = FALSE)

#### classify tweets ####
tweets$tag <- predict(twitter.model, tweets_dtm)

#### save ####
tweets$doc_id = 1:nrow(tweets)
output = tweets[,c("doc_id", "text", "tag", "favorite_count", "retweet_count")]

write.csv(output, "data/classified.csv", row.names = FALSE)



