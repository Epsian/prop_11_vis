# To clean and classify tweets into for and against

#### Setup ####
library(caret)
library(tm)

#### in/out ####
# Read Teets
tweets = list.files(path = "data/tweets/", full.names = TRUE)
tweets = lapply(tweets, readRDS)
tweets = do.call(rbind, tweets)

#### Data Clean ####
# Clean Tweets
tweets = unique(tweets) # remove duplicates
tweets = tweets[!grepl("Florida", tweets$text, ignore.case = TRUE),] # Clear tweets talking about florida
tweets = tweets[!grepl("amendment", tweets$text, ignore.case = TRUE),]
tweets = tweets[!grepl("jacksonville", tweets$text, ignore.case = TRUE),]
tweets$text = iconv(tweets$text, from = "UTF-8", to = "ASCII", sub = "") # clear out unicode
tweets$text = tolower(tweets$text)

# remove urls
tweets$text = stringr::str_remove_all(tweets$text, "((http[s]?|ftp):\\/)?\\/?([^:\\/\\s]+)((\\/\\w+)*\\/)([\\w\\-\\.]+[^#?\\s]+)(.*)?(#[\\w\\-]+)?")

# parse hashtags
tags = apply(tweets, 1, FUN = function(x){
  tags = unlist(x$hashtags)
  tags = tolower(tags)
  
  yes = ifelse(any(c("yeson11", "yesonprop11") %in% tags), TRUE, FALSE)
  no = ifelse(any(c("noon11", "noonprop11") %in% tags), TRUE, FALSE)
  neut = ifelse(any(yes == TRUE & no == TRUE), TRUE, FALSE)
  
  out = data.frame(yes,no,neut, stringsAsFactors = FALSE)
  
  return(out)
  
})

tags = do.call(rbind, tags)
tweets = cbind(tweets, tags)
rm(tags)

# Parse text
tweets$yes = ifelse(grepl("yes", tweets$text), TRUE, tweets$yes)
tweets$yes = ifelse(grepl("'yes'", tweets$text), TRUE, tweets$yes)
tweets$no = ifelse(grepl("no", tweets$text), TRUE, tweets$no)

tweets$tag = "Neutral"
tweets$tag = ifelse(tweets$yes == TRUE, "Yes", tweets$tag)
tweets$tag = ifelse(tweets$no == TRUE, "No", tweets$tag)
tweets$tag = ifelse(tweets$yes == TRUE & tweets$no == TRUE, "Neutral", tweets$tag)

tweets$tag = factor(tweets$tag, levels = c("Yes", "Neutral", "No"))

#human = tweets[, c("status_id", "text", "yes", "no", "neut", "tag")]
#write.csv(human, "data/human_validate.csv", row.names = FALSE)
human = read.csv("data/human_validate.csv", header = TRUE, stringsAsFactors = FALSE)

tweets$tag = human$tag


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

# add tags to dtm
tweets_dtm$tag.class = tweets$tag

#### Classify ####
# Make training set
partition_indexes <- createDataPartition(tweets_dtm$tag.class, times = 1, p = 0.7, list = FALSE)
tweets.train <- tweets_dtm[partition_indexes, ]
tweets.test <- tweets_dtm[-partition_indexes, ]

# Train
tweets.caret <- train(tag.class ~ ., data = tweets.train, method = "nnet", trace = FALSE, MaxNWts = 5000)
predictions <- predict(tweets.caret, tweets.test)
table(predictions, tweets.test$tag.class)






