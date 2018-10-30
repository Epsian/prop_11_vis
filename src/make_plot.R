# to make a plot out of our tweets

#### Setup ####
library(plotly)
library(sentimentr)
library(tm)
library(syuzhet)

#### In/Out ####
# Read Teets
tweets = list.files(path = "data/tweets/", full.names = TRUE)
tweets = lapply(tweets, readRDS)
tweets = do.call(rbind, tweets)

# Prop Text
prop_text = readRDS("data/prop_clean.rda")

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
  neut = ifelse(any(yes == FALSE && no == FALSE), TRUE, FALSE)
  
  out = data.frame(yes,no,neut, stringsAsFactors = FALSE)
  
  return(out)
  
})

tags = do.call(rbind, tags)
tweets = cbind(tweets, tags)

# Parse text
tweets$yes = ifelse(grepl("yes", tweets$text), TRUE, tweets$yes)
tweets$yes = ifelse(grepl("'yes'", tweets$text), TRUE, tweets$yes)
tweets$no = ifelse(grepl("no", tweets$text), TRUE, tweets$no)

colSums(tweets[, c("yes", "no", "neut")])

#### TM Prep ####

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

prop_dtm = DocumentTermMatrix(VCorpus(VectorSource(prop_text)), control = list(
  "weighting" = "weightTfIdf",
  "tolower" = TRUE,
  "removePunctuation" = TRUE,
  "stopwords" = TRUE
))

sim_per = RNewsflow::documents.compare(tweets_dtm, prop_dtm, measure = "percentage.to", return.zeros = FALSE)

#### Other ####
emotion = get_nrc_sentiment(tweets$text)
tweets = cbind(tweets, emotion)

merge = merge(sim_per, tweets[, c("doc_id", "text", "screen_name", "favorite_count", "retweet_count", "hashtags", "yes", "no", "neut", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")], all.x = TRUE, by.x = "x", by.y = "doc_id")

merge$tag = "Neutral"
merge$tag = ifelse(merge$yes == TRUE, "Yes", merge$tag)
merge$tag = ifelse(merge$no == TRUE, "No", merge$tag)
merge$tag = ifelse(merge$yes == TRUE & merge$no == TRUE, "Neutral", merge$tag)

merge$tag = factor(merge$tag, levels = c("Yes", "Neutral", "No"))

#### Plot ####
p = plot_ly()
p = add_trace(p, x = jitter(as.numeric(merge$tag), 1), y = merge$similarity, mode = "markers", type = "scatter", marker = list(size = merge$favorite_count + 5), text = merge$text, color = merge$tag)
p = layout(p, title = "Percent Similarity with Official YES Statement by Position", yaxis = list(title = "% Cosine Similarity with 'YES' Statement"))


#### Scrapts ####



colSums(get_nrc_sentiment(tweets$text[tweets$yes == TRUE]))/nrow(tweets[tweets$yes == TRUE,])
colSums(get_nrc_sentiment(tweets$text[tweets$no == TRUE]))/nrow(tweets[tweets$no == TRUE,])
colSums(get_nrc_sentiment(tweets$text[tweets$neut == TRUE]))/nrow(tweets[tweets$neut == TRUE,])



