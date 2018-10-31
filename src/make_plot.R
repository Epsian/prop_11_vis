# to make a plot out of our tweets

#### Setup ####
library(plotly)
library(tm)
library(sentimentr)
library(syuzhet)

#### In/Out ####
# tagged tweets
tweets = read.csv("data/classified.csv", header = TRUE, stringsAsFactors = FALSE)

# Prop Text
prop_text = readRDS("data/prop_clean.rda")

#### TM Prep ####
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
tweets = merge(tweets, sim_per[, c("x", "similarity")], all.x = TRUE, by.x = "doc_id", by.y = "x")
rm(sim_per)

#### Other ####
emotion = get_nrc_sentiment(tweets$text)
tweets = cbind(tweets, emotion)
rm(emotion)

#### Plot ####
tweets$tag = factor(tweets$tag, levels = c("Yes", "Neutral", "No"))

p = plot_ly()
p = add_trace(p, x = jitter(as.numeric(tweets$tag), 1), y = tweets$similarity, mode = "markers", type = "scatter", marker = list(size = tweets$favorite_count + 5), text = tweets$text, color = tweets$tag)
p = layout(p, title = "Percent Similarity with Official YES Statement by Position",
           yaxis = list(title = "% Cosine Similarity with 'YES' Statement"),
           xaxis = list(showline = FALSE, showticklabels = FALSE))

saveRDS(p, "vis/plot.rda")

