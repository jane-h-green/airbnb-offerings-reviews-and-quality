####################################################################
#           AirBnB Reviews - Estimating AirBnB Offerings' Quality  #
#            Using Sentiment Analysis, Text Mining,                #
#              And Libraries for Word Cloud Visualizations         #
####################################################################

# Author: Jane Nikolova
# Occupation: Senior Consultant
# All Rights Reserved.
# Date: December, 2018

# Models & Data Mining Work completed as part a Data Science Course (part of Harvard Data Science graduate certificate program);
# Course: Data Science for Business
# University: Harvard Extension School - Harvard University

# Summary of methods & content:
# @ NLP libraries - Text Mining Package (tm); Quickly visualize the keywords as a word cloud (wordcloud)
# @ Applying sentiment analysis to customer reviews to define positive versus negative reviews and associated word frequencies.

library(tm)
library(qdap)
library(wordcloud) # package for wordclouds
library(RColorBrewer)
library(tidytext) # for those not able to load qdap
library(dplyr) # for those not able to load qdap
library(radarchart) # for those not able to load qdap
library(tidyr)


# Set your local work directory
setwd("")

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Function cleaning the corpus of data -
cleanCorpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  print('removed urls')
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) #new: isn't to is not
  print('replace contractions')
  corpus <- tm_map(corpus, content_transformer(replace_symbol)) #new: @ to "at"
  print('replace symbol')
  corpus <- tm_map(corpus, removePunctuation)
  print('remove punctuation')
  corpus <- tm_map(corpus, stripWhitespace)
  print('stip whitespace')
  corpus <- tm_map(corpus, removeNumbers)
  print('remove numbers')
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  print('making lowercase')
  corpus <- tm_map(corpus, removeWords, customStopwords)
  print('DONE!')
  return(corpus)
}

# Loading the data -
text <- read.csv("sampledAirBnB_Boston.csv")

# Sample -  1000:
set.seed(1234)
idx <- sample (1:nrow(text),1000)
text <- text[idx, ]

# Stopwords, add "apartment" and "boston" to the list
customStopwords <- c(stopwords('english'), 'place', 'apartment', 'boston')

# process:
txtCorpus <- VCorpus(VectorSource(text$comments))
txtCorpus<-cleanCorpus(txtCorpus)
txtDTM <- TermDocumentMatrix(txtCorpus)
#txtDTM

# Convert TDM to a simple matrix; takes a moment
txtDTMm <- as.matrix(txtDTM)
# Get column Sums and sort decreasing =TRUE
txtDTMv <- sort(rowSums(txtDTMm),decreasing=TRUE)
# Organize the row sums
txtDF <- data.frame(word = names(txtDTMv),freq=txtDTMv)

# Choose the "Purples" color & drop light ones in the color palette
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Simple word cloud of all terms with 75 words with your palette object
set.seed(1234)
wordcloud(txtDF$word,txtDF$freq, max.words=75, random.order=FALSE, colors=pal)

# The reviews contain mostly positive terms such as great, stay, host, clean, comfortable, etc.

# Split corpus based on polarity - sentiment analysis - positive vs. negative words:
polarityScores <- polarity(text$comments)

# Make it Tidy
tidyCorp <- tidy(txtDTM)
# Get a sentiment lexicon that has positive & negative
bing     <- get_sentiments(lexicon = c("afinn"))# "afinn" for affinity.
bingSent <- inner_join(tidyCorp, bing, by=c('term'='word'))
table(bingSent)

# NRC lexicon to constuct a radatChart
nrc <- get_sentiments(lexicon = c("nrc"))

# Perform Inner Join
nrcSent <- inner_join(tidyCorp, nrc, by=c('term'='word'))
# Quick Analysis
table(nrcSent$sentiment)
emos <- data.frame(table(nrcSent$sentiment))
emos <- emos[-c(6,7),] #drop the higher level positive/negative to get explicit emotion
# Review
emos

# Make a radar plot
chartJSRadar(scores=emos)

# Joyful words - 2121

# Anger words - 164

#         anger  164
#  anticipation 1676
#       disgust  139
#          fear  247
#           joy 2121
#       sadness  480
#      surprise  711
#        trust  2537

positive.negative.ratio <- (2121 + 2537 + 1676) / (480 + 247 + 164)
positive.negative.ratio

# The ratio of positive to negative reviews is about 7 to 1. This means that 1 out of 7 comments is bad, and indicates that most of the AirBnB offerings in the area are of GOOD QUALITY.




