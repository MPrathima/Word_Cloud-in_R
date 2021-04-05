getwd()
setwd("D:/pratz")
getwd()

# Loading required libraryies
library(qdap)

library(dplyr)
library(tm)
library(wordcloud)

library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)

# Reading data

review=read.csv("Book1.csv", stringsAsFactors = FALSE)

names(review)
head(review)
# Creating corpos from review column
##########################################
#STEP2???-???Text Pre-processing
##########################################

# Creating corpos from review column

text<-review$text
docs <- Corpus(VectorSource(text))

#Text transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "//")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("com", "http","www")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
##########################################
#STEP4  Create the DTM & TDM from the corpus

##########################################
#review_dtm <- DocumentTermMatrix(docs)
review_tdm <- TermDocumentMatrix(docs)
#dim(review_dtm)
dim(review_tdm)

#Using the TDM to identify frequent terms

# Convert TDM to matrix
review_m <- as.matrix(review_tdm)

dim(review_m)
write.csv(review_m,"data.csv")
# Sum rows and frequency data frame
words <- sort(rowSums(review_m),decreasing = TRUE)
words
##write.csv(words,"Q10(4).csv")
df <- data.frame(word = names(words),freq=words)
head(df)
set.seed(1234)

wordcloud(words = df$word, freq = df$freq,scale=c(3,0.2), min.freq = 1,max.words =40 , random.order = FALSE, rot.per = 0.4,colors=brewer.pal(8, "Dark2"))

##Plot word frequencies
barplot(df[1:10,]$freq, las = 2, names.arg = df[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
