#Importing Basic Info Required from Twitter.
consumer_key <- "XXX"
consumer_secret <- "XXX"
access_token <- "XXX"
access_token_secret <- "XXX"

#Load libraries
library(twitteR)
library(RCurl)
library(tm)
library(wordcloud)
library(ggplot2)

#Setting up a connection to Twitter.
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)

#Trend Locations
trend <- availableTrendLocations()
head(trend)
trend

#Getting Trends
thess <- getTrends(963291)
thess
thess$name

#Getting Tweets . 
tweets <- searchTwitter("morning+coffee", n = 500 , lang = "en" , resultType = "recent")
class(tweets) 

#We want to create a character vector from the tweets list above. so we do the following.
tweets_text <- sapply(tweets, function(x) x$getText())
class(tweets_text)

#then we use tm package to convert the character tweets_text to a Corpus.
tweet_corpus <- Corpus(VectorSource(tweets_text))
tweet_corpus

#Now we want to clean this up a bit. Remove any unwanted things.
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
tweet_corpus_clean <- tm_map(tweet_corpus, content_transformer(removeNumPunct))
tweet_corpus_clean <- tm_map(tweet_corpus_clean, content_transformer(tolower))
tweet_corpus_clean <- tm_map(tweet_corpus_clean, removeWords, stopwords("en"))
tweet_corpus_clean <- tm_map(tweet_corpus_clean, stripWhitespace)
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
tweet_corpus_clean <- tm_map(tweet_corpus_clean, content_transformer(removeURL)) 
tweet_corpus_clean <- tm_map(tweet_corpus_clean, removeNumbers) 


#WordClouds
wordcloud(tweet_corpus_clean) 
wordcloud(tweet_corpus_clean, random.order = F, max.words = 500, scale = c(3,0.5), colors = rainbow(50))

#Building a Term Document Matrix
tdm <- TermDocumentMatrix(tweet_corpus_clean)
m <- as.matrix(tdm) 
v <- sort(rowSums(m),decreasing=TRUE) #Sorting the matrix to decreasing order 
d <- data.frame(word = names(v),freq=v) #building a dataframe with words and frequencies
head(d, 20) #inspecting the head of the data frame above.
tail(d, 20) #less common words.

#PLOTS
#1. Barplot of frequent words.
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


#Explore frequent terms and their associations
#1. words that occur at least four times
findFreqTerms(tdm, lowfreq = 4)
#2. Association between frequent terms.  Words that are associated with "coffee" and "morning"
findAssocs(tdm, terms = "coffee", corlimit = 0.2)
findAssocs(tdm, terms = "morning", corlimit = 0.2)

#Clustering by term similarity. First we remove a lot of the uninteresting or infrequent words.
dtmss <- removeSparseTerms(tdm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss)   

#1. Hierarchical clustering.
#1.1 First calculate distance between words & then cluster them according to similarity.
library(cluster)   
s <- dist(t(dtmss), method="euclidian")   
#1.2 Create the model
fit <- hclust(d=s, method="ward.D2")   
fit
#1.3 Plot the Model.
plot(fit, hang=-1)  #This produces a cluster dendrogram that is hard to read.
#1.4 Asking R to identify the clusters and Create the borders.
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   




