#read file
apple <-read.csv(file.choose(),header=T)

str(apple)

#build corpas
library(tm)
corpus <-iconv(apple$text,to="utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#clean text
corpus<-tm_map(corpus, tolower )
inspect(corpus [1:5])

corpus<-tm_map(corpus ,removePunctuation)
inspect(corpus[1.5])

corpus<- tm_map(corpus , removeNumbers)
inspect(corpus[1:5])

cleanset<- tm_map(corpus , removeWords, stopwords('english'))
inspect(cleanset[1.5])

removeURL <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords , c('aapl','apple'))
cleanset <-tm_map(cleanset, gsub, 
                  pattern ='stocks',
                  replacement='stock')

cleanset <-tm_map(cleanset, stripWhitespace)

#term document matrix
tdm<-TermDocumentMatrix(cleanset)
tdm

tdm <-as.matrix(tdm)
tdm [1:10,1:20]

#bar plot
w <-rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las =2,
        col =rainbow(50))


#word cloud
library(wordcloud)
w<-sort(rowSums(tdm), decreasing=TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq =w,
          max.words=150,
          random.order=F,
          min.freq=5,
          colors=brewer.pal(8,'Dark2'),
          scale =c(5,0.3),
          rot.per=0.7)

#interesting wordcloud options in wordcloud2
library(wordcloud2)
w<-data.frame(names(w),w)
colnames(w) <-c('word', 'freq')
wordcloud2(w,
           size=0.5,
           shape ='star',
           rotateRatio =0.5,
           minSize =1 )
letterCloud (w,
             word ="apple",
             size =1)



#sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

#read file
apple <- read.csv(file.choose(), header =T)
tweets <-iconv(apple$text, to= 'utf-8')

#obtain sentiment scores
# lexicon based algorithm - NRC lexicon
s<- get_nrc_sentiment(tweets)
head(s)
#bar plot
barplot(colSums(s),
        las=2,
        col=rainbow(10),
        ylab ='Count',
        main ='sentiment Scores for Apple Tweets')

#tweets[4]
#get_nrc_sentiment('delay')
t <- data.frame(
  positive = s$positive,
  negative = s$negative
)
head(t)
#s <- s[-c(9,10)]
#head(s)

barplot(colSums(t),
        las=2,
        col=rainbow(10),
        ylab ='Count',
        main ='sentiment Scores for Apple Tweets')
