# load packages in r 
library(tm)
library(wordcloud)
install.packages("syuzhet")
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

#import data 

reviews<-read.csv("C:/Users/indupravs/Downloads/Reviewroomba.csv",header=TRUE)
str(reviews)
#creating corpus
corpus<-iconv(reviews$text)
corpus<-Corpus(VectorSource(corpus))
#to see the corpus
inspect(corpus[1:5])
#Data cleaning and preparation
#change to lower case 
corpus<-tm_map(corpus,tolower)
inspect(corpus[1:5])
#remove punctuation marks
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,removeWords,stopwords("english"))
#remove some common words not used in analysis
corpus<-tm_map(corpus,removeWords,c("irobot","roomba","one","get","even","now","two",
                                    "vacuum","just","back","house","will",
                                    "'s","vaccum","let","said","vacuuming","gets",
                                    "robot","got","left","make","etc","say","around",
                                    "thought","ive","'S","also","every"))
corpus<-tm_map(corpus,stripWhitespace)

# make wasn't=was not, can't=can not, etc..
corpus<- gsub("wasn[\u2019']t", "was not", corpus)
corpus<- gsub("wasn[\u2019']t", "will not", corpus)
corpus<- gsub("wasn[\u2019']t", "can not", corpus)
corpus<- gsub("wasn[\u2019']t", "did not", corpus)
corpus<- gsub("wasn[\u2019']t", "do not", corpus)
corpus<- gsub("wasn[\u2019']t", "I am", corpus)
corpus<- gsub("wasn[\u2019']t", " have", corpus)
corpus<- gsub("wasn[\u2019']t",  "", corpus)
corpus<- gsub("wasn[\u2019']t", " are", corpus)
corpus<- gsub("wasn[\u2019']t", " will", corpus)



inspect(corpus[1:5])



reviews_final<-corpus
Fix Negations
# Create a list to identify the sentiment shifters in the text
negation.words <- c("not",
                    "no",
                    "without",
                    "never",
                    "bad",
                    "none",
                    "never",
                    "nobody",
                    "nowhere",
                    "neither",
                    "nothing"
)
#create a term document
install.packages("TermDocumentMatrix")

tdm<-TermDocumentMatrix(reviews_final)
tdm<-as.matrix(tdm)
tdm[1:10,1:5]

#bar plot of words
w<-rowSums(tdm)
w<-subset(w,w>=108)
barplot(w,las=2,col="lightgreen")

#creating a wod cloud
w<-sort(rowSums(tdm),decreasing=T)
set.seed(4444)
wordcloud(words=names(w),
          freq=w,
          max.words=40,
          random.order=T,
          min.freq=5,
          colors=brewer.pal(25,"Dark2"),
          scale=c(5,0.3))

#obtain sentiment scores
sentiment_data<-iconv(reviews$text)
s<-get_nrc_sentiment(sentiment_data)
s[1:10,]
          
#calculate reviewwise score
s$score<-s$positive-s$negative
s[1:10,]

#write scores in to a new csv file 

write.csv(x=s,file="C:/Users/indupravs/Downloads/DataScienceProjects/SentimentAnalysis/Final_score1.csv")

#check prodcut sentiment
dim(s)
#check overallsentiment of the prodcut
review_score<-colSums(s[,])
print(review_score)

barplot(colSums(s),
        las=2,
        col=rainbow(10),
        ylab='count',
        main='Sentiment')
        

