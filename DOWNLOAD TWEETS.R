library("twitteR")
library("ROAuth")
library("modeest")
library("stringr")
library("dismo")
library("ggplot2")
library("maps")
library("RCurl")
library("RJSONIO")
library("datasets")
library("lattice")
library("dplyr")
library("sqldf")
library("syuzhet")
library("lubridate")
library("scales")
library("reshape2")
library("tm")
library("wordcloud")
library("RTextTools")
library("e1071")
library("httr")
CUSTOMER_KEY="3tP5vK16rM6EjPSir5NmUAqm6"
CUSTOMER_SECRET="EJRM4RI6d65SVcepAqoQuXpMLll4LqRBLoqb0vogCxkkHpPTRO"
ACCESS_TOKEN="836601552-pnIKKwbhvN6ZGre4F7RcSF1NeqolnEsu2tP7bALC"
ACCESS_secret="Jx21J3rJrn0NKgUgScLcDsEBKZZUyteSpXbhAaUdDy2UY"
setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret)
TweetFrame<-function(searchTerm, maxTweets){
  twtList<-searchTwitter(searchTerm,n=maxTweets, lang = "en", geocode='39.8333333,-98.585522,1500mi')
  return(do.call("rbind",lapply(twtList,as.data.frame))) }
lgData <- TweetFrame("#depressed" , 500)
attach(lgData)
sortweetDF<-lgData[order(as.integer(created)), ]
detach(lgData)
attach(sortweetDF)
diff(created) 
#hist(as.integer(diff(created)))
mfv(as.integer(diff(created)))
median(as.integer(diff(sortweetDF$created)))
detach(sortweetDF)
attach(sortweetDF)
text

#text2=iconv(text, "UTF-8", "UTF-8",sub='') 
#sortweetDF$textlen3 <- str_length(text2)

str_length(text)
sortweetDF$textlen <- str_length(text)
detach(sortweetDF)
attach(sortweetDF)
sortweetDF[textlen>140, "text"]
sortweetDF$modtext <- str_replace_all(text,"https://t.co/[a-z,A-Z,0-9]{12}"," ")
sortweetDF$modtext <- str_replace_all(text,"�","")
sortweetDF$textlen2 <- str_length(sortweetDF$modtext)
detach(sortweetDF)
attach(sortweetDF)
sortweetDF[textlen != textlen2,"text"]
sortweetDF$wordCount<-(str_count(modtext," ") + 1)
detach(sortweetDF)
attach(sortweetDF)
mean(wordCount)

sortweetDF$rt <- str_match(modtext,"RT @[a-z,A-Z]*: ")
detach(sortweetDF)
attach(sortweetDF)
head(rt, 10)
sortweetDF$rt <- sortweetDF$rt[ ,1]
detach(sortweetDF)
attach(sortweetDF)
table(as.factor(rt))

sortweetDF$longtext <- (textlen>140)
detach(sortweetDF)
attach(sortweetDF)
table(as.factor(rt),as.factor(longtext))

sortweetDF$hasrt <- !(is.na(rt))
detach(sortweetDF)
attach(sortweetDF)
View(sortweetDF)

table(hasrt,longtext)
str_match_all(text,"https://t.co/[a-z,A-Z,0-9]{8}")
sortweetDF$urlist<-str_match_all(text,"https://t.co/[a-z,A-Z,0-9]{8}")
detach(sortweetDF)
attach(sortweetDF)
head(sortweetDF)
length(urlist[[1]])
length(urlist[[5]])

sortweetDF$numurls<-rapply(urlist,length) 
detach(sortweetDF)
attach(sortweetDF)
table(numurls,longtext)
prop.table(table(numurls,longtext))
sum(is.na(longtext))

pos = gregexpr(':)', text)
pos = gregexpr(':-)', text)
pos=gregexpr("((?::|;|=)(?:-)?(?:\\)|D|P))",text)
sortweetDF$numSmileys<-rapply(pos,length)
table(numurls,hasrt,longtext)
library("maps") # tell R that we will use maps package
map('usa') # create a map of whole USA
with(filter(sortweetDF), points(longitude, latitude))#plot the abnormal pm25 value locations
cleanText<-sortweetDF$TweetText
head(cleanText)

#Encoding(cleanText) <- "UTF-8"
text2=iconv(cleanText, "UTF-8", "UTF-8",sub='')
cleanText=text2

cleanText<-sortweetDF$text
CleanTweets<-function(tweets)
{
  # Remove redundant spaces
  tweets <- str_replace_all(tweets,"  "," ")
  # Get rid of URLs
  tweets <- str_replace_all(tweets,"http://t.co/[a-z,A-Z,0-9]{10}","")
  # Take out retweet header, there is only one
  tweets <- str_replace(tweets,"RT @[a-z,A-Z]*: ","")
  # Get rid of hashtags
  tweets <- str_replace_all(tweets,"#[a-z,A-Z]*","")
  # Get rid of references to other screennames
  tweets <- str_replace_all(tweets,"@[a-z,A-Z]*","")
  # Get rid of URLs
  tweets <- str_replace_all(tweets,"https://t.co/[a-z,A-Z,0-9]{10}","")
  # Take out retweet header, there is only one
  tweets <- str_replace(tweets,"RT @[a-z,A-Z]*: ","")
  # Get rid of hashtags
  tweets <- str_replace_all(tweets,"#","")
  # Get rid of references to other screennames
  tweets <- str_replace_all(tweets,"@[a-z,A-Z]*","")
  # Remove redundant spaces
  tweets <- str_replace_all(tweets,"  "," ")
  return(tweets)
}
cleanerText<-CleanTweets(cleanText)
cleanText=cleanerText
tweetCorpus<-Corpus(VectorSource(cleanText))
#check what's inside the newly defined variable
#tweetCorpus<-tm_map(tweetCorpus, tolower) #gives an error with TermDocumentMatrix function
tweetCorpus<-tm_map(tweetCorpus, removePunctuation)
tweetCorpus<-tm_map(tweetCorpus, removeWords, stopwords('english'))
tweetTDM<-TermDocumentMatrix(tweetCorpus)
tweetTDM
inspect(tweetTDM)
#create a matrix out of the term document matrix
tdMatrix <- as.matrix(tweetTDM)
#get the total number of occurences for each term and then sort the matrix
sortedMatrix<-sort(rowSums(tdMatrix), decreasing=TRUE)
#building a new data frame called cloudfram that organizes data in the format needed by the worddcloud 
cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)


#specify the canvas parameter for the word cloud. 1 columns by 1 rows.
par(mfrow = c(1, 1))
#show the wordcloud
wordcloud(cloudFrame$word,cloudFrame$freq)
#to colorize- pick a color pallete
pal <- colorRampPalette(c(rainbow(3)))
#regenerate the wordcloud
wordcloud(cloudFrame$word,cloudFrame$freq,c(6,.3),2,max.words = 75,FALSE,TRUE,.15,pal(10))


#get sentiment score for the the tweets stored under cleanText
mySentiment <- get_nrc_sentiment(as.character(cleanText))

#creating a column for positive values obtained
sortweetDF$PositiveVals=mySentiment$positive

#creating a column for negative values obtained
sortweetDF$NegativeVals=mySentiment$negative

#creating a column for the net i.e. positive - negative values obtained for emotion
sortweetDF$NetEmotion=mySentiment$positive - mySentiment$negative

#table($statename) %>% barplot(col = "wheat")
d <- density(sortweetDF$textlen2)
plot(d, main="Density of Text Length Starbucks")
polygon(d, col="red", border="blue")

