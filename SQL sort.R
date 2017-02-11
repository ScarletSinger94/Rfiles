# Install and load the package
library(sqldf)

# reading the csv file into R
# this creates a dataframe which acts as a Table for sqldf
ClintonTweets<- read.csv("/Users/Singingking/Documents/Starbucksfinal.csv")

#select specfic columns of interest
results=sqldf('SELECT id, Text FROM ClintonTweets')

#select (different) specfic columns of interest
results=sqldf('SELECT ScreenName, Text, Numchar FROM ClintonTweets')

#select (different) specfic columns of interest based on a condition
results=sqldf('SELECT UserId, TweetText, NumChar FROM ClintonTweets WHERE NumChar>140')

#select all columns based on a condition
results=sqldf('SELECT * FROM ClintonTweets WHERE NumChar>140')

#select all columns based on another condition
results=sqldf('SELECT * FROM ClintonTweets WHERE NumChar<>140')

#select all columns based on another condition
results=sqldf('SELECT * FROM ClintonTweets WHERE NumChar BETWEEN 130 AND 140')

#select all columns based on another condition
results=sqldf('SELECT * FROM ClintonTweets WHERE NumChar> 140 AND NumWords>20')

#select all columns based on another condition
results=sqldf('SELECT * FROM ClintonTweets WHERE NumChar> 140 OR NumWords>20')


#select all columns based on a condition using userid
results=sqldf('SELECT * FROM ClintonTweets WHERE userid="Asher_P_Fly"')

#select all columns based on a condition using userid
#but we dont remember the full name. May be just the first letter
results=sqldf('SELECT * FROM ClintonTweets WHERE userid LIKE "A%"')

#select all columns based on a condition using userid 
#but we dont remember the full name. May be just the last two characters
results=sqldf('SELECT * FROM ClintonTweets WHERE userid LIKE "%ly"')

#select all columns based on a condition using userid 
#but we dont remember the full name. May be just something in the middle
results=sqldf('SELECT * FROM sortweetDF WHERE urlist LIKE "https%"')


#obtain values based on a particular condition 
#MIN	     	returns the smallest value in a given column
results=sqldf('SELECT MIN("textlen") FROM ClintonTweets')

#MAX	returns the largest value in a given column
results=sqldf('SELECT MAX("NumChar") FROM ClintonTweets')

#SUM	returns the sum of the numeric values in a given column
results=sqldf('SELECT SUM("Positive") FROM mySentiment')

#AVG		returns the average value of a given column
results=sqldf('SELECT AVG("score") FROM men WHERE age = 45')

#COUNT	returns the total number of values in a given column
results=sqldf('SELECT COUNT("NumChar") FROM ClintonTweets')
results=sqldf('SELECT COUNT("Longitude") FROM ClintonTweets')

#COUNT(*)	returns the number of rows in a table
results=sqldf('SELECT COUNT("*") FROM ClintonTweets')

#Order the data based on a particular column- NumChar - default ascending
results=sqldf('SELECT * FROM ClintonTweets ORDER BY NumChar')

#Order the data based on a particular column- NumChar - descending
results=sqldf('SELECT * FROM ClintonTweets ORDER BY NumChar DESC')

#Order the data based on a particular column- NumChar followed by another column - NumWords
results=sqldf('SELECT * FROM ClintonTweets ORDER BY NumChar DESC, NumWords')

#Order the data based on a particular column- TimeStamp - descending
results=sqldf('SELECT * FROM ClintonTweets ORDER BY TimeStamp DESC')

#Group the results by values on a particular column - UserId
results=sqldf('SELECT * FROM ClintonTweets GROUP BY UserId')

#Group the results by values on a particular column -UserIs and the number of posts
results=sqldf('SELECT UserId, COUNT("*") FROM ClintonTweets GROUP BY UserId')

#Group the results by values on a particular column- UserId and the number of Characters
results=sqldf('SELECT UserId, AVG("NumChar") FROM ClintonTweets GROUP BY UserId')

#Select DISTINCT UserIds
results=sqldf('SELECT DISTINCT UserId FROM ClintonTweets')

#Create a separate table for Geo data
GeoData=sqldf('SELECT TweetId, Latitude, Longitude FROM ClintonTweets')

#Create a separate table for textual Data
TextData=sqldf('SELECT TweetId, TweetText, NumChar FROM ClintonTweets')

#Making a join between two tables - GeoData and Text Data
results=sqldf('SELECT * FROM GeoData NATURAL JOIN TextData')

#Create a separate table for "older" data
OldData=sqldf('SELECT * FROM ClintonTweets WHERE TimeStamp< "10/9/2016 19:47"')

#Create a separate table for "newer" Data
NewData=sqldf('SELECT * FROM ClintonTweets WHERE TimeStamp>= "10/9/2016 19:47"')

#Merge the two subsets of data. Note the column names must be exactly the same in the two datasets
CombinedData=rbind(OldData, NewData)

# reading the csv file into R
ClintonTweets<- read.csv("/Users/Singingking/Documents/ClintonTweets.csv")
#focusing only on the text of tweets for this exercise
cleanText<-ClintonTweets$TweetText
head(cleanText)

#Encoding(cleanText) <- "UTF-8"
text2=iconv(cleanText, "UTF-8", "UTF-8",sub='')
cleanText=text2

#making sure we can use functions from the stringr package
#install.packages("stringr")
library(stringr)

##CleanTweets() - Takes the junk out of a vector of
# tweet texts
CleanTweets<-function(tweets)
{
  
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

#let's get rid of the redundants spaces, urls, RTs, #s, screennames 
cleanerText<-CleanTweets(cleanText)

#check if the function has worked
head(cleanText)
head(cleanerText)

#now reassign the cleanText variable to follow the code from the book.
cleanText=cleanerText

#installing (and initiating) a number of packages needed to run the sentiment analysis code

library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)


#get sentiment score for the the tweets stored under cleanText
mySentiment <- get_nrc_sentiment(as.character(cleanText))

#creating a column for positive values obtained
ClintonTweets$PositiveVals=mySentiment$positive

#creating a column for negative values obtained
ClintonTweets$NegativeVals=mySentiment$negative

#creating a column for the net i.e. positive - negative values obtained for emotion
ClintonTweets$NetEmotion=mySentiment$positive - mySentiment$negative
