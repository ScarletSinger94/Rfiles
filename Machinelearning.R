#myFamilyNames <-c("Dad","Mom","Sis","Bro","Dog")
#myFamilyAges <-c(43,42,12,8,5)
#myFamilyGenders<-c("Male","Female","Female","Male","Female")
#myFamilyWeights<-c(188,136,83,61,44)
#myFamily = data.frame(myFamilyNames,myFamilyAges, myFamilyGenders, myFamilyWeights)
#myFamily$myFamilySizes<-c("L","M","s","s","s")
#install the packages neeeded for text processing and machine learning 
#install.packages("RTextTools")
#install.packages("e1071")

#use the libraries
library(RTextTools)
library(e1071)

#create some positive samples to learn from
pos_tweets =  rbind(
  c('I love this car', 'positive'),
  c('This view is amazing', 'positive'),
  c('I feel great this morning', 'positive'),
  c('I am so excited about the concert', 'positive'),
  c('He is my best friend', 'positive')
)

#create some negative samples to learn from
neg_tweets = rbind(
  c('I do not like this car', 'negative'),
  c('This view is horrible', 'negative'),
  c('I feel tired this morning', 'negative'),
  c('I am not looking forward to the concert', 'negative'),
  c('He is my enemy', 'negative')
)

#create some samples - positive and negative - to test with
test_tweets = rbind(
  c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative')
)

#combine the various samples into one dataframe
tweets = rbind(pos_tweets, neg_tweets, test_tweets)

# build dtm - document term matrix
matrix= create_matrix(tweets[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE)

# train the model
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:10,], as.factor(tweets[1:10,2]) )


## test the validity of the model
#get the predicted values
predicted = predict(classifier, mat[11:15,])
predicted

#get a summary table of results
table(tweets[11:15, 2], predicted)

#get the accuracy score
recall_accuracy(tweets[11:15, 2], predicted)



##Other machine learning approaches?

# First, build the data to specify response variable, training set, testing set.
container = create_container(matrix, as.numeric(as.factor(tweets[,2])),
                             trainSize=1:10, testSize=11:15,virgin=FALSE)

#Second, to train the model with multiple machine learning algorithms:
models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

#Now, we can classify the testing set using the trained models.
results = classify_models(container, models)

# accuracy table
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])

# recall accuracy
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"SVM_LABEL"])

#To summarize the results (especially the validity) in a formal way:
all_results=create_precisionRecallSummary(container, results)
all_results

# reading the csv file into R
ClintonTweets<- read.csv("/Users/Singingking/Documents/ClintonTweets.csv")

#undertake correlation using R - NumChar vs NumWords
cor(ClintonTweets$NumChar, ClintonTweets$NumWords)
#plotting the data
plot(ClintonTweets$NumChar, ClintonTweets$NumWords)
#is the correlation significant
cor.test(ClintonTweets$NumChar, ClintonTweets$NumWords)

##Repeating the process with another column - NumChar vs EMOCount
cor(ClintonTweets$NumChar, ClintonTweets$EMOcount)
plot(ClintonTweets$NumChar, ClintonTweets$EMOcount)
cor.test(ClintonTweets$NumChar, ClintonTweets$EMOcount)

##Repeating the process with another column - NumChar vs URLCount
cor(ClintonTweets$NumChar, ClintonTweets$URLcount)
plot(ClintonTweets$NumChar, ClintonTweets$URLcount)
cor.test(ClintonTweets$NumChar, ClintonTweets$URLcount)

# Linear Regression Example 1
fit <- lm(NumChar ~ NumWords, data=ClintonTweets)
# show results
summary(fit)
#plot the results
plot(ClintonTweets$NumChar, ClintonTweets$NumWords)

# (Multiple) Linear Regression Example 2
fit2 <- lm(NumChar ~ NumWords + EMOcount + URLcount, data=ClintonTweets)
summary(fit2) # show results
#plot the results
plot(ClintonTweets$NumChar, fit$fitted.values)
install.packages("pROC")
library("pROC")
auc(as.numeric(as.factor(tweets[11:15, 2])), as.numeric(as.factor(results[,"FORESTS_LABEL"])))
