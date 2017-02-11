#install.packages("car")
library("car")
library("sqldf")
library("scatterplot3d")
library("corrplot")
library("PerformanceAnalytics")
library("plotly")
library("psych")
mach <- read.csv("~/Documents/mach.csv", header=TRUE)
men=sqldf('SELECT * FROM mach WHERE gender LIKE "1"')
women=sqldf('SELECT * FROM mach WHERE gender LIKE "2"')
men=sqldf('SELECT * FROM men WHERE age<= "65"')
women=sqldf('SELECT * FROM women WHERE age<= "65"')
men=sqldf('SELECT * FROM men WHERE age>= "15"')
women=sqldf('SELECT * FROM women WHERE age>= "15"')
men=sqldf('SELECT * FROM men WHERE seconds_elapsed <= "600"')
women=sqldf('SELECT * FROM women WHERE seconds_elapsed <= "600"')
CombinedData=rbind(men, women)
#men=sqldf('SELECT * FROM men WHERE score >= "60"')
#women=sqldf('SELECT * FROM women WHERE score >= "60"')

t.test(men$age, men$score)
t.test(women$age, women$score)
t.test(men$age, men$seconds_elapsed)
t.test(women$age, women$seconds_elapsed)
t.test(men$score, men$seconds_elapsed)
t.test(women$score, women$seconds_elapsed)
t.test(CombinedData$age, CombinedData$score)
t.test(CombinedData$age, CombinedData$seconds_elapsed)
t.test(CombinedData$score, CombinedData$seconds_elapsed)
t.test(men$score,women$score)
t.test(men$seconds_elapsed,women$seconds_elapsed)
t.test(men$age,women$age)
#plot(men$score,men$seconds_elapsed)
#scatterplotMatrix(men[1:5])
sapply(men[1:20],mean)
sapply(women[1:20],mean)
sapply(men[1:20],sd)
sapply(women[1:20],sd)
sapply(men[21:24],mean)
sapply(women[21:24],mean)
sapply(men[21:24],sd)
sapply(women[21:24],sd)
sapply(CombinedData[21:24],mean)
sapply(CombinedData[21:24],sd)
 printMeanAndSdByGroup <- function(variables,groupvariable)
{
  # find the names of the variables
  variablenames <- c(names(groupvariable),names(as.data.frame(variables)))
  # within each group, find the mean of each variable
  groupvariable <- groupvariable[,1] # ensures groupvariable is not a list
  means <- aggregate(as.matrix(variables) ~ groupvariable, FUN = mean)
  names(means) <- variablenames
  print(paste("Means:"))
  print(means)
  #within each group, find the standard deviation of each variable:
  sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
  names(sds) <- variablenames
  print(paste("Standard deviations:"))
  print(sds)
   #within each group, find the number of samples:
  samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
 names(samplesizes) <- variablenames
  print(paste("Sample sizes:"))
  print(samplesizes)
}
printMeanAndSdByGroup(men[1:20],men[21])
printMeanAndSdByGroup(women[1:20],women[21])
printMeanAndSdByGroup(men[1:20],men[23])
printMeanAndSdByGroup(women[1:20],women[23])
menavg<-(printMeanAndSdByGroup(men[21:24],men[23]))
womenavg<-(printMeanAndSdByGroup(women[21:24],women[23]))
printMeanAndSdByGroup(CombinedData[1:20],CombinedData[21])
 mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
 }
 mosthighlycorrelated(men[1:20], 50)
 mosthighlycorrelated(women[1:20], 50)
 mosthighlycorrelated(men[21:24], 2)
 mosthighlycorrelated(women[21:24], 2) 
 mosthighlycorrelated(men[1:24], 10)
 mosthighlycorrelated(women[1:24], 10) 
library("hexbin")
bin<-hexbin(men$age,men$score)
plot(bin, main="Age and Score Men")
bin2<-hexbin(women$age,women$score)
plot(bin2,main="Age and Score Women")
bin3<-hexbin(men$age,men$seconds_elapsed)
plot(bin3,main="Age and Time Men")
bin4<-hexbin(women$age,women$seconds_elapsed)
plot(bin4,main="Age and Time Women")
bin5<-hexbin(men$score,men$seconds_elapsed)
plot(bin5,main="Score and Time Men")
bin6<-hexbin(women$score,women$seconds_elapsed)
plot(bin6,main="Score and Time women")

attach(men) 
s3d<-scatterplot3d(men$score,men$seconds_elapsed,men$age, pch=16, highlight.3d=TRUE,
              type="h", main="The Money Graph Men")
fit <- lm(men$age ~ men$score+men$seconds_elapsed) 
s3d$plane3d(fit)
summary(fit)
detach(men)

attach(women)
s3d1<-scatterplot3d(women$score,women$seconds_elapsed,women$age, pch=16, highlight.3d=TRUE,
                   type="h", main="The Money Graph Women")
detach(women)
fit1 <- lm(women$age ~ women$score+women$seconds_elapsed) 
s3d1$plane3d(fit1)
summary(fit1)

attach(CombinedData)
s3d2<-scatterplot3d(CombinedData$score,CombinedData$seconds_elapsed,CombinedData$age, pch=16, highlight.3d=TRUE,
                    type="h", main="The Money Graph All")
detach(CombinedData)
fit2 <- lm(CombinedData$age ~ CombinedData$score+CombinedData$seconds_elapsed) 
s3d2$plane3d(fit2)
summary(fit2)

x <- cor(men[1:20]) 
corrplot(x, type="upper", order="hclust", method = "square")
y <- cor(women[1:20]) 
corrplot(y, type="upper", order="hclust", method= "square")

corr.test(men[1:20])
corr.test(men[21:24])
corr.test(women[1:20])
corr.test(women[21:24])

cor.test(men$score,men$age)
cor.test(men$seconds_elapsed,men$age)
cor.test(men$score,men$seconds_elapsed)
cor.test(women$score,women$age)
cor.test(women$seconds_elapsed,women$age)
cor.test(women$score,women$seconds_elapsed)
                     