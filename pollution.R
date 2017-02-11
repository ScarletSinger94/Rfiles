class <- c("character", "numeric", "numeric", "numeric", "factor")
#pollution <- read.csv("EPA_vals.csv", colClasses = class) # reading the csv file into R
pollution <- mapexample
head(pollution)
str(pollution)
fivenum(pollution$pm25)
summary(pollution$pm25)
boxplot(pollution$pm25, col = "blue") 
summary(pollution$pm25) # almost the same as above; also includes mean

boxplot(pollution$pm25, col = "blue") # generate boxplot for the pmt25 column

# install a package called dplyr. It helps with data processing.
library(dplyr)#use a library that helps with data processing. Filter command in the next line comes from this library
filter(pollution, pm25 > 15)# find rows that have pm25 value greater than 15 

# install a package called maps. It helps with data geo-visualization.
library(maps) # tell R that we will use maps package
map("county", "california") #we create a map of the US based on states
map('usa') # create a map of whole USA
with(filter(pollution, pm25 > 15), points(longitude, latitude))#plot the abnormal pm25 value locations

hist(pollution$pm25, col = "green")#create histogram with green color
rug(pollution$pm25)#plot data points on the histogram
hist(pollution$pm25, col = "green", breaks = 100) #suggested number of bars

library(dplyr)#use dplyr library to call some functions 
table(pollution$region) %>% barplot(col = "wheat") # tabulate data based on region then create a barplot.
table(pollution$statename) %>% barplot(col = "wheat") # tabulate data based on statename then create a barplot.
