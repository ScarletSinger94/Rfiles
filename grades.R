Grades<- read.csv("/Users/Singingking/Documents/Grades.csv")
hist(Grades$Grade, col = "blue")
a<-table(Grades$Grade)
a
summary(Grades$Grade)
sd(Grades$Grade)
plot(a)
