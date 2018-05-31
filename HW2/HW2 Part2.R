# Bryson Cook
# MGT6502x, Summer 2018 HW2, part 2

rm(list = ls())
cat("\014")
set.seed(1)

mydata = data.frame(read.csv("EDSAL.csv",header=TRUE))
head(mydata)
 
mydata$lnSalary = 0
mydata$lnExperience = 0

for (i in 1:length(mydata$Salary)){
  mydata$lnSalary[i] = log(mydata$Salary[i])
  mydata$lnExperience[i] = log(mydata$Experience[i])}

head(mydata)
linlin = lm(Salary ~ Experience, mydata)
summary(linlin)

linlog = lm(Salary ~ lnExperience, mydata)
summary(linlog)

loglin = lm(lnSalary ~ Experience, mydata)
summary(loglin)

loglog = lm(lnSalary ~ lnExperience, mydata)
summary(loglog)