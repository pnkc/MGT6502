# Bryson Cook
# MGT6502x, Summer 2018 HW1

rm(list = ls())
cat("\014")
set.seed(1)

#install.packages("ISLR")
#install.packages("ISLR")
library(ISLR)

data = data.frame(Auto)
head(data)

summary(data)

x <- data[1:7]
y <- data[1:7]
cor(x, y)

model1 = lm(mpg ~ horsepower, data)
summary(model1)

anova(model1)

hp = data.frame(horsepower = 101)
predict.lm(model1, hp,interval = "predict")

model2 = lm(mpg ~ horsepower+cylinders+weight+acceleration+year, data)
summary(model2)

hp2 = data.frame(horsepower = 104.5,cylinders = 4, weight = 3000, acceleration = 16, year = 80)

predict.lm(model2, hp2, interval = "predict")
VIF(model2)
