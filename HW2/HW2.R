# Bryson Cook
# MGT6502x, Summer 2018 HW2

rm(list = ls())
cat("\014")
set.seed(1)

#install.packages("ISLR")
library(ISLR)

data = data.frame(Carseats)
head(data)

summary
C1 = data
C1$Bad_Shelf = 0
C1$Good_Shelf = 0
C1$Price_Bad_Shelf = 0
C1$Price_Good_Shelf = 0

for (i in 1:length(C1$ShelveLoc)){
  C1$ShelveLoc[i]
  if (C1$ShelveLoc[i] == 'Bad'){
    C1$Bad_Shelf[i] = 1
    C1$Price_Bad_Shelf[i] = C1$Bad_Shelf[i]*C1$Price[i]
  }
  if (C1$ShelveLoc[i] == 'Good'){
    C1$Good_Shelf[i] = 1
    C1$Price_Good_Shelf[i] = C1$Good_Shelf[i]*C1$Price[i]
  }
}

head(C1)

