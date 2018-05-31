# Bryson Cook
# MGT6502x, Summer 2018 HW2, part 1

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

m1 = lm(Sales ~ Price, C1)
summary(m1)

m2 = lm(Sales ~ Price+Bad_Shelf+Good_Shelf, C1)
summary(m2)

m3 = lm(Sales ~ Price+ShelveLoc, C1)
summary(m3)

m4 = lm(Sales ~ Price+Bad_Shelf+Good_Shelf+Price_Bad_Shelf+Price_Good_Shelf, C1)
summary(m4)