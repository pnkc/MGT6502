# Bryson Cook
# MGT6502x, Summer 2018 HW3, Q14-15

rm(list = ls())
cat("\014")
set.seed(1)

#install.packages("Ecdat")
library(Ecdat)


data = data.frame(Star)
head(data)

data1 = data[!(data$classk == 'regular'), ]
head(data1)

data1$small = 0

for (i in 1:length(data1$small)){
  if (data1$classk[i] == 'small.class'){
    data1$small[i] = 1
  }}

data1$totalscore = 0
for (i in 1:length(data1$small)){
  data1$totalscore[i] = data1$tmathssk[i] + data1$treadssk[i]
}
head(data1)

model1 = lm(totalscore~small, data = data1)
summary(model1)

model2 = lm(totalscore~small+totexpk, data = data1)
summary(model2)

