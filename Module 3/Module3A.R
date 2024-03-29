# R code for Module 3 on indicator variables -
# except for the last section on the AirBbB example

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2) 

getwd()

# Use the setwd command to point to your folder where you have saved the EDSAL.csv file
#
# setwd("C:/Users/sn2/Dropbox (GaTech)/Computer/Desktop/Summer 2018 MGT 6203/May 23 Class")  

# edsal is a dataframe to store the contents of the EDAL.csv file
 
edsal <- read_csv("EDSAL.csv", col_types = list(
  Education = col_factor(c("HS", "UG", "GRAD")),
  Experience = col_integer(),
  Salary = col_double()))

str(edsal)  # what happened to the first row of the csv file? 
View(head(edsal,10))

# Using the mutate function, to create new variables
# I've creating two new indicator variables called Graduate and HS which are determined by the value of Education
# note the use of the pipe operator %>% to add these two new variables to edsal 

edsal<- edsal %>%
  mutate(Graduate = ifelse(Education=="GRAD",1,0)) %>%
  mutate(HS = ifelse(Education=="HS",1,0))

View(head(edsal,10))

ggplot(edsal, aes(x=Experience, y=Salary)) + geom_point() +
  scale_colour_hue(l=50) + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
        axis.text=element_text(size=40), axis.title=element_text(size=24,face="bold"))
  
RS.lm <- lm(Salary ~ Experience, data=edsal)
summary(RS.lm)

ggplot(edsal, aes(x=Experience, y=Salary)) + geom_point() +
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) +
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
        axis.text=element_text(size=40), axis.title=element_text(size=24,face="bold"))


DR1.lm <- lm(Salary ~ HS + Graduate, data=edsal)
summary(DR1.lm)


DR2.lm <- lm(Salary ~ Experience + HS + Graduate, data=edsal)
summary(DR2.lm)

# adding the INTERACTION VARIBLES to edsal using the mutate function in R
#
edsal<- edsal %>%
  mutate(H_Exp = HS*Experience) %>%
  mutate(G_Exp = Graduate*Experience)


DR3.lm <- lm(Salary ~ Experience + HS + Graduate + H_Exp + G_Exp, data=edsal)
summary(DR3.lm)

ggplot(edsal, aes(x=Experience, y=Salary, color=factor(Education)))  + geom_point(mapping = aes(color=factor(Education))) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) + # Extend regression lines
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
        axis.text=element_text(size=40), axis.title=element_text(size=24,face="bold"))
