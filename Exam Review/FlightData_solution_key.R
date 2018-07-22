
# Flight Data - solution key
# Karthik Babu Nattamai Kannan


setwd("~/data/Dropbox (GaTech)/teaching/2018-Summer/exam/sample")
#After installing, we need to call the package to make it available to R
library(dplyr)
library(readr)
library(ggplot2)
library(stargazer)


# Please create a dummy variable “many” that equals 1 when “passengers” is greater than 500, and equals 0 otherwise. 
# “WN” is the code for Southwest Airlines. http://en.wikipedia.org/wiki/List_of_airline_codes
# Please create a dummy variable “southwestdom” that equals 1 when “largestcarrier” is equal to “WN”, and equals 0 otherwise. 


flight <- read_csv("./FlightData.csv",
                   col_names = TRUE,
                   cols(
                        city1 = col_character(),
                        city2 = col_character(),
                        distance = col_integer(),
                        passengers = col_number(),
                        airfare = col_double(),
                        largestcarrier = col_character(),
                        mktshlargest = col_double(),
                        lowestfarecarrier = col_character(),
                        mktshlowestfare = col_number(),
                        farelargestcarrier = col_double(),
                        farelowestfarecarrier = col_character()
                      )) %>% 
          mutate(many = ifelse(passengers>500,1,0)) %>% 
          mutate(many_X_dist = many *distance) %>% 
          mutate(many_X_distlog = many *log(1+distance)) %>% 
          mutate(southwestdom = ifelse(largestcarrier=="WN",1,0))


flight <- flight %>% 
            mutate(many = ifelse(passengers>500,1,0)) %>% 
            mutate(southwestdom = ifelse(largestcarrier=="WN",1,0))
            


# q1
# Use dplyr to group by ‘many’ and ‘southwestdom’. Find the average airfare for each group. There will be four groups

flight %>% 
  group_by(many,southwestdom) %>% 
  summarise(avg_airfare = mean(airfare))

# Groups:   many [?]
# many southwestdom avg_airfare
# <dbl>        <dbl>       <dbl>
# 1  0            0            232
# 2  0            1.00         206
# 3  1.00         0            216
# 4  1.00         1.00         184

# select(flight, airfare, many,southwestdom) %>% filter(many == 0 , southwestdom == 0) %>% summary() # Mean   :232.20 
# select(flight, airfare, many,southwestdom) %>% filter(many == 0 , southwestdom == 1) %>% summary() # Mean   :205.5 
# select(flight, airfare, many,southwestdom) %>% filter(many == 1 , southwestdom == 0) %>% summary() # Mean   :216.50 
# select(flight, airfare, many,southwestdom) %>% filter(many == 1 , southwestdom == 1) %>% summary() # Mean   :183.6 


# q2
# Run linear regression with airfare as the dependent variable, ‘many’ and ‘southwestdom’ as the independent variables. Now, can you use the below table to derive the individual group means obtained in #1 from the regression model? Show your work as to how you got your answers

reg1 <- lm(airfare ~  many + southwestdom , data = flight)
summary(reg1)

# Solution:
# lm(formula = airfare ~ many + southwestdom, data = flight)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -134.462  -40.935   -2.984   35.858  164.666 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   233.082      2.508  92.930  < 2e-16 ***
#   many          -17.688      3.356  -5.271 1.66e-07 ***
#   southwestdom  -29.417      3.575  -8.229 5.86e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 52.69 on 997 degrees of freedom
# Multiple R-squared:  0.08689,	Adjusted R-squared:  0.08506 
# F-statistic: 47.44 on 2 and 997 DF,  p-value: < 2.2e-16


# The formula is -17.688(many) - 29.417(southwestdom) + 233.082 = airfare
# -17.788*(0) - 29.417*(0) + 233.082 = 233.082: The mean predicted by the regression model comes very close to the actual mean of 232.2041
# -17.788*(0) - 29.417*(1) + 233.082 = 203.665: The mean predicted by the regression model comes very close to the actual mean of 205.5138
# -17.788*(1) - 29.417*(0) + 233.082 = 215.294: The mean predicted by the regression model comes very close to the actual mean of 216.4954
# -17.788*(1) - 29.417*(1) + 233.082 = 185.877: The mean predicted by the regression model comes very close to the actual mean of 183.5835

# q3
# Create a scatterplot with airfare as y axis and distance as the x axis with the regression line shown. Interpret the plot.
  
ggplot(flight, aes(x=distance, y=airfare)) + geom_point() +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines


# According to the scatterplot, there is a positive relationship between distance and airfare. As the distance increases, we see an increase in airfare.
# When distance is zero, airfare is around 180, which can be interpretted as the fixed cost of operating the aircraft. Therafter, for every unit increase, airfare increases by the slope.


# q4
# Please run a linear regression to understand how “airfare” can be predicted by “distance”. Please write down your interpretation of the two coefficients in such a linear regression. 

reg2 <- lm(airfare ~  distance , data = flight)
summary(reg2)
# Call:
#   lm(formula = airfare ~ distance, data = flight)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -120.96  -30.95   -6.47   25.33  188.38 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 153.86034    2.61789   58.77   <2e-16 ***
#   distance      0.05894    0.00215   27.41   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 41.62 on 998 degrees of freedom
# Multiple R-squared:  0.4296,	Adjusted R-squared:  0.429 
# F-statistic: 751.5 on 1 and 998 DF,  p-value: < 2.2e-16


# Both of the incercept and non-intercept coefficients are statistically significant
# 
# The intercept means that the dependent variable “airfare” has a predicted value of 153.86034 when the independent variable “distance” equals 0. This can be considered the fixed cost of operating the aircraft.
# 
# The coefficient on the independent variable “distance” means that a unit change of “distance” will lead to a 0.05894 change in the dependent variable “airfare”.
# 
# This is consistent with the scatter plot obtained in q3.



# q5

# Please run a linear regression to understand how “airfare” can be predicted by “distance” for a subsample in the whole sample with “many” being 1. Please run a linear regression to understand how “airfare” can be predicted by “distance” for a subsample in the whole sample with “many” being 0. Please write down your interpretation of the two coefficients in such a linear regression

reg3 <- lm(airfare ~  distance , data = filter(flight,many == 0))
reg4 <- lm(airfare ~  distance , data = filter(flight,many == 1))


# Both of the incercept and non-intercept coefficients are statistically significant for both the models.
# 
# For many == 0:
# The coefficient on the independent variable “distance” means that a change of 1 in “distance” will lead to a 0.068 change in the dependent variable “airfare” within the subsample of 501 or more passengers
# 
# The intercept means that the dependent variable “airfare” has a predicted value of 133.982 when the independent variable “distance” equals 0 within the subsample of 501 or more passengers
# 
# many == 1:
# 
# The coefficient on the independent variable “distance” means that a change of 1 in “distance” will lead to a 0.052 change in the dependent variable “airfare” within the subsample of 500 or less passengers
# 
# The intercept means that the dependent variable “airfare” has a predicted value of 169.444 when the independent variable “distance” equals 0 within the subsample of 500 or less passengers

# q6

# Please run ONE linear regression to generate all the results in question 6. Please write down your interpretation of all the coefficients in such a linear regression. 
reg5 <- lm(airfare ~  distance + many+ many_X_dist, data = flight)
summary(reg5)

# All of the incercept and non-intercept coefficients are statistically significant.
# 
# By introducing the interaction term `many_X_dist` and the dummy vairable `many`, we modify both the intercept and slope of the regression.
# 
# The overall regression equation is:
# 
# airfare = 169.444 -35.461 x many + 0.052 x distance + 0.016 x many_X_dist
# 
# For many = 0, we substitute `many` with the value `0` to get
# 
# airfare = 169.444 -35.461 x (0) + 0.052 x distance + 0.016 x (0) 
#         = 169.444 +  0.052 x distance
#     
# This is the same regression model obtained in 1st part of q6.
# 
# For many = 1, we substitute `many` with the value `1` to get
# 
# airfare = 169.444 -35.461 x (1) + 0.052 x distance + 0.016 x (1) 
#         = (169.444 - 35.461) + (0.052 + 0.016)  x distance
#         = 133.983 + 0.068 x distance
# 
# This is the same regression model obtained in 2nd part of q6.

# q7

# Now, can you use ggplot with grouping on ‘many’ to show the two regression lines in the graph? Please interpret the plot explaining how it relates to questions 5,6 and 7.

ggplot(flight, aes(x=distance, y=airfare, color=factor(many))) + geom_point() +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines


# This plot shows the regressions calculated in the previous questions. It depicts how distance affects airfare with a distinction in regressions between 500 or less passengers and 501 and more passengers. The regression line for many = 0 shows a less steeper slope than many = 1 and it shows a higher intercept.

# q8
# Now can you create a scatter plot (with regression lines) with facets for two factor variables – many and southwestdom ? Please interpret the plot explaining how intercept and slope is for each facet. [Hint: see http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/ for examples on how to construct facets]

ggplot(flight, aes(x=distance, y=airfare, color=factor(southwestdom))) + geom_point() +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) + # Extend regression lines
  facet_grid(factor(southwestdom) ~ factor(many))


# The graph at the top left quadrant (0,0) decpicts the scatterplot (with a regression line) of how distance affects airfare when there are passengers of 500 or less and when Southwest is not the largest carrier. There is a moderate to strong postitive relationship between airfare and distance. The incercept seems to be between 150 and 200. The slope is not too steep and it is positive.
# 
# The graph at the top right quadrant (1,0) decpicts the scatterplot (with a regression line) of how distance affects airfare when there are passengers of 500 or less and when Southwest is the largest carrier. There is a strong postitive relationship between airfare and distance. The incercept seems to be between 100 and 150. The slope is not too steep and it is positive.
# 
# The graph at the bottom left quadrant (0,1) decpicts the scatterplot (with a regression line) of how distance affects airfare when there are passengers of 501 or more and when Southwest is not the largest carrier. There is a moderate to strong postitive relationship between airfare and distance. The incercept seems to be between 100 and 150. The slope is slightly steeper and it is positive.
# 
# The graph at the top left quadrant (1,1) decpicts the scatterplot (with a regression line) of how distance affects airfare when there are passengers of 501 or more and when Southwest is the largest carrier. There is a moderate to strong postitive relationship between airfare and distance. The incercept seems to be between 100 and 150. The slope is slightly steeper and it is positive.

