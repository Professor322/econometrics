library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("hexbin")
library("dplyr")
library("GGally")
library(expss)

data <- diamonds
glimpse(diamonds)
help(diamonds)
describe(diamonds)
min(data$price)
count_if("Premium", data$cut)

help(glm)
model <- lm(data=data, price~carat+y+x)
summary(model)
round(coeftest(model),2)
t_crit <- qt(0.95,df=53940-3)
t_crit
std_error <- 25.765
right_border <- 126.003+t_crit*std_error
round(right_border, 2)
qt(0.95,df=61)
round(-qt(0.95,df=28)*0.5-3, 2)
round(pchisq(9,df=10),2)
