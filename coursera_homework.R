library(devtools)

install_github("bdemeshev/rlms")
library(rlms)
library(lubridate) #date manip
library(sandwich) # vcovHC vcobHAC
library(lmtest) # tests
library(car) # more tests
library(zoo) # time seq
library(xts) # more time seq
library(dplyr) #data manip
library(broom) # manip
library(ggplot2) # graphics

library(quantmod) # download from finance.google.com
library(rusquant) # download from finam.ru
library(sophisthse) # download from sophist.hse.ru
library(Quandl) # download from Quand1


setwd("Desktop/econometrics/")

df <- rlms_read(file = "r22i_os_31.sav")
glimpse(df)

2013 - df$rh6

glimpse(data)
data <- data.frame(salary=df$rj13.2, age = (2013 - df$rh6),
                   sex = df$rh5, educ = df$r_diplom,
                   status = df$status, work_liking = df$rj1.1.1)
glimpse(data)

data <- filter(data, status %in% 1:2)
unique(data$work_liking)
data$work_liking

data <- filter(data, work_liking %in% c(NA,1,2))
nrow(data)

glimpse(data)
summary(data)
data[data$sex == 2, "sex"] <- 0
glimpse(data)

data[data$status == 1, "status"] <- 0
data[data$status == 2, "status"] <- 1

glimpse(data)
data <- mutate_each(data, as.factor, status, work_liking, sex)
glimpse(data)

glimpse(data)
data[!is.na(data$work_liking) & data$work_liking == 2, "work_liking"] <- 0
glimpse(data)
data <- mutate_each(data, as.factor, status, work_liking, sex)
glimpse(data)

data <- mutate(data, middle_unfinished = ifelse(test = (educ %in% 1:3), yes = 1, no = 0))               
data <- mutate(data, middle_finished = ifelse(test = (educ == 4), yes = 1, no = 0))
data <- mutate(data, middle_finished_special = ifelse(test = (educ == 5), yes = 1, no = 0))
data <- mutate(data, higher = ifelse(test = (educ == 6), yes = 1, no = 0))

data <- mutate_each(data, as.factor, middle_unfinished, middle_finished, middle_finished_special, higher)
glimpse(data)

summary(data)

data <- na.omit(data)
nrow(data)
qplot(data = data, salary / 1000) + facet_grid(~sex)
qplot(data = data, age) + facet_grid(~sex)

model <- lm(data=data, salary~age+sex+middle_finished+middle_finished_special + higher + status + work_liking)
summary(model)

coeftest(model, vcov. = vcovHC(model))
summary(model)
