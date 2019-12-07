library(lmtest)
library(dplyr)
library(broom)
library(vcd)
library(ggplot2)

qchisq(0.99, df = 1)

round(pnorm(0.8), 2)

round(200/(200 * log(2) - log(7.78*10^-20)), 2)

t <- read.csv(file = "datasets/titanic.csv")
t <- mutate(t, sex = as.factor(sex), pclass = as.factor(pclass), survived = as.factor(survived))
glimpse(t)

logit_m1 <- glm(data = t, formula = survived~age+sex+fare+sibsp + parch, 
                family = binomial(link= "logit"), x = TRUE)
summary(logit_m1)
round(summary(logit_m1)$coefficients[5], 2)

logit_m2 <- glm(data = t, formula = survived~age+I(age^2)+sex+fare+I(fare^2)+sibsp, 
                family = binomial(link= "logit"), x = TRUE)
summary(logit_m2)
round(-summary(logit_m2)$coefficients[2]/(2*summary(logit_m2)$coefficients[3]), 1)

logit_m3 <- glm(data = t, formula = survived~age+I(age^2)+sex+fare+I(fare^2)+sibsp, 
                family = binomial(link= "logit"), x = TRUE)
summary(logit_m3)
round(confint(logit_m3, level = 0.95)[7], 2)

logit_m4 <- glm(data = t, formula = survived~age+I(age^2)+sex+fare+sibsp, 
                family = binomial(link= "logit"), x = TRUE)

man <- data.frame(sex = "male", sibsp = 2, fare = 200, age = 30)
pr_l_m4 <- predict(logit_m4, man, se = TRUE)
round(plogis(pr_l_m4$fit), 2)
round(plogis(pr_l_m4$fit - 1.96 * pr_l_m4$se.fit), 2)
confint(logit_m4, man, type = "response")

maBina(logit_m4)

d <- select(t,age, sibsp, sex, fare, survived)
d <- na.omit(d)
logit_m5 <- glm(data = d, formula = survived~age+I(age^2)+sex+fare+sibsp, 
                family = binomial(link= "logit"), x = TRUE)
summary(logit_m5)
pr_l_m5 <- predict(logit_m5, d, se = TRUE)
d <- mutate(d, prob = plogis(pr_l_m5$fit))
head(d)
round(count(d[d$prob > 0.6, ]) / count(d[d$prob <= 0.6, ]), 2)
table(d$survived, d$prob, )
logit_m5 <- glm(data = t, formula = survived~age + I(age^2) + sex + fare + sibsp,
                family = binomial(link = "logit"), x = TRUE)
summary(logit_m5)
vcov(logit_m5)[6, 6]
round(vcov(logit_m5)[6,6], 2)

