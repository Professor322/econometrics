library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library(knitr)
library(psych)
library(matlib)

df <- data.frame(check_wasted=c(7,8,3,10,7,2,10,4,2,16,6,11,8,3,10), 
                 talk_time=c(9,9,2,12,12,1,16,5,6,19,7,14,10,5,14))
df
model <- lm(data=df, check_wasted~talk_time)
summary(model)
describe(df)

summary(model)
qf(0.95, 1, 13)
cor(df)




qt(0.975,df = 13)
x <- df$talk_time
x
qf(0.95,df1 = 1, 13)

m <- cbind(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
m <- cbind(m, x)
m_t <- t(m)
m_t
sigma <- 25.19/13
var <- solve(m_t%*%m)
var*sigma
rss <- sum((x-mean(x))^2)
sigma <- 25.19/13
sigma
var <- sigma/rss
sqrt(var)

beta_hat <- coef(model)
beta_hat
eps_hat <- residuals(model)
eps_hat
y <- df$check_wasted
y
y_hat <- fitted(model)
y_hat
RSS <- deviance(model)
RSS
TSS <- sum((y-mean(y))^2)
TSS
ESS <- TSS - RSS
ESS
R2 <- ESS/TSS
R2
cor(y, y_hat)^2
X <- model.matrix(model)
cov(df)
sqrt(26.68)
