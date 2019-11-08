#lab2
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

# generate random variable
# z_1, ....., z_100 ~ N(5,9)
z <- rnorm(100, 5, 3)

qplot(z)

# create density function

x <- seq(-10,15, by=0.5)
y <- dnorm(x, mean=5, sd=3)
qplot(x,y, geom = "line")
# P(Z < 3)

# P(Z<3)=F(3)

pnorm(3, mean=5, sd=3)

# P(Z /in [4;9])
# P(Z<9)-P(Z<4)
pnorm(9, mean=5, sd=3) - pnorm(4, mean=5, sd=3)

# P(Z<a)=0.7 a?

qnorm(0.7, mean=5, sd=3)

# chisq, t, f

rchisq, dchisq, pchisq, qchisq
rt, dt, pt, qt

# multiple regression, checking hypothesis

h <- swiss
glimpse(h)
help(swiss)

model <- lm(data=h, Fertility~Catholic+Agriculture+Examination)
summary(model)

coeftest(model)
confint(model)

# check hypothesis b_cath=b_agri

model_aux <- lm(data=h, 
                Fertility~Catholic+I(Catholic+ Agriculture) + Examination)
summary(model_aux)
linear
linearHypothesis(model, "Catholic-Agriculture")

# standartize coefficients

h_st <- mutate_each(h, "scale")
glimpse(h_st)
model_st <- lm(data=h_st, Fertility~Catholic+Agriculture+Examination)
summary(model_st)

# artificial experiment

D <- matrix(nrow=100, rnorm(100*41,mean=0, sd=1))
df <- data.frame(D)
glimpse(df)
model_empty <- lm(data=df, X1~.)
summary(model_empty)


# compare models

model2<- lm(data=h, Fertility~Catholic+Agriculture)
compare_12 <- mtable(model, model2)
compare_12

# saving results of work

stuff <- list(data=h, model=model2)
saveRDS(file="mydata.RDS", stuff)

mylist <- readRDS("mydata.RDS")
summary(mylist$model)

# csv. comma separated values

t <- read.csv("", ])