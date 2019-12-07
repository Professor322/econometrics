library(ggplot2)
library(dplyr)
library(lmtest)
library(glmnet)
library(car)
library(Ecdat)

df <- ChickWeight
help("ChickWeight")
glimpse(df)

vec <- df$weight[df$Time == 10]
round(mean(vec), 2)

mean(df$weight[df$Diet == 1 & df$Time == 21])
mean(df$weight[df$Diet == 2 & df$Time == 21])
mean(df$weight[df$Diet == 3 & df$Time == 21])
mean(df$weight[df$Diet == 4 & df$Time == 21])

model <- lm(data = df, weight~Time+Diet)
sum <- summary(model)
round(sum$r.squared, 2)

se <- sqrt(0.49+0.25+0.5)
round(1/se, 3)

qplot(data=diamonds, log(price), color=cut) + facet_grid(~cut)

model_1 <- lm(data = diamonds, price~carat+table+x+y+depth)
summary(model)
help(diamonds)
conf <- confint(model_1, level = 0.90)
conf
round(conf[3],2)

data("BudgetFood")
help("BudgetFood")

df <- BudgetFood
glimpse(BudgetFood)

model_2 <- lm(data=BudgetFood, wfood~totexp+size)
summary(model_2)

nw <- data.frame(totexp=700000, size=4)
int <- predict(model_2, nw, level=0.9,interval = "prediction")
round(int[,2], 2)
resettest(model_2)
qf(0.95, df1=2, df2=23967)


h <- na.omit(BudgetFood)
model_22 <- lm(data = h, wfood~totexp+size)
summary(model_22)
aux_model <- lm(data=h, wfood~totexp*sex+size*sex)
summary(aux_model)
waldtest(aux_model,model_22)
qf(0.95,df1=3, df2=23968)
df <- mtcars
model_3 <- lm(data=mtcars, mpg~disp+hp+wt)
round(vif(model_3)[1],2)

glimpse(mtcars)
describe(mtcars)
#df <- select(df, -mpg)
h.pca <- prcomp(df, scale=TRUE)
h.pca$x[,1]
round(norm(h.pca$x[,1], type = "2"),2)
model_pca <- lm(data=mtcars, mpg~h.pca$x[,1]+h.pca$x[,2])
s <- summary(model_pca)
s
model_pca <- lm(data=mtcars, mpg~h.pca$x[,1]+h.pca$x[,2]+h.pca$x[,3])
ss <- summary(model_pca)
ss
ss$r.squared - s$r.squared
round(ss$r.squared - s$r.squared,2)
ss  
1200/6
