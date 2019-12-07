library(memisc)
library(ggplot2)
library(lmtest)
library(glmnet)

df <- airquality
help("airquality")

qplot(data=df, Ozone, Wind)

model <- lm(data=df, Ozone~Solar.R+Wind+Temp)
summary(model)
vif_data <- vif(model)
round(vif_data[1],3)

df <- na.omit(df)
glimpse(df)
y <- df$Ozone
M0 <- model.matrix(object=Ozone~0+Solar.R + Wind + Temp, df)
head(M0)
lambdas <- seq(50,0.1, length=30)

model_lasso <- glmnet(M0, y, alpha = 0, lambda = lambdas)
summary(model_lasso)

dt <- coef(model_lasso, s = 2)
round(dt[2], 3)

plot(model_lasso, xvar="lambda")
pca <- prcomp(M0, scale. = TRUE)
pca1 <- pca$x[,1]
pca2 <- pca$x[,2]
pca3 <- pca$x[,3]

qplot(x = pca1, y = pca2)
qplot(x=pca1, y=pca3)

d <- (1*(1/6) + 1*(1/6) +1*(1/6) +2*(1/6)+2*(1/6)+3*(1/6))
d
mean <- (300*3+400*4+1*200) / 900
mean
x <- ((3-3)^2)*300+((4-3)^2)*400+((1-3)^2)*200
var <- x/900
sqrt(var)
