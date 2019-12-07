library("sandwich")
library("lmtest")
library("car")
library("bstats")
library("zoo")
library("xts")
library("dplyr")
library("broom")
library("ggplot2")
library("lubridate")
library("devtools")
library("quantmod")
library("rusquant")
library("sophisthse")
library("Quandl")
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
library("gdata")
getwd()
h$X.3
h$X.4
jj <- c(1, 1:2)
tt1 <- tt[-jj]
tt

tt <- data.frame(h$Сальдо, h$Экспорт, h$Импорт, h$Год)
tt_2 <- na.omit(tt)
tt_2
qplot(data = tt_2,ylab = "Сальдо", y = tt_2$h.X.3,xlab = "Года(1994-2018)", x = tt_2$h.X, geom = "line")
ggplot(tt_2, aes(x = tt_2$h.Год))+geom_line(aes(y = tt_2$h.Сальдо), color = "green") + geom_line(aes(y = tt_2$h.Импорт), color = "red") + geom_line(aes(y = tt_2$h.Экспорт), color = "blue")
ggplot(tt_2, aes(x = tt_2$Год))
remove(tt[1,])
help(plot)
plot(tt$h.X,tt$h.X.3)
a <- h$X.3
remove <- c(1,0)
a <- tt_2$h.X.3
a
d <- c(1994:2018)
g <- h$X.3
m <- h[h$X.3 == 2]
h$X.3[1,1]
head(m)
head(g)
h <- read.csv("testplot2.csv", sep = ";", header = TRUE, dec = ",")
h <- mutate_each(h, "int", h$Сальдо)
glimpse(h)
na.omit(h)
h
glimpse(tt_2)
tt_2 <- mutate_each(tt_2, "fact", tt_2$h$X)
glimpse(tt_2)
plot(d,h$X.3)
qplot(h, x = , y = h$X.3)
aes(x = d,y = h$X.3)