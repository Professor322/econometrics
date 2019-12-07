library(sandwich)
library(dplyr)
library(lmtest)
library(broom)
library(car)
library(ggplot2)
library(Ecdat)

data("Griliches")
df <- Griliches
help("Griliches")

model <- lm(data=df, lw80~age80+iq+school80+expr80)
summary(model)

vc <- vcov(model)
vc["iq", "age80"]
vcHC <- vcovHC(model, type = "HC3")
abs(vc["iq", "age80"] - vcHC["iq", "age80"])

get_std_error <- function (type = "HC3") {
  vcHC <- vcovHC(model, type = type)
  sqrt(vcHC["age80", "age80"])
}

get_std_error("HC3")
get_std_error("HC4m")
get_std_error("HC5")
get_std_error("HC1")

bptest(model, data = df,varformula = ~age80)
gqtest(model, fraction = 0.2, order.by = ~expr80, data = df)

data("Solow")
df <- Solow
df
help(Solow)
model2 <- lm(data=df, q~k+A)
summary(model2)
vc <- vcov(model2)

vcHAC <- vcovHAC(model2)
abs(vc["A", "A"] - vcHAC["A", "A"])

model3 <- lm(data=df, q~A)
summary(model3)

dwt(model3)
bgtest(model3, order = 3)

library(quantmod) # download from finance.google.com
library(rusquant) # download from finam.ru
library(sophisthse) # download from sophist.hse.ru
library(Quandl) # download from Quand1\
library(zoo) # time seq
library(xts) # more time seq
library(lubridate) #date manip

Sys.setlocale("LC_TIME", "C")
getSymbols(Symbols = "INTC", from = "2010-01-01", to = "2014-02-03", src="yahoo")
plot(INTC$INTC.Close, main = "")

getSymbols(Symbols = "MSFT", from = "2010-01-01", to = "2014-02-03", src="yahoo")
plot(MSFT$MSFT.Close, main = "")

close <- MSFT$MSFT.Close
stats::lag(close, -1)
model4 <- lm(close~stats::lag(close, -1) + stats::lag(close, -2))
sum <- summary(model4)
round(sum$r.squared, 2)
