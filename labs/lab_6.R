library(devtools)
install.packages("sophisthse",repos="http://r-forge.r-project.org", type="source")

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


#

x <- c("2012-04-15", "2011-08-17")
y <- ymd(x)
y
y + days(20)
y - year(10)
day(y)
month(y)
year(y)

vignette("lubridate")

#

x <- rnorm(5)
x
y <- ymd("2014-01-01") + days(0:4)
y
ts <- zoo(x, order.by = y)
ts

stats::lag(ts, -1)
stats::lag(ts, 1)
diff(ts)

ts2 <- zooreg(x, start = as.yearqtr("2014-01"), frequency = 4)
ts2

ts3 <- zooreg(x, start = as.yearmon("2014-01"), frequency = 12)
ts3


data("Investment")
help("Investment")

start(Investment)
end(Investment)
time(Investment)
coredata(Investment)

dna <- Investment
dna[1,2] <- NA
dna[5,3] <- NA
dna
na.approx(dna)
na.locf(dna)


# donwloading of data
vignette("sophisthse_intro")

#hse doesnt work
a <- sophisthse("POPMOR_Y")
a

# quandl
b <- Quandl("FRED/GNP")
b

#finance.google.com
Sys.setlocale("LC_TIME", "C")

getSymbols(Symbols = "AAPL", from="2010-01-01", to="2014-01-01", src = "yahoo")
head(AAPL)
tail(AAPL)

#finam.ru

getSymbols(Symbols = "GAZP", from = "2011-01-02", to = "2014-09-09", src = "Finam")
head(GAZP)
tail(GAZP)

plot(GAZP)
autoplot(GAZP[,1:4])
autoplot(GAZP[,1:4], facets = NULL)
chartSeries(GAZP)

d <- as.zoo(Investment)
autoplot(d[,1:2], facets = NULL)

model <- lm(data=d, RealInv~RealInt+RealGNP)
summary(model)
coeftest(model)
confint(model)

d_aug <- augment(model, as.data.frame(d))
glimpse(d_aug)
qplot(data=d_aug, lag(.resid), .resid)

vcov(model)
vcovHAC(model)

coeftest(model, vcov. = vcovHAC(model))
conftable <- coeftest(model, vcov. = vcovHAC(model))
ci <- data.frame(estimate=conftable[,1],
                 se_ac = conftable[,2])
ci
ci <- mutate(ci, left_95=estimate - 1.96*se_ac, right_95=estimate + 1.96*se_ac)
ci
confint(model)

#test Durbin-Watson
# H0: there is no autocorrelation
# Ha: there is 1st order autocorrelation
dwt(model)
res <- dwt(model)
res$dw
res$p
res$r

# BG-test
# H0: there is no autocorrelation
# Ha: there is p-st order
bgtest(model, order = 1)
# H0 is not rejected

res <- bgtest(model, 2)
res$statistic
res$p.value
