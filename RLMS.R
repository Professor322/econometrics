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
library("devtools")
library("rlms")

devtools::install_github("bdemeshev/rlms")

h <- read.rlms("r23ind.sav")

h2 <- select(h, qm1, qm2, qh6, qh5)
desctibe(h2)

h3 <- rename(h2, ves=qm1, rost=qm2, sex=qh5, b_year=qh6)
h3 <- mutate(h3, vpzrast=2019-b_year)

summary(h3$sex)

h4 <- filter(h3, sex=="МУЖСКОЙ")

qplot(data=h4, rost, ves)
qplot(data=h4, ves)
