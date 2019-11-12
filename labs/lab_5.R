
f <- function(x) {
  res <- x * x
  return (res)
}
f(3)
f(-1)

fs <- function(x, pow=2) {
  res <- x^pow
  return (res)
}

fs(4)
fs(2, pow = 5)

d <- cars
d[1,2] <- NA
d[3,1] <- NA
d

na_perc <- function(d) {
  res <- sum(is.na(d)/nrow(d)/ncol(d))
  return (res)
}

na_perc(d)

na_perc <- function(d) {
  if (!is.data.frame(d)) stop("d should be data.frame")
  res <- sum(is.na(d)/nrow(d)/ncol(d))
  return (res)
}

for(i in 5:10) {
  k <- i^2
  cat("i = ", i, "i^2 = ", k, "\n")
}

all_data <- NULL

for(fname in c("file01.csv", "file02.csv")) {
  temp <- read.csv(fname)
  all_data <- rbind(all_data, temp)
}
