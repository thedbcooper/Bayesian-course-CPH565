
x <- list()
x$player <- 1:7
x$success <- c(18, 16, 14, 13, 11, 10, 8)
x$total <- rep(45, times = 7)

x$prop = x$success/x$total

mean(x$prop)
sd(x$prop)

source("./DBDA2Eprograms/DBDA2E-utilities.R")

betaParam <- betaABfromMeanSD(mean = mean(x$prop), sd = sd(x$prop))

x$mean <- (x$success + betaParam$a) / (x$total + betaParam$a + betaParam$b)

x
