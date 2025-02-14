# Author: Daniel B. Cooper

# Chapter 3 ----
# The following R code was adapted from Dr. Charnigo's Chapter 3b examples.

# Linear regression ----
# read data
my_data <- read.table("posts/primary/Ch2_3/Chapter3PortfolioData.txt")
x <- my_data$x
y <- my_data$y

fit = lm(y~x)

summary(fit)

plot(x,y,xlab="X",ylab="Y")
abline(a=fit$co[1],b=fit$co[2],lwd=2)
title("Linear regression")
legend(0.5,-3,col=c(1),lty=c(1),lwd=c(2),
	legend=c("Fitted"))


u = fit$f
v = fit$re
my_data_fitted = as.data.frame(round(cbind(x,y,u,v),2))

plot(my_data_fitted$x, my_data_fitted$v, xlab = "X", ylab = "Residuals")
lines(seq(min(my_data_fitted$x), max(my_data_fitted$x)), rep(0, 6))
title("Residual Plot of Chapter 3 Data")

# Mahalanobis Distance ----
