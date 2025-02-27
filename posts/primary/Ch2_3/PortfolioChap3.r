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

png("./posts/primary/Ch2_3/linear-model.png", width = 400, height = 400)

plot(x,y,xlab="X",ylab="Y")
abline(a=fit$co[1],b=fit$co[2],lwd=2)
title("Linear regression")
legend(0.5,-3,col=c(1),lty=c(1),lwd=c(2),
	legend=c("Fitted"))

dev.off()

u = fit$f
v = fit$re
my_data_fitted = as.data.frame(round(cbind(x,y,u,v),2))

png("./posts/primary/Ch2_3/linear-model-residuals.png", width = 400, height = 400)

plot(my_data_fitted$x, my_data_fitted$v, xlab = "X", ylab = "Residuals")
lines(seq(min(my_data_fitted$x), max(my_data_fitted$x)), rep(0, 6))
title("Residual Plot For Linear Model")
dev.off()

# Mahalanobis Distance ----
# The following R code was adapted from Dr. Charnigo's Chapter 3b examples.

mean_x <- mean(x)
mean_y <- mean(y)
sd_x <- sd(x)
sd_y <- sd(y)
correlation <- as.numeric(cor.test(x,y,method="pearson")[["estimate"]])
cov_xy <- sd_x * sd_y * correlation
var_x <- sd_x ^ 2
var_y <- sd_y ^ 2

meanvector = c(mean_x, mean_y)
varcovmatrix = cbind( c(var_x, cov_xy), c(cov_xy, var_y) )

# This initializes a vector in which Mahalanobis distances will be stored.
madistxy <- c()
for (k in 1:length(x))
{ madistxy[k] = sqrt( t(  c(x[k], y[k]) - meanvector ) %*% solve(varcovmatrix) %*% (  c(x[k], y[k]) - meanvector ) ) }

my_data_fitted$madistxy <- madistxy

# M distance contour plot
xrange = sort(x)

yrange = sort(y)

xgrid = as.vector(t(matrix( rep( xrange, length(yrange) ), ncol=length(yrange) )))

ygrid = rep( yrange, length(xrange) )

madistxy = rep(0, length(xgrid) )

for (k in 1:length(xgrid))
{ madistxy[k] = sqrt( t(  c(xgrid[k], ygrid[k]) - meanvector )
		%*% solve(varcovmatrix) %*% (  c(xgrid[k], ygrid[k]) - meanvector ) ) }

madistxymatrix = t( matrix( madistxy, ncol=length(xrange) ))


png("./posts/primary/Ch2_3/mdistance-plot.png", width = 400, height = 400)

contour( xrange, yrange, madistxymatrix,xlab="X",ylab="Y",main="Mahalanobis Distance Plot")

dev.off()
