# Author: Daniel B. Cooper

# Chapter 2 ----
# The following R code was adapted from Dr. Charnigo's Chapter 2b examples.

grid = (1:999)/1000
# This creates a grid of possible values of  p  from 0.001 to 0.999 in increments of 0.001.

firstprior = dbeta(grid,1,1)
# This defines prior relative credibilities for possible values of  p  on the grid.

firstposterior = dbeta(grid,1+1,1+4)
# This defines posterior relative credibilities for possible values of  p  on the grid,
# with the pessimistic prior, after  1  success and  4  failures.

secondposterior = dbeta(grid,1+4,1+16)
# This defines posterior relative credibilities for possible values of  p  on the grid,
# with the pessimistic prior, after  4  successes and  16  failures.

# here::i_am("posts/primary/portfolio_full/PortfolioChap2-3.r")
# png(here::here("posts/primary/portfolio_full/non-informative-prior.png"))
png("./posts/primary/portfolio_full/non-informative-prior.png", width = 400, height = 400)

plot(grid,firstprior,type="l",ylim=c(0,1.1*max(firstprior,firstposterior,secondposterior)),
	col=1, xlab="p", lwd=2, ylab="Relative credibilities")

lines(grid,firstposterior,col=2,lty=5,lwd=2)
lines(grid,secondposterior,col=4,lty=3,lwd=2)

legend(0.5,4,col=c(1,2,4),lty=c(1,5,3),lwd=c(2,2,2),
	legend=c("Prior","Posterior (n=5)", "Posterior (n=20)"))

title("Non-Informative Prior and Posterior Distributions")
# To the existing graphic we add a title.

dev.off()

# Chapter 2, grad student work
# i adapted this code: https://math.arizona.edu/~jwatkins/I4_bayes.pdf
# define credible interval value for gamma
gamma <- 0.95

# define function to find the minimum value of c where b - c is minimal
# b := qbeta(gamma + c, 5, 17)
# a := qbeta(c, 5, 17)
diff <- function(c) qbeta(gamma + c, 5, 17) - qbeta(c, 5, 17)

# find minimum value of c from the possible values of 0 to 0.05 using the optimize function
c <- optimize(diff, interval = c(0, 0.05), maximum = FALSE)
c$minimum
upper_limit <- qbeta(gamma + c$minimum, 5, 17)
lower_limit <- qbeta(c$minimum, 5, 17)
upper_limit
lower_limit

# long way: (use Newton-Raphson method to optimize)
diff(0.05)

png("./posts/primary/portfolio_full/95_CI.png", width = 400, height = 400)
plot(grid, secondposterior, type = "l", xlab = "p", ylab = "Relative credibilities")

lines( rep(lower_limit,100), seq(0,secondposterior[grid==round(lower_limit,3)],length.out=100), col=2, lwd=2)
lines( rep(upper_limit,100), seq(0,secondposterior[grid==round(upper_limit,3)],length.out=100), col=2, lwd=2)

title("Non-Informative Prior and Larger\nData Set Posterior Distribution with 95% CI")
dev.off()

diff_primeapprox <- function(x, delta = 0.0001) {
  (diff(x + delta) - diff(x - delta)) / (2 * delta)
}

diff_doubleprimeapprox <- function(x, delta = 0.0001) {
  (diff_primeapprox(x + delta) - diff_primeapprox(x - delta)) / (2 * delta)
}

guess = 0.04
maxit = 100
thresh = 0.000001
for (counter in 1:maxit)
{
oldguess = guess
guess = oldguess - diff_primeapprox( oldguess ) / diff_doubleprimeapprox( oldguess )
print(guess)
if ( abs(oldguess - guess) < thresh ) { break }
}

all.equal(guess,c$minimum)
# very similar!
