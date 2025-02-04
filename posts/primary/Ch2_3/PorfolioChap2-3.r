# Author: Daniel B. Cooper
# The following R code was adapted from Dr. Charnigo's Chapter 2b examples.

grid = (1:999)/1000
# This creates a grid of possible values of  p  from 0.001 to 0.999 in increments of 0.001.

firstprior = dbeta(grid,1,1)
# This defines prior relative credibilities for possible values of  p  on the grid.
# The prior relative credibilities are (reasonably) pessimistic about my ability to
# make free throws.

firstposterior = dbeta(grid,1+1,1+4)
# This defines posterior relative credibilities for possible values of  p  on the grid,
# with the pessimistic prior, after  1  success and  4  failures.

secondposterior = dbeta(grid,1+4,1+16)
# This defines posterior relative credibilities for possible values of  p  on the grid,
# with the pessimistic prior, after  4  successes and  16  failures.

png("./Ch2_3/non-informative-prior.png")

plot(grid,firstprior,type="l",ylim=c(0,1.1*max(firstprior,firstposterior,secondposterior)),
	col=1, xlab="p", lwd=2, ylab="Relative credibilities")
# This tells  R  to construct a graphic in which the relative credibilities of the first
# prior distribution are plotted against possible values of  p.  The "l" type asks  R  to
# generate a smooth line (curve, actually).  The lower "ylim" of  0  and the upper "ylim"
# of 1.1*max(firstprior,firstposterior,secondposterior) tell  R  about the range of
# vertical values to be shown in the graph, which is chosen so that no line will fail to
# be displayed in its entirety.  Because there is no specification for "xlim",  R  will
# by default use the lowest and highest possible values of  p  on the grid.  The color
# choice  1  is black, the label for the x axis is "p", the line width is doubled to make
# the line easier to see, and the label for the y axis is "Relative credibilities".

lines(grid,firstposterior,col=2,lty=5,lwd=2)
lines(grid,secondposterior,col=4,lty=3,lwd=2)
# To the existing graphic we add relative credibilities of the first and second
# posterior distributions.  The color choices  2  and  4  are red and blue.  The line
# types  5  and  3  are dashes and dots.  (Having different line types is potentially
# useful, in case a graphic cannot be printed in color.)  As before, line widths are
# doubled to make the lines easier to see.

legend(0.5,4,col=c(1,2,4),lty=c(1,5,3),lwd=c(2,2,2),
	legend=c("Prior","Posterior (n=5)", "Posterior (n=20)"))
# To the existing graphic we add a legend.  The upper left corner of the legend box
# is placed at horizontal location  0.5  and vertical location  4.  The colors, line
# types, and line widths in the legend are specified to match those in the graphic.
# The quoted strings in "legend" identify the corresponding distributions.

title("Non-Informative Prior and Posterior Distributions")
# To the existing graphic we add a title.

dev.off()
