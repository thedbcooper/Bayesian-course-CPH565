graphics.off()
source("./DBDA2Eprograms/DBDA2E-utilities.R")
source("./DBDA2Eprograms/BernGrid.R")

Theta = seq( 0 , 1 , length=1001 )  # Fine teeth for Theta.
pTheta = sqrt(0.25 - ((Theta - 0.5)^2))
pTheta = pTheta/sum(pTheta)      # Make pTheta sum to 1.0
Data = c(rep(0,3),rep(1,7))    # 7 heads N = 10

openGraph(width=5,height=7)
posterior = BernGrid(
  Theta,
  pTheta,
  Data,
  plotType = "Bars",
  showCentTend = "Mode",
  showHDI = TRUE,
  showpD = FALSE
)

saveGraph(file = "./posts/primary/portfolio_full/BernGridExample1", type = "png")

openGraph(width=5,height=7)
Data = c(rep(0,30),rep(1,70))    # 70 heads N = 100

posterior = BernGrid(
  Theta,
  pTheta,
  Data,
  plotType = "Bars",
  showCentTend = "Mode",
  showHDI = TRUE,
  showpD = FALSE
)

saveGraph(file = "./posts/primary/portfolio_full/BernGridExample2", type = "png")

pTheta = ifelse(abs(Theta - 0.5) >= 0.25, 2, 0)
pTheta = pTheta/sum(pTheta)      # Make pTheta sum to 1.0
Data = c(rep(0,50),rep(1,50))    # 50 heads N = 100

openGraph(width=5,height=7)
posterior = BernGrid(
  Theta,
  pTheta,
  Data,
  plotType = "Bars",
  showCentTend = "Mode",
  showHDI = TRUE,
  showpD = FALSE
)

saveGraph(file = "./posts/primary/portfolio_full/BernGridExample3", type = "png")
