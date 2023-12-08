sumx <- 4460
sums <- 271.6
n <- 20
A2 <- 0.729
D4 <- 2.282
D2 <- 2.059

Xbar <- sumx/n
Sbar <- sums/n
UCLx <- Xbar + A2 * 559.2244
LCLx <- Xbar - A2 * 559.2244
UCLs <- D4 * 559.2244
LCLs <- 0
Rbar <- sums*D2

cat("xbar =", Xbar, "Sbar =", Sbar, "UCLx =", UCLx, "LCLx =", LCLx, "Rbar =", Rbar)