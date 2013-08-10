library(ggplot2)

##---- 30-Year compounding

compound.30.years <- function () {

    mu <- .085
    sigma <- .15
    initial <- 1000

    # 30 years returns
    rets <- matrix(rnorm(100*30, mu, sigma), ncol=30)

    finalReturns <- initial * apply(1 + rets, 1, prod)
    hist(finalReturns, breaks=20, col="skyblue", xlab="30-Year Value", ylab="Counts", main="30-Year Compounding")

    cat("\nInitial amount: ", initial)
    cat("\nMean return: ", mu)
    cat("\nStd Dev: ", sigma)
    cat("\nFinal Mean: ", mean(finalReturns))
    cat("\nFinal Std Dev: ", sd(finalReturns))
    cat("\n\n")

}
compound.30.years()

#----

##---- Simulate joint distributions of stocks and bonds
bonds.stocks <- function () {

    library(mvtnorm)

    mu <- c(.085, .03)
    corr <- -.05
    N <- 5000
    weights <- c(.5, .5)
    initial <- 1000

    sigma <- matrix(c(mu[1]^2, corr*prod(mu), corr*prod(mu), mu[2]^2), ncol=2)
    sigma

    stockReturns <- rep(1, N)
    bondReturns <- rep(1, N)

    for (i in 1:30) {
        scenarios <- rmvnorm(N, mu, sigma)
        stockReturns <- stockReturns*(1+scenarios[,1])
        bondReturns <- bondReturns*(1+scenarios[,2])
    }

    finalReturns <- initial*(weights[1]*stockReturns + weights[2]*bondReturns)

    hist(finalReturns, breaks=20, col="skyblue", xlab="30-Year Value", ylab="Counts", main="Stocks and Bonds")

    cat("\nInitial amount: ", initial)
    cat("\nMean return: ", mu)
    cat("\nCov Matrix: ", sigma)
    cat("\nWeights: ", weights)
    cat("\nFinal Mean: ", mean(finalReturns))
    cat("\nFinal Std Dev: ", sd(finalReturns))
    cat("\n\n")

}
bonds.stocks()

#----

##---- Correlation of Returns

source("../R-examples/misc.R")

GMPrices <- get.quotes("GM")$Close
FPrices <- get.quotes("F")$Close
n <- length(GMPrices)

GMRets <- GMPrices[2:n]/GMPrices[1:(n-1)] - 1
FRets <- FPrices[2:n]/FPrices[1:(n-1)] - 1
plot(GMRets, FRets)

#----

##---- Log Price

initial <- 10000
r <- rnorm(253, mean=.05/253, sd=.25/sqrt(253))
logPrice <- log(initial) + cumsum(r)
plot(exp(logPrice))

#----




