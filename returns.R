library(ggplot2)

##---- 30-Year compounding

compound.30.years <- function () {

    mu <- .085
    sigma <- .15
    initial <- 1000

    # 30 years returns
    rets <- matrix(rnorm(100*30, mu, sigma), ncol=30)

    values <- initial * apply(1 + rets, 1, prod)
    hist(values, breaks=20, col="skyblue", xlab="30-Year Value", ylab="Counts", main="30-Year Compounding")

    cat("\nInitial capital: ", initial)
    cat("\nMean return: ", mu)
    cat("\nStd Dev: ", sigma)
    cat("\n30-Year Value Mean: ", mean(values))
    cat("\n30-Year Value Std Dev: ", sd(values))
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

    stocksRet <- rep(1, N)
    bondsRet <- rep(1, N)

    for (i in 1:30) {
        scenarios <- rmvnorm(N, mu, sigma)
        stocksRet <- stocksRet*(1+scenarios[,1])
        bondsRet <- bondsRet*(1+scenarios[,2])
    }

    finalReturns <- initial*(weights*stocksRet + weights[2]*bondsRet)

    hist(finalReturns, breaks=20, col="skyblue", xlab="30-Year Value", ylab="Counts", main="30-Year Compounding of Stocks and Bonds")

    cat("\nInitial amount: ", initial)
    cat("\nMean return: ", mu)
    cat("\nStd Dev: ", sigma)
    cat("\nWeights: ", weights)
    cat("\nFinal Mean: ", mean(finalReturns))
    cat("\nFinal Std Dev: ", sd(finalReturns))
    cat("\n\n")

}
bonds.stocks()
