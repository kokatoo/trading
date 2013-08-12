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

logPriceRWSimu <- function() {
    # r(t) = log(1+R(t)) = log(P(t)/P(t-1) = p(t) - p(t-1)
    initial <- 10000
    N <- 253*2
    r <- rnorm(N, mean=.05/N, sd=.25/sqrt(N))
    logPriceRW <- log(initial) + cumsum(r)

    par(mfrow=c(2, 1))
    # price chart
    plot(exp(logPriceRW), pch=19, cex=.5, col="skyblue")

    # log normal distribution
    hist(exp(r), col="skyblue", breaks=20, main="Log Normal Distribution")
    par(mfrow=c(1,1))
}
logPriceRWSimu()

logPriceSimu <- function(numPeriods) {
    initial <- 10000
    N <- 100
    r <- rnorm(N, mean=.5/N, sd=1/sqrt(N))

    cumsumRates <- NULL
    for(i in 1:5000) {
        rates <- sample(r, numPeriods, replace=T)
        cumsumRates <- rbind(cumsumRates, cumsum(rates[1:numPeriods]))
    }

    logPrice <- log(initial) + cumsumRates
    par(mfrow=c(1,2))
    hist(logPrice, breaks=20, cex.main=.7, col="skyblue",
         main=paste("Log Price Distribution for", numPeriods, "Period(s)"))
    hist(exp(logPrice), breaks=30, cex.main=.7, col="skyblue", xlab="Price",
         main=paste("Price Distribution for", numPeriods, "Period(s)"))
    par(mfrow=c(1,1))
}
logPriceSimu(10)

logPriceYahoo <- function() {
    source("../R-examples/misc.R")
    spy <- get.quotes("SPY")$Close

    logPrice <- log(spy)

    n <- length(logPrice)
    r <- diff(logPrice)

    hist(r, pch=19, col="skyblue")
}
logPriceYahoo()

#----

##---- Stylized Facts about Market Returns

nmktRets <- function() {
    # 1) time series mkt returns are generally not iid
    # 2) mkt returns does not have constant volatility
    # 3) the abs rets are highly correlated
    # 4) the distribution is leptokurtic
    # 5) exhibits volatility clustering
    # 6) distribution skewed to the left

    source("../R-examples/misc.R")
    library("timeSeries")

    GM <- get.quotes("GM")
    GMPrices <- GM$Close
    dates <- as.character(format(as.POSIXct(GM$Date[-1]), "%Y-%m-%d"))

    n <- length(GMPrices)
    GMRets <- GMPrices[2:n]/GMPrices[1:(n-1)] - 1
    GMRets <- timeSeries(GMRets * 100, charvec=dates)
    GMRetsAbs <- abs(GMRets)
    colnames(GMRets) <- "GMRet"

    # normal returns
    dev.new()
    par(mfrow=c(2, 2))
    plot(GMRets, main="Daily Returns of GM", col="skyblue")
    grid()
    boxplot(GMRets, main="Box Plot of Returns", col="skyblue", cex=.5, pch=19)
    acf(GMRets, main="ACF of Returns", col="skyblue", ci.col="red")
    pacf(GMRets, main="PACF of Returns", col="skyblue", ci.col="red")

    # abs returns
    dev.new()
    par(mfrow=c(2, 2))
    acf(GMRetsAbs, main="ACF of Abs Returns", col="skyblue", ci.col="red")
    pacf(GMRetsAbs, main="PACF of Abs Returns", col="skyblue", ci.col="red")
    names(GMRets)
    qqnorm(GMRets, main="QQ-Plot of Returns", col="skyblue", cex=.5, pch=19)
    plot(GMRetsAbs, type="h", main="Volatility Clustering", col="skyblue")
}

mktRets()

#----


