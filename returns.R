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
corr.ret <- function() {
    source("../R-examples/misc.R")

    GM.prices <- get.quotes("GM")$Close
    F.prices <- get.quotes("F")$Close
    n <- length(GM.prices)

    GM.ret <- GM.prices[2:n]/GM.prices[1:(n-1)] - 1
    F.ret <- F.prices[2:n]/F.prices[1:(n-1)] - 1
    plot(GM.ret, F.ret)

    data <- data.frame(GM.ret, F.ret)

    # plot overlapping histograms
    library(reshape2)
    data.melt <- melt(data)
    ggplot(data.melt, aes(value, fill=variable)) + geom_histogram(alpha=.2)

    # plot scatterplots as ellipses
    corrs <- cor(data)
    op <- par(mfrow=c(1,2))
    plotcorr(corrs, col="skyblue")
    plotcorr(corrs, numbers=T)
    par(op)

    # plot as pairs
    panel.corr <- function(x, y, digits=2, prefix="", cex.cor, ...) {
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y)
        txt <- format(r, digits=digits)[1]
        txt <- paste(prefix, txt, sep="")

        text(0.5, 0.5, txt)
    }
    pairs(data, lower.panel=panel.smooth, upper.panel=panel.corr)

    # plot rolling correlations
    library(zoo)
    data.zoo <- zoo(data)
    GM.F <- rollapply(z, 30, function(x) cor(x[,1], x[,2]), by.column=F)
    plot(GM.F)
}

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
    GM.prices <- GM$Close
    dates <- as.character(format(as.POSIXct(GM$Date[-1]), "%Y-%m-%d"))

    n <- length(GM.prices)
    GM.ret <- GM.prices[2:n]/GM.prices[1:(n-1)] - 1
    GM.ret <- timeSeries(GM.ret * 100, charvec=dates)
    GM.ret.abs <- abs(GM.ret)
    colnames(GM.ret) <- "GMRet"

    # normal returns
    dev.new()
    par(mfrow=c(2, 2))
    plot(GM.ret, main="Daily Returns of GM", col="skyblue")
    grid()
    boxplot(GM.ret, main="Box Plot of Returns", col="skyblue", cex=.5, pch=19)
    acf(GM.ret, main="ACF of Returns", col="skyblue", ci.col="red")
    pacf(GM.ret, main="PACF of Returns", col="skyblue", ci.col="red")

    # abs returns
    dev.new()
    par(mfrow=c(2, 2))
    acf(GM.ret.abs, main="ACF of Abs Returns", col="skyblue", ci.col="red")
    pacf(GM.ret.abs, main="PACF of Abs Returns", col="skyblue", ci.col="red")
    names(GM.ret)
    qqnorm(GM.ret, main="QQ-Plot of Returns", col="skyblue", cex=.5, pch=19)
    plot(GM.ret.abs, type="h", main="Volatility Clustering", col="skyblue")
}

mktRets()

#----


