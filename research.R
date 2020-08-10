library(quantmod)

##---- SPY
getSymbols(Symbols="SPY", src="yahoo")

prices <- SPY$SPY.Close

myfile <- "./data/spy_prices.rds"
saveRDS(prices, file=myfile)

myfile <- "./data/spy_prices.rds"
prices <- readRDS(myfile)

myfile <- "./data/spy_prices.rds"
prices <- readRDS(myfile)
log_rets <- diff(log(prices), lag=1)
log_rets <- log_rets[-1]

dayofweeks <- sapply(log_rets, .indexwday)  

log_rets <- cbind(log_rets, dayofweeks)
log_rets <- as.data.frame(log_rets)
head(log_rets)

names(log_rets) <- c("return",  "dayofweek")
head(log_rets)

png("./images/spy_dayofweek_boxplot.png")
boxplot(return ~ dayofweek, data=log_rets,
        main="SPY Returns by Day of Week (2007-2020)",
        horizontal=TRUE, names=c("Mon", "Tues", "Wed", "Thurs", "Fri"))
dev.off()
