library(quantmod)

##---- SPY
getSymbols(Symbols="SPY", src="yahoo")

prices <- SPY$SPY.Close
log_rets <- diff(log(prices), lag=1)
