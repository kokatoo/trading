##---- YTM

bondVal <- function(coupon, T, rate, par) {
    # T = time to maturity in years
    # rates = semiannual rates

    coupon/rate + (par - coupon/rate)*(1+rate)^(-2*T)
}

ytm <- function() {
    price <- 1200
    coupon <- 40
    T <- 30
    par <- 1000
    r <- seq(.02, .05, len=300)
    values <- bondVal(coupon, T, r, par)

    # interpolate ytm
    ytm1 <- spline(values, r, xout=price)$y

    plot(r, values, xlab="YTM", ylab="Price of Bond", type="l",
         main="Bond Price vs YTM", lwd=2)
    abline(h=1200)
    abline(v=ytm1)
    cat("\nYTM 1: ", ytm1)

    # using uniroot
    ytm2 <- uniroot(function(r) {
        bondVals(coupon, T, r, par) - price
    }, c(0.01, 1))$root
    cat("\nYTM 2: ", ytm2)
    cat("\n\n")
}

ytm()

#----
