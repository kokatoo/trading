library(ggplot2)

##---- 30-Year compounding

compound.30.years <- function () {

    mu <- .085
    sigma <- .15
    initial <- 1000

    # 30 years returns
    rets <- matrix(rnorm(100*30, mu, sigma), ncol=30)

    values <- initial * apply(1 + rets.30, 1, prod)
    hist(values, breaks=20, col="skyblue", xlab="30-Year Value", ylab="Counts", main="30-Year Compounding")
    
    cat("\nInitial capital: ", initial)
    cat("\nMean return: ", mu)
    cat("\nStd Dev: ", sigma)
    cat("\nValue Mean: ", mean(values))
    cat("\nValue Std Dev: ", sd(values))
    cat("\n\n")
    
}
compound.30.years()

#----
