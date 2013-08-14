##---- Portfolio Allocation

# max portfolio expectation given constraints
portfolioAllocation <- function() {

    ## ExpectedReturns:
    ## | Growth (x1) | Value (x2) | Bond (x3) | MM (x4) |
    ## |-------------+------------+-----------+---------|
    ## |         25% |       5.5% |      3.3% |    2.5% |

    ## Constraints:
    ## 1) Max Investment for each Asset Class: 30% of Initial
    ## 2) 2*x1 + 3*x3 < 30% of Initial
    ## 3) x1 + 2*x2 + 3*x3 + 4*x4 < 2 * Initial
    ## 4) x1, x2, x3, x4 >= 0 and sums to 1

    library(lpSolve)
    library(lpSolveAPI)

    initial <- 10e6
    model <- make.lp(0, 4)
    set.objfn(model, -1*c(.25, .055, .033, .025))

    add.constraint(model, c(1, 0, 1, 0), "<=", .5*initial)
    add.constraint(model, c(4, 2, 2, 1), "<=", 2* initial)
    add.constraint(model, c(1,0, 0, 0), "<=", .4*initial)
    add.constraint(model, c(0, 1, 0, 0), "<=", .4*initial)
    add.constraint(model, c(0,0, 1, 0), "<=", .4*initial)
    add.constraint(model, c(0,0, 0, 1), "<=", .4*initial)
    add.constraint(model, c(1, 1, 1, 1), "=", initial)

    set.bounds(model, lower=c(0, 0, 0, 0))
    solve(model)
    max.val <- -get.objective(model)
    weights <- get.variables(model)

    cat("\nMax Objective Value:", as.integer(max.val))
    cat("\nx1:", as.integer(weights[1]))
    cat("\nx2:", as.integer(weights[2]))
    cat("\nx3:", as.integer(weights[3]))
    cat("\nx4:", as.integer(weights[4]))
    cat("\n\n")
}
portfolioAllocation()

# 2nd method of maximizing expectation
portfolioAllocation2ndMethod <- function () {

    library(lpSolve)

    initial <- 10e6
    constraints <- rbind(c(1, 0, 1, 0),
                         c(4, 2, 2, 1),
                         c(1, 0, 0, 0),
                         c(0, 1, 0, 0),
                         c(0, 0, 1, 0),
                         c(0, 0, 0, 1),
                         c(1, 1, 1, 1))
    lp <- lp(objective.in=-1*c(.25, .055, .033, .025),
             const.mat=constraints,
             const.rhs=initial*c(.5, 2, .4, .4, .4, .4, 1),
             const.dir=c(rep("<=", 6), "="))

    cat("\nMax Objective Value:", as.integer(-lp$objval))
    for(i in 1:4) {
        cat("\nx", i, ":", as.integer(lp$solution[i]))
    }
    cat("\n\n")

}
portfolioAllocation2ndMethod()

# Quadratic Programming, maximizing expected return and minimizing risk
portfolioRisk <- function() {
    ## invest in 3 stocks with expected returns .03, .05, .06
    ## constraints that the weights add up to 1
    ## and cov:
    ## [.01, .02, .02]
    ## [.02, .01, .02]
    ## [.02, .02, .01]
    ## k = 2
    ## max t(x)*beta - k/2 * t(beta) * Cov * beta

    library(quadprog)
    A <- rbind(rep(1, 3), diag(3))
    A <- t(A)

    x <- c(.005, .0075, .02)
    b <- c(1, 0, 0, 0)
    Cov <- matrix(c(0.01, 0.003, 0.003, 0.003, 0.01, 0.003, 0.003, 0.003, 0.01), nrow=3)

    result <- solve.QP(2 * Cov, x, A, b, meq=1)
    cat("\nMax Objective Value:", -result$value)
    for(i in 1 : 3) {
        cat("\nx", i, ":", result$solution[i])
    }
    cat("\n\n")
}
portfolioRisk()

#----
