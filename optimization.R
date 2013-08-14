##---- Portfolio Allocation Max Expectation

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

#----
