# R-Quadratic Optimization (Quadratic Programming
library(quadprog)
# Optimization
# min Q(x, y) : Q(x) = 1/2 [x, y][2 -1: -1 2][x:y] - [-3 2][x:y] + 4
# Subject to : y >= 2 - x ; y >= -2+x ; y <= 3

Dmat <- matrix(c(2, -1, -1, 2), nrow = 2, byrow = TRUE)
dvec <- c(-3, 2)
# quadprog required A^T x = b_o
# then we have to transpose  matrix coeff of x -> Amat
Amat <- t(matrix(c(1, 1, -1, 1, 0, -1), nrow = 3, byrow = TRUE))
bvec <- c(2, -2, -3)

sol  <- solve.QP(Dmat, dvec, Amat, bvec, meq = 0)
sol$solution
sol$value
sol$iterations
sol$Lagrangian


## Example portfolio optimization with 10 stocks combination
# Get monthly return data from 2014 through 2015
require(quantmod)
myStocks <- c("AAPL","XOM","GOOGL","MSFT","GE","JNJ","WMT","CVX","PG","WF")
getSymbols(myStocks, src='yahoo')
# Change to Monthly return (@month end)
returnsList <- lapply(myStocks, 
                      function(s) periodReturn(eval(parse(text=s)),
                                               period='monthly', subset='2015::2016'))

returns.df  <- do.call(cbind, returnsList)
colnames(returns.df) <- myStocks

# Quadratic Programming with
# Constraints : 
# 1 weight of stock (w) ; sum(w) = 1
# 2 range of w ; 0 <= w <= 1
# 3 target portfolio return >= 10% return (r) ; sum(r * w) >= 0.01 
# Objective fn : minimum risk Q(x) = w^t * C * w (where C = covarience return)

# Compute the average returns and covariance matrix of the return series
r <- matrix(colMeans(returns.df), nrow=1)
C <- cov(returns.df)

# Solve QP
library(quadprog)
# Constraints
# 1) weight of stock (w) ; sum(w) = 1
# Represted in [1..1]
A <- matrix(1,1,10)

# 3) sum(r * w) >= 0.01
# represened in r
# 2) range of w ; 0 <= w <= 1
# Break to 20 constraints* fn wrt. Iw >= 0 and -Iw >= -1 * each w for each constraints
# represented I = diag(10) and -I = -diag(10)
A <- rbind(A, r, diag(10),-diag(10))

# Right hand side
# for Iw >= 0 then right hand side = 10 times of 0
# for -Iw >= -1 then right hand side = 10 times of -1
f <- c(1, 0.01, rep(0,10),rep(-1,10))

sol <- solve.QP(Dmat=C, dvec = rep(0,10), Amat=t(A), bvec=f, meq=1)

# expected retrun from combination
sum(sol$solution * r)
# risk 0
sol$value
# iteration
sol$iterations

# plot portfolio allocation
library(ggplot2)
portfolio <- data.frame(name = myStocks, w = round(sol$solution,3))
ggplot(portfolio, aes(x=name, y=w)) + geom_bar(stat="identity", fill="blue")