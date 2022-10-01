# Normal graphical methods
p <- function(x, y){
  out <- 142*x + 60*y
  return(out)
}

p(0, 71.42)
p(8.35, 66.65)
# This points maximized p
p(21.87, 53.14)
p(36.36, 0)

library(linprog)

# Set up problem matrix
# 143w + 60 b : maximized
# w + b <= 75
# 120w + 210b <= 15000
# 110w + 30b <= 4000

# Objective function
f.obj <- c(143, 60)
# Constraint matrix
f.con <- matrix(c(1, 1, 120, 210, 110, 30), nrow = 3, byrow = TRUE)
# Sign matrix
f.dir <- c("<=", "<=", "<=")
# Right hand side (RHS) constants
f.rhs <- c(75, 15000, 4000)

# run Linear Programming
out <- lp("max", f.obj, f.con, f.dir, f.rhs)

# x, y point that maximized & maximized output
out$solution
out

# run Integer Linear Programming with (all parameter must be init)
out.int <- lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:2)
out.int$solution
out.int