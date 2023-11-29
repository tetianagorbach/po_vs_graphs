# Author: Juha Karvanen
# Date: 2023-11-02
# This code presents an example on multivariate dependency structures
# that cannot be presented as a DAG. 

library(mvtnorm) #to simulate multivariate normal
library(ppcor) #to calculate partial correlations

# Function to create a correlation matrix for multivariate normal
# distribution that follows the diamond dependency structure:
# A - B, A - C, B - D, C - D 
# TODO: check the valid range for rho. Is it (-1,1) or something else?
sigmaf <- function(rho) {
  a <- (sqrt(8*rho^2+1)-1)/2
  sigm <- matrix(c(1,rho,rho,a, rho,1,a,rho, rho,a,1,rho, a,rho,rho,1 ),4,4)
  return(sigm)
}

# We consider an example with nine variables.
# The clustered graph for the example can be presented as a DAG as follows
# A -> Y, Z -> A, Z -> Y
# Here A is a univariate treatment (binary) and
# Z = (Z1,Z2,Z3,Z4) and Y = (Y1,Y2,Y3,Y4) are four-variate variables (clusters) 
# that have the diamond dependency structure. It is known (and can be checked) 
# that this structure cannot be presented as a DAG.

set.seed(2112023)
n <- 1000000

# Variables Z = (Z1,Z2,Z3,Z4)
sigmaz <- sigmaf(0.6)
z <- rmvnorm(n, sigma = sigmaz)
# Checking the partial correlations:
# Theoretical Z1 indep Z4 | Z2,Z3:
print(sigmaz[c(1,4),c(1,4)] - sigmaz[c(1,4),c(2,3)] %*% solve(sigmaz[c(2,3),c(2,3)]) %*% sigmaz[c(2,3),c(1,4)] )
# Empirical (by default pcor conditions on all other variables)
print(pcor(z))
# We can see that Z has the diamond dependency structure.

# Treatment A depends on Z1 and Z2 via the logit link
a <- 1 * (runif(n) > exp(z[,1] + z[,2])/(1 + exp(z[,1] + z[,2])))

# Variables Y = (Y1,Y2,Y3,Y4) depend on A, Z = (Z1,Z2,Z3,Z4) and noise UY.
# Treatment A may switch the sign of Z.
# Noise UY must have the same correlation structure as Z (but UY is independent from Z).
uy <- rmvnorm(n, sigma = sigmaz)
y <- cbind(z[,1] * (2*a-1), z[,2] * (2*a-1), z[,3] * (2*a-1), z[,4] * (2*a-1)) + uy
print(pcor(y))
# We can see that Y has the diamond dependency structure.

# Randomizing treatment A
a <- 1 * (runif(n) < 0.8)
y <- cbind(z[,1] * (2*a-1), z[,2] * (2*a-1), z[,3] * (2*a-1), z[,4] * (2*a-1)) + uy
print(pcor(y))
# We can see that Y has the diamond dependency structure also in this case.




# Alternative type of the treatment effect (Perhaps even more interesting)
a2 <- 0.2 + 0.4*a
y <- cbind(a2 * z[,1], a2 * z[,2], a2 * z[,3], a2 * z[,4]) + (1 - a2) * uy
print(pcor(y))
# We can see that Y has the diamond dependency structure also in this case.

