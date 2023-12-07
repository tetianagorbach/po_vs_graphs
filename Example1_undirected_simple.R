# Author: Juha Karvanen
# Date: 2023-11-08
# This code presents an example on multivariate dependency structures
# that cannot be presented as a DAG.

# Load required library
library(mvtnorm) # to simulate multivariate normal
library(ppcor) # to calculate partial correlations
library(bnlearn)

# Function to create a correlation matrix for multivariate normal
# distribution that follows the diamond dependency structure:
# A - B, A - C, B - D, C - D
# TODO: check the valid range for rho. Is it (-1,1) or something else?
sigmaf <- function(rho) {
  a <- (sqrt(8 * rho^2 + 1) - 1) / 2
  sigm <- matrix(c(1, rho, rho, a, rho, 1, a, rho, rho, a, 1, rho, a, rho, rho, 1), 4, 4)
  return(sigm)
}
# We consider an example with nine variables.
# The clustered graph for the example can be presented as a DAG as follows
# A -> Y, Z -> A, Z -> Y
# Here A is a univariate treatment (binary) and
# Z = (Z1,Z2,Z3,Z4) and Y = (Y1,Y2,Y3,Y4) are four-variate variables (clusters)
# that have the diamond dependency structure. It is known (and can be checked)
# that this structure cannot be presented as a DAG.
# Generate data -----------------------------------------------------------
set.seed(2112023)
n <- 1000000

# Generate observed  4-variate confounders with a diamnond dependency structure  
# Z1 - Z2, Z1 - Z3, Z3 - Z4, Z2 - Z4
covariance_diamond <- sigmaf(0.6)
confounders <- rmvnorm(n, sigma = covariance_diamond)
c1 <- confounders[, 1]
c2 <- confounders[, 2]
c3 <- confounders[, 3]
c4 <- confounders[, 4]
# Checking that the structure is indeed a diamond structure.
# In theory Z1 is independent of Z4 given Z2 and Z3:
ci.test(c1, c4, data.frame(confounders[,2], confounders[,3]))
print(covariance_diamond[c(1, 4), c(1, 4)] - covariance_diamond[c(1, 4), c(2, 3)] %*% 
        solve(covariance_diamond[c(2, 3), c(2, 3)]) %*% covariance_diamond[c(2, 3), c(1, 4)])
# Empirical (by default pcor conditions on all other variables)
print(pcor(confounders))
# We can see that Z has the diamond dependency structure.

# Treatment A depends on C1 and C2 via the logit link
a <- 1 * (runif(n) > exp(c1 + c2) / (1 + exp(c1 + c2)))

a <- rbinom(n, size = 1, prob = plogis(c1 + c2))
y1 <- 4 + c1 + c2 + rnorm(n, 0, 1)
y0 <- 2 + c1 + c2 + rnorm(n, 0, 1)
# Average treatment effect is equal to total effect of Ey1 - Ey0 = 4 + Ec1 + EC0 - (2 - Ec1 - EC0) = 4  - 2 = 2
mean(y1) - mean(y0) 

