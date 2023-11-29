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

# Variables Y = (Y1,Y2,Y3,Y4) depend on A, Z = (Z1,Z2,Z3,Z4) and noise UY.
# Treatment A determines the strength of the effect of Z.
# Noise UY must have the same correlation structure as Z (but UY is independent from Z).
uy <- rmvnorm(n, sigma = covariance_diamond)
a2 <- 0.2 + 0.4 * a
y <- cbind(a2 * c1, a2 * c2, a2 * c3, a2 * c4) + (1 - a2) * uy
print(pcor(y))
# We can see that Y has the diamond dependency structure also in this case.


# Counterfactuals
y0 <- cbind(0.2 * c1, 0.2 * c2, 0.2 * c3, 
            0.2 * c4) + (1 - 0.2) * rmvnorm(n, sigma = covariance_diamond) # !!!!!!
y1 <- cbind(0.6 * c1, 0.6 * c2, 0.6 * c3, 
            0.6 * c4) + (1 - 0.6) * rmvnorm(n, sigma = covariance_diamond)
y <- ifelse(a == 1, y1, y0)

# The counterfactuals are independent from treatment a
# (component-wise testing because it seems that ci.test does not support
# multivariate testing)
ci.test(y0[, 1], a, confounders)
ci.test(y0[, 2], a, confounders)
ci.test(y0[, 3], a, confounders)
ci.test(y0[, 4], a, confounders)

ci.test(y1[, 1], a, confounders)
ci.test(y1[, 2], a, confounders)
ci.test(y1[, 3], a, confounders)
ci.test(y1[, 4], a, confounders)

# Estimate the ATE --------------------------------------------------------

