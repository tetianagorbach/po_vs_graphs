# Author: Juha Karvanen
# Date: 2023-11-08
# This code presents an example on multivariate dependency structures
# that cannot be presented as a DAG in Example 1.

# Load required libraries
library(mvtnorm) # to simulate multivariate normal
library(ppcor) # to calculate partial correlations
library(bnlearn)

# Function to create a correlation matrix for multivariate normal
# distribution that follows the diamond dependency structure:
# A - B, A - C, B - D, C - D
sigmaf <- function(rho) {
  a <- (sqrt(8 * rho^2 + 1) - 1) / 2
  sigm <- matrix(c(1, rho, rho, a, rho, 1, a, rho, rho, a, 1, rho, a, rho, rho, 1), 4, 4)
  return(sigm)
}
# We consider the following example.
# The clustered graph for the example can be presented as a DAG as follows
# a -> y, c -> a, c -> y
# Here a is a univariate binary treatment, y is the observed outcome,  and
# C = (c1,c2,c3,c4) is a four-variate variables (clusters)
# that have the diamond dependency structure. It is known (and can be checked)
# that this structure cannot be presented as a DAG.

# Generate data -----------------------------------------------------------
# Set a seed for reproducibility
set.seed(2112023)
# Set the sample size
n <- 1000000

# Generate observed  4-variate confounders with a diamnond dependency structure
# c1 - c2, c1 - c3, c3 - c4, c2 - c4
covariance_diamond <- sigmaf(0.6)
confounders <- rmvnorm(n, sigma = covariance_diamond)
c1 <- confounders[, 1]
c2 <- confounders[, 2]
c3 <- confounders[, 3]
c4 <- confounders[, 4]
# Checking that the structure is indeed a diamond structure.
# In theory c1 is independent of c4 given c2 and c3:
ci.test(c1, c4, data.frame(confounders[, 2], confounders[, 3]))
print(covariance_diamond[c(1, 4), c(1, 4)] - covariance_diamond[c(1, 4), c(2, 3)] %*%
  solve(covariance_diamond[c(2, 3), c(2, 3)]) %*% covariance_diamond[c(2, 3), c(1, 4)])
# Empirical (by default pcor conditions on all other variables)
print(pcor(confounders))
# We can see that C has the diamond dependency structure.
# Generate binary treatment A  that depends on c1 and c4 via the logit link
a <- rbinom(n, size = 1, prob = plogis(c1 + c2))
# Generate potential outcomes
errors <- rmvnorm(n, sigma = matrix(c(1, 0, 0, 1), nrow = 2))
y1 <- 4 + c1 + c2 + errors[, 1]
y0 <- 2 + c1 + c2 + errors[, 2]
# Define the observed outcome
y <- ifelse(a == 1, y1, y0)

# Average treatment effect is equal to Ey1 - Ey0 = 4 + Ec1 + Ec2 - (2 + Ec1 + Ec2) = 4 - 2 = 2
mean(y1) - mean(y0)


# Check ignorability -----------------------------------------
ci.test(as.numeric(y0), as.numeric(a), data.frame(c1, c2))
ci.test(as.numeric(y1), as.numeric(a), data.frame(c1, c2))

# Average causal effect can be consistently estimated by a linear regression: --------
lm(y ~ a + c1 + c2 + c3 + c4)
lm(y ~ a + c1 + c2)
