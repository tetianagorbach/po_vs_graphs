# Author: Juha Karvanen/ code Tetiana Gorbach
# Date: 2024-05-27
# This code presents an example of M-bias in Example 4.

# Load required library
library(mvtnorm) # to simulate multivariate normal
library(ppcor) # to calculate partial correlations
library(bnlearn)


# Generate data -----------------------------------------------------------
# Set a seed for reproducibility
set.seed(13959077)
# Set the sample size
n <- 1000000
# Generate unobserved covariates
u1 <- rbinom(n, size = 1, prob = 0.6)
u2 <- rbinom(n, size = 1, prob = 0.4)
# Generate C that leads to M-bias
c <- (u1 + u2) %% 2
# Generate binary treatment A  that depends on u1
a <- rbinom(n, size = 1, prob = 0.1 + 0.8 * u1)
# Generate potential outcomes that depend on u2
y1 <- numeric(n)
y0 <- numeric(n)
y1[u2 == 1] <- rbinom(sum(u2 == 1),
  size = 1,
  prob = 0.9
)
y1[u2 == 0] <- rbinom(sum(u2 == 0),
  size = 1,
  prob = 0.1
)
y0[u2 == 1] <- rbinom(sum(u2 == 1),
  size = 1,
  prob = 0.1
)
y0[u2 == 0] <- rbinom(sum(u2 == 0),
  size = 1,
  prob = 0.9
)
# Define the observed outcome
y <- ifelse(a == 1, y1, y0)


# Checks ------------------------------------------------------------------
ci.test(as.numeric(y), as.numeric(a))
ci.test(y0, as.numeric(a), as.numeric(c)) # unconfoundness is not fulfilled
ci.test(y1, as.numeric(a), as.numeric(c))
ci.test(as.numeric(u1), as.numeric(c), as.numeric(u2)) # c depends on u1 and u2
ci.test(as.numeric(u2), as.numeric(c), as.numeric(u1))

ci.test(as.numeric(y1), as.numeric(u2)) # y(1) and y(0) depend on u2
ci.test(as.numeric(y0), as.numeric(u2))

# EY(1)
mean(y1)
# EY(1) using the adjustment for C
mean(y[a == 1 & c == 1]) * mean(c) + mean(y[a == 1 & c == 0]) * (1 - mean(c))

# EY(0)
mean(y0)
# EY(0) using the adjustment for C
mean(y[a == 0 & c == 1]) * mean(c) + mean(y[a == 0 & c == 0]) * (1 - mean(c))



# Estimation -------------------------------------------------------------
# Average treatment effect is equal to
mean(y1) - mean(y0)
# ATE estimated by adjusting Y on C
lm(y ~ a + c)

mean(y[a == 1 & c == 1]) * mean(c) + mean(y[a == 1 & c == 0]) * (1 - mean(c)) -
  mean(y[a == 0 & c == 1]) * mean(c) - mean(y[a == 0 & c == 0]) * (1 - mean(c))
