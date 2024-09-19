# Author: Juha Karvanen/code: Tetiana Gorbach
# Date: 2024-05-17
# This code presents an example of a DAG with a trapdoor variable in Example 5

# Load required libraries
library(mvtnorm)
library(bnlearn)

# Generate data -----------------------------------------------------------
# Set a seed for reproducibility
set.seed(292377111)
# Set the sample size
n <- 100000
# Define confounders c1, c2
errors <- rmvnorm(n, sigma = diag(4))
# Generate confounders
u1 <- errors[, 1]
u2 <- errors[, 2]
c1 <- rbinom(n, size = 1, prob = plogis(3 + u1 + u2)) # 3 + u1 + u2 +   errors[, 1]
c2 <- rbinom(n, size = 1, prob = plogis(1 + c1)) # 1 +  c1 + errors[, 2]
# Generate binary treatment that depends on c2 and u1
a <- rbinom(n, size = 1, prob = plogis(c2 + u1))
# Generate potential outcomes that depend on u2
y1 <- 4 + u2 + errors[, 3]
y0 <- 2 + u2 + errors[, 4]
# Generate observed outcome
y <- ifelse(a == 1, y1, y0)

# EY(1)
mean(y1)

# EY(1) estimated using the identification
(mean(y[c1 == 0 & c2 == 1 & a == 1]) * mean(a[c1 == 0 & c2 == 1]) * (1 - mean(c1)) +
  mean(y[c1 == 1 & c2 == 1 & a == 1]) * mean(a[c1 == 1 & c2 == 1]) * mean(c1)) /
  (mean(a[c1 == 0 & c2 == 1]) * (1 - mean(c1)) + mean(a[c1 == 1 & c2 == 1]) * mean(c1))

(mean(y[c1 == 0 & c2 == 0 & a == 1]) * mean(a[c1 == 0 & c2 == 0]) * (1 - mean(c1)) +
  mean(y[c1 == 1 & c2 == 0 & a == 1]) * mean(a[c1 == 1 & c2 == 0]) * mean(c1)) /
  (mean(a[c1 == 0 & c2 == 0]) * (1 - mean(c1)) + mean(a[c1 == 1 & c2 == 0]) * mean(c1))

# EY(0)
mean(y0)

# EY(0) estimate using the identification
(mean(y[c1 == 0 & c2 == 1 & a == 0]) * (1 - mean(a[c1 == 0 & c2 == 1])) * (1 - mean(c1)) +
  mean(y[c1 == 1 & c2 == 1 & a == 0]) * (1 - mean(a[c1 == 1 & c2 == 1])) * mean(c1)) /
  ((1 - mean(a[c1 == 0 & c2 == 1])) * (1 - mean(c1)) + (1 - mean(a[c1 == 1 & c2 == 1])) * mean(c1))

(mean(y[c1 == 0 & c2 == 0 & a == 0]) * (1 - mean(a[c1 == 0 & c2 == 0])) * (1 - mean(c1)) +
  mean(y[c1 == 1 & c2 == 0 & a == 0]) * (1 - mean(a[c1 == 1 & c2 == 0])) * mean(c1)) /
  ((1 - mean(a[c1 == 0 & c2 == 0])) * (1 - mean(c1)) + (1 - mean(a[c1 == 1 & c2 == 0])) * mean(c1))


# Check ignorability ------------------------------------------------------
ci.test(y0, as.numeric(a), data.frame(as.numeric(c1), as.numeric(c2))) # y0 is dependent on a given c1, c2: weak unconfoundedness is not fulfilled
ci.test(y1, as.numeric(a), data.frame(as.numeric(c1), as.numeric(c2))) # y1 is dependent on a given c1, c2: weak unconfoundedness is not fulfilled

# Estimation -------------------------------------------------------------
# Average treatment effect is equal to
mean(y1) - mean(y0)
# Average treatment effect estimated by adjusting y for c1 and c2
lm(y ~ a + c1 + c2)


# Helske's example --------------------------------------------------------
rm(list = ls())
set.seed(292377111)
# Set the sample size
n <- 1000000
v <- rbinom(n, size = 1, p = 0.5)
u <- rbinom(n, size = 1, p = 0.5)
c1 <- rbinom(n, size = 1, prob = 0.4 * u + 0.4 * v)
c2 <- rbinom(n, size = 1, prob = 0.4 + 0.4 * c1)
a <- rbinom(n, size = 1, prob = 0.4 * c2 + 0.4 * v)
y <- rbinom(n, size = 1, prob = 0.4 * a + 0.4 * u)


# EY(1) = P(Y=1|do(X)=1)
(0.2 + 0.4)^1 * (0.8 - 0.4)^(1 - 1)


# EY(1) estimate using the identification and c2=1
(mean(y[c1 == 0 & c2 == 1 & a == 1]) * mean(a[c1 == 0 & c2 == 1]) * (1 - mean(c1)) +
  mean(y[c1 == 1 & c2 == 1 & a == 1]) * mean(a[c1 == 1 & c2 == 1]) * mean(c1)) /
  (mean(a[c1 == 0 & c2 == 1]) * (1 - mean(c1)) + mean(a[c1 == 1 & c2 == 1]) * mean(c1))

# EY(1) estimate using the identification and c2=0
(mean(y[c1 == 0 & c2 == 0 & a == 1]) * mean(a[c1 == 0 & c2 == 0]) * (1 - mean(c1)) +
  mean(y[c1 == 1 & c2 == 0 & a == 1]) * mean(a[c1 == 1 & c2 == 0]) * mean(c1)) /
  (mean(a[c1 == 0 & c2 == 0]) * (1 - mean(c1)) + mean(a[c1 == 1 & c2 == 0]) * mean(c1))

# EY(0)
(0.2 + 0.4 * 0)^1 * (0.8 - 0.4 * 0)^(1 - 1)

# EY(0) estimate using the identification and c2 =1
(mean(y[c1 == 0 & c2 == 1 & a == 0]) * (1 - mean(a[c1 == 0 & c2 == 1])) * (1 - mean(c1)) +
  mean(y[c1 == 1 & c2 == 1 & a == 0]) * (1 - mean(a[c1 == 1 & c2 == 1])) * mean(c1)) /
  ((1 - mean(a[c1 == 0 & c2 == 1])) * (1 - mean(c1)) + (1 - mean(a[c1 == 1 & c2 == 1])) * mean(c1))

# EY(0) estimate using the identification and c2 =0
(mean(y[c1 == 0 & c2 == 0 & a == 0]) * (1 - mean(a[c1 == 0 & c2 == 0])) * (1 - mean(c1)) +
  mean(y[c1 == 1 & c2 == 0 & a == 0]) * (1 - mean(a[c1 == 1 & c2 == 0])) * mean(c1)) /
  ((1 - mean(a[c1 == 0 & c2 == 0])) * (1 - mean(c1)) + (1 - mean(a[c1 == 1 & c2 == 0])) * mean(c1))
