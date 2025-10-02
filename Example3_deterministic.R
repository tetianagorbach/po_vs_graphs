# This code presents an example on multivariate dependency structures in Example 3 with a
# deterministic relationships between the variables that cannot be presented as a DAG.


# Load required libraries
library(mvtnorm)
library(bnlearn)

# Generate data -----------------------------------------------------------
# Set a seed for reproducibility
set.seed(291111)
# Set the sample size
n <- 1000000
# Define confounders c1, c2 with a deterministic relationship
errors <- rmvnorm(n, sigma = diag(4))
# Generate confounders
c2 <- errors[, 1]
c3 <- errors[, 2]
c1 <- c2 + c3
c4 <- c2 - c3
# Generate a binary treatment that depends on c1 and c2
a <- rbinom(n, size = 1, prob = plogis(c1))
# Generate potential outcomes
y1 <- 4 + c4 + errors[, 3]
y0 <- 2 + c4 + errors[, 4]
# Generate the observed outcome
y <- ifelse(a == 1, y1, y0)

# Average treatment effect is equal to total effect of Ey1 - Ey0 = 4 + Ec1 - (2 + Ec2) = 4 - 2  = 2
mean(y1) - mean(y0)



# Check ignorability ------------------------------------------------------
ci.test(y0, as.numeric(a), data.frame(c2, c3)) # y0 is independent of a given c4: weak unconfoundedness is fulfilled
ci.test(y1, as.numeric(a), data.frame(c2, c3)) # y0 is independent of a given c4: weak unconfoundedness is fulfilled


# Average causal effect can be consistently estimated by a linear regression: --------
lm(y ~ a + c2 + c3)
lm(y ~ a + c4)
lm(y ~ a + c1)


# Constructing graphs
df <- as.data.frame(cbind(a, c1, c2, c3, c4,   y))
cpdag <- pc.stable(df)
# Plot the graph to visualize the inferred causal relationships
plot(cpdag)



