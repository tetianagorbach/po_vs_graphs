# Author: Ingeborg Waernbaum
# Date: 2023-11-08
# This code presents an example on multivariate dependency structures with a
# deterministic relationships between the variables that cannot be presented as a DAG.
# Generate data with a binary treatment, continuous first mediator and dichotomized subsequent mediator
# no confounding

# Load required library
library(bnlearn)

# Generate data -----------------------------------------------------------
# Set a seed for reproducibility
set.seed(2911)
# Set the sample size
n <- 1000000
# Define confounders c1, c2 with a deterministic relationship
errors <- rmvnorm(n, sigma = diag(3))
# Generate confounders
c1 <- 3 + errors[, 1]
c2 <- 2*as.numeric(c1 >= 3)
# Generate binary treatment that depends on c2
a <- rbinom(n, size = 1, prob = plogis(c2))

# Generate potential outcomes
y1 <- 4 + c2 + errors[, 2]
y0 <- 2 + c2 + errors[, 3]

# Generate observed outcome
y <- ifelse(a == 1, y1, y0)

# Average treatment effect is equal to total effect of Ey1 - Ey0 = 4 + Ec1 - (2 + Ec2) = 4 - 2  = 2
mean(y1) - mean(y0)


# Estimate DAGs from the observed and potential outcomes data -------------
# Estimate a graph from the observed data
df <- as.data.frame(cbind(a, c1, c2, y))
cpdag <- pc.stable(df)
# Plot the graph to visualize the inferred causal relationships
plot(cpdag)


# Drawing a graph from the unobserved potential outcomes and the treatment variable (corresponding to the SWIG)
# not a valid SWIG due to the deterministic function c2 of c1!
df2 <- as.data.frame(cbind(a, c1, c2, y1))
cpdag2 <- pc.stable(df2)
# Plot the graph to visualize the inferred causal relationships
plot(cpdag2)



# Check ignorability ------------------------------------------------------
ci.test(y0, as.numeric(a), data.frame(c2)) # y0 is independent of a given c2: weak unconfoundedness is fulfilled
ci.test(y1, as.numeric(a), data.frame(c2)) # y1 is independent of a given c2: weak unconfoundedness is fulfilled

# Average causal effect can be consistently estimated by a linear regression: --------
lm(y ~ a + c2)


