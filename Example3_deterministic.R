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
# Generate binary treatment 
a <- rbinom(n, 1, 0.5)
# Generate counterfactual mediators
z1 <- rnorm(n, 3, 1)
z0 <- rnorm(n, 2, 1)
# Mediators that are counterfactuals that are deterministic functions of the mediators above
# The deterministic relation between z1 and v1, z0 and v0 violates positivity assumptions P(V=v)>0
# which in turn violates faithfulness.
v1 <- ifelse(z1 < 3, 0, 2)
v0 <- ifelse(z0 < 2, 0, 1)
# Generate potential outcomes
y1 <- 4 + v1 + rnorm(n, 0, 1)
y0 <- 2 + v0 + rnorm(n, 0, 1)
# Average treatment effect is equal to total effect of Ey1 - Ey0 = 4 + Ev1 - (2 - Ev0) = 4 + 1 - 2 - 0.5 = 2.5
# Ev1 = 2*P(z1>3) + 0P(z1<3) = 2*0.5+0*0.5= 1
mean(y1) - mean(y0)
# Observed variables
y <- ifelse(a == 1, y1, y0)
z <- ifelse(a == 1, z1, z0)
v <- ifelse(a == 1, v1, v0)

# Conditional independence properties of the counterfactuals cannot be depicted in a SWIG
# Y(t) indep V(t)|M(t)


# Estimate DAGs from the observed and potential outcomes data -------------
# Estimate a graph from the observed data
df <- as.data.frame(cbind(a, z, v, y))
cpdag <- pc.stable(df)
# Plot the graph to visualize the inferred causal relationships
plot(cpdag)
# Test for conditional independence of Y and Z given V
ci.test(y, z , v) # does this show something?

# Drawing a graphfrom the unobserved potential outcomes and the treatment variable (corresponding to the SWIG)
# not a valid SWIG due to the deterministic function v1 of z1!
df2 <- as.data.frame(cbind(a, z1, v1, y1))
cpdag2 <- pc.stable(df2)
# Plot the graph to visualize the inferred causal relationships
plot(cpdag2)
# Test for conditional independence using the ci.test function
ci.test(y1, z1, v1) # y1 independent of z1 given v1

# Estimate the ATE --------------------------------------------------------
summary(lm(y ~ a)) # back-door

lm(y ~ v  +  a)[["coefficients"]][["v"]]* # Baron and Kenny
lm(v~a)[["coefficients"]][["a"]] + 
lm(y ~ v  +  a)[["coefficients"]][["a"]] 


lm(y ~ z  +  a)[["coefficients"]][["z"]]* # Baron and Kenny using z
  lm(z ~ a)[["coefficients"]][["a"]] + 
  lm(y ~ z  +  a)[["coefficients"]][["a"]] 

