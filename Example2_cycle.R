# Generate data for a model with a cycle that includes
# a binary treatment, two confounders with a cyclic relationship, continuous outcome
# Graph is define by paths: c1 -> a, c2 -> treat, c1 -> c2, c2 -> c1, c1 -> Y, c2 -> Y, treat -> Y
library(mvtnorm) # to simulate multivariate normal
library(bnlearn)

# Generate data -----------------------------------------------------------
# Set a seed for reproducibility
set.seed(123)
# Set the sample size
n <- 1000000 
# Define confounders c1, c2 with a cyclic relationship
errors <- rmvnorm(n, sigma = diag(4))
error1 <- errors[, 1]
error2 <- errors[, 2]
# c1 = 0.1*c2 + error1
# c2 = 0.1*c1 + error2
# in  a matrix form c = c*B + e. Then c = e*(I-B)^(-1)
B <- matrix(c(0, 0.1, 0.1, 0), nrow = 2, byrow = T)
inverse_i_b <- solve(diag(2) - B) # (I-B)^-1
confouders <- cbind(error1, error2) %*% inverse_i_b # n*2 matrix of confounders c1, c2
c1 <- confouders[, 1]
c2 <- confouders[, 2]
# Generate binary treatment that depends on c2
a <- rbinom(n, size = 1, prob = plogis(c1 + c2))

# Generate potential outcomes
y1 <- 4 + c1 + c2 + errors[, 3]
y0 <- 2 + c1 + c2 + errors[, 4]

# Generate observed outcome
y <- ifelse(a == 1, y1, y0)
# Average treatment effect is equal to total effect of Ey1 - Ey0 = 4 +  Ec1 + Ec2 - (2 + Ec1 + Ec2) = 4 - 2 = 2
mean(y1) - mean(y0) 


# Estimate DAGs from the observed and potential outcomes data -------------
# Estimate a graph from the observed data
df <- as.data.frame(cbind(a, c1, c2, y))
# Estimate the Completed Partially Directed Acyclic Graph (CPDAG)
cpdag <- pc.stable(df)
# Plot the CPDAG to visualize the inferred causal relationships
plot(cpdag)
# Test for conditional independence of y and a given c2
ci.test(y, as.numeric(a), data.frame(c2)) # y depends on a given  c2

# Drawing a graph from the unobserved potential outcomes and the treatment variable
#  but y1 is independent of a given c2: weak unconfoundness is fulfilled
# Estimate a graph from the potential outcomes data
df2 <- as.data.frame(cbind(a, c1, c2, y1))
cpdag2 <- pc.stable(df2)
# Plot the graph
plot(cpdag2)


# Check ignorability  -----------------------------------------------------
ci.test(y0, as.numeric(a), data.frame(c1, c2)) # y0 is independent of a given c1, c2: weak unconfoundedness is fulfilled
ci.test(y1, as.numeric(a), data.frame(c1, c2)) # y1 is independent of a given c1, c2: weak unconfoundedness is fulfilled

# Average causal effect can be consistently estimated by a linear regression: --------
lm(y ~ a + c1 + c2)

