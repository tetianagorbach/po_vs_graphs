# Generate data for a model with a cycle for which unconfoundness is fulfilled
# a binary treatment, two confounders with a cyclic relationship, continuous outcome
# Graph is define by paths: C1 -> treat, C2 -> treat, C1 -> C2, C2 -> C1, C1 -> Y, C2 -> Y, treat -> Y
library(bnlearn)

# Set a seed for reproducibility
set.seed(123)
n <- 1000 # number of observations

# Define confounders C1, C2 with a cyclic relationship
error1 <- rnorm(n, 0, 1)
error2 <- rnorm(n, 0, 1)
## if c = c*A + e, then c = e*(I-A)^-1
inverse_i_a <- solve(diag(2) - matrix(c(0, 0.1, 0.1, 0), nrow = 2, byrow = T)) # (I-A)^-1
confouders <- cbind(error1, error2) %*% inverse_i_a # n*2 matrix of confounders C1, C2
C1 <- confouders[, 1]
C2 <- confouders[, 2]

# Define treatment that depends on C1, C2
T <- rbinom(n, size = 1, prob = plogis(C1 + C2))

Y1 <- 4 + C1 + C2 + rnorm(n, 0, 1)
Y0 <- 2 + C1 + C2 + rnorm(n, 0, 1)

mean(Y1) - mean(Y0) # ATE

# Observed variables
Y <- ifelse(T == 1, Y1, Y0)

# Drawing a graph from the observed data
df <- as.data.frame(cbind(T, C1, C2, Y))

cpdag <- pc.stable(df)

plot(cpdag)

ci.test(Y, as.numeric(T), data.frame(C1, C2)) # Y depends on T given C1, C2

# Drawing a graph from the unobserved potential outcomes and the treatmentment variable 
# undirected edges! but Y1 is independent of T given C1,C2: weak unconfoundness is fulfilled

df2 <- as.data.frame(cbind(T, C1, C2, Y1))

cpdag2 <- pc.stable(df2)

plot(cpdag2)

ci.test(Y1, as.numeric(T), data.frame(C1, C2)) # Y1 is independent of T given C1,C2: weak unconfoundness is fulfilled
ci.test(Y0, as.numeric(T), data.frame(C1, C2)) 
