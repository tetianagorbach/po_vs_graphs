# Generate data for a model with a cycle for which unconfoundness is fulfilled
# a->z1->y, z1->z2, z2->z1.
library(bnlearn)

# Generate data -----------------------------------------------------------
# Set a seed for reproducibility
library(MASS)
set.seed(123)
# Set the sample size
n <- 100000
alpha <-  0.1
# Define confounders c1, c2 with a cyclic relationship
error <- mvrnorm(n, rep(0,6), diag(6))
a <- rbinom(n, size = 1, prob = 0.5)
# z11 = a + alpha*z2 + error1
# z12 = alpha * z1 + error2. Substitute z2 into z1:
# z11 = a + alpha(alpha * z1 + error2) + error1
# z11 = a + alpha^2 * z1 + alpha*error2 + error1
# z11 = (a + alpha * error2 + error1)/(1 - alpha^2)

z11 <- (3+alpha*error[ ,2] + error[ ,1])/(1 - alpha^2)
z12 <-  alpha * z11 + error[,2]
z01 <- (alpha*error[ ,3] + error[ ,4])/(1 - alpha^2)
z02 <- alpha * z01 + error[,3]

# Observed mediator
z1 <- ifelse(a==1, z11, z01) 
z2 <- ifelse(a==1, z12, z02) 

y1 <- 4 + z11  + error[, 5]
y0 <- 2 + z01  + error[, 6]
# Average treatment effect is 
mean(y1) - mean(y0) 
# Observed  outcome
y <- ifelse(a == 1, y1, y0)

# Estimate the ATE 
lm(y ~  a)



