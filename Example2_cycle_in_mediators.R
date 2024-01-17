# Generate data for a model with a cycle for which unconfoundness is fulfilled
# a binary treatment, two confounders with a cyclic relationship, continuous outcome
# Graph is define by paths: c1 -> treat, c2 -> treat, c1 -> c2, c2 -> c1, c1 -> Y, c2 -> Y, treat -> Y
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
z1 <- ifelse(a==1, z11, z01) 
z2 <- ifelse(a==1, z12, z02) 
  
# Generate binary treatment that depends on c1 and c2

y1 <- 4 + z11  + error[, 5]
y0 <- 2 + z01  + error[, 6]
# Average treatment effect is equal to total effect of Ey1 - Ey0 = 4 + Ec1 + EC0 - (2 - Ec1 - EC0) = 4  - 2 = 2
mean(y1) - mean(y0) 
# Observed variables
y <- ifelse(a == 1, y1, y0)



# Estimate the ATE --------------------------------------------------------
lm(y ~  a)
lm(y ~ z1 +  a)

lm(y ~ z1 + z2 + a)
lm(z1 ~ a)

