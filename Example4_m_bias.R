# Author: Juha Karvanen/ code Tetiana Gorbach
# Date: 2024-05-17
# This code presents an example of M-bias.

# Load required library
library(mvtnorm) # to simulate multivariate normal
library(ppcor) # to calculate partial correlations
library(bnlearn)


# Generate data -----------------------------------------------------------
# Set a seed for reproducibility
set.seed(13959077)
# Set the sample size
n <- 1000000

u1 <- rbinom(n, size = 1, prob = 0.5)
u2 <- rbinom(n, size = 1, prob = 0.5)


# c  <-  (u1+u2) %% 2
c1 <- rbinom(n, size = 1, prob = plogis(u1 + u2))

a <- rbinom(n, size = 1, prob = 0.1 + 0.8*u1)
y1 <-  numeric(n)
y0 <-  numeric(n)
y1[u2 == 1] <-  rbinom(sum(u2 == 1), 
                                size = 1, 
                                prob = 0.9)
y1[u2 == 0] <-  rbinom(sum(u2 == 0), 
                                size = 1, 
                                prob = 0.1)
y0[u2 == 1] <-  rbinom(sum( u2 == 1), size = 1, 
                                prob = 0.9)

y0[u2 == 0] <-  rbinom(sum(u2 == 0), size = 1, 
                                prob = 0.1)
mean(y1) - mean(y0)

# Generate observed outcome
y <- ifelse(a == 1, y1, y0)

ci.test(y0, as.numeric(a), as.numeric(c1)) 

# EY(1)
mean(y1) 
# EY(1) using the adjustment for C
mean(y[a==1 & c1==1])* mean(c1) + mean(y[a==1 & c1==0])*(1-mean(c1))

# EY(0)
mean(y0) 
# EY(0) using the adjustment for C
mean(y[a==0 & c1==1])* mean(c1) + mean(y[a==0 & c1==0])*(1-mean(c1))




# ATE
mean(y1) - mean(y0)
# ATE estimated through the adjustment
lm(y ~ a + c1)

mean(y[a==1 & c1==1])* mean(c1) + mean(y[a==1 & c1==0])*(1-mean(c1)) -
mean(y[a==0 & c1==1])* mean(c1) - mean(y[a==0 & c1==0])*(1-mean(c1))


