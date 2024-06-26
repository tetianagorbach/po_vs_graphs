# Author: Juha Karvanen/code: Tetiana Gorbach
# Date: 2024-05-17
# This code presents an example of a DAg with a trpdoor variable

# Load required library
library(mvtnorm)
library(bnlearn)

# Generate data -----------------------------------------------------------
# Set a seed for reproducibility
set.seed(292377111)
set.seed(0)
# Set the sample size
n <- 1000000
# errors <- rmvnorm(n, sigma = diag(8))
# # Generate confounders
# u_z_c1 <- errors[, 1]
# u_c1_c3 <- errors[, 2]
# u_a_c3 <- errors[, 3]
# u_a_ya <- errors[, 4]

u_z_c1 <- rbinom(n, size = 1, prob = 0.5)
u_c1_c3 <- rbinom(n, size = 1, prob = 0.5)
u_a_c3 <- rbinom(n, size = 1, prob = 0.5)
u_a_ya <- rbinom(n, size = 1, prob = 0.5)
c1 <- rbinom(n, size = 1, prob = plogis(u_z_c1 + u_c1_c3)) #
c2 <- rbinom(n, size = 1, prob = plogis(1 + c1)) #
c3 <- rbinom(n, size = 1, prob = plogis(1 + c2 + u_a_c3 + u_c1_c3))

# Generate binary treatment
a <- rbinom(n, size = 1, prob = plogis(c3 + u_a_c3 + u_a_ya))

z1 <- rbinom(n, size = 1, prob = plogis(1 + u_z_c1)) 
z0 <- rbinom(n, size = 1, prob = plogis(u_z_c1))
z <- ifelse(a == 1, z1, z0)

# Generate potential outcomes
y1 <- rbinom(n, size = 1, prob = plogis(z1 + u_a_ya)) 
y0 <- rbinom(n, size = 1, prob = plogis(-1 + z0 + u_a_ya))


# Generate observed outcome
y <- ifelse(a == 1, y1, y0)

# EY(1)
mean(y1)

# EY(0)
mean(y0)

# df2 <- as.data.frame(cbind(a=as.numeric(a), c1=as.numeric(c1), c2=as.numeric(c2),
#                            c3 = as.numeric(c3), z = as.numeric(z), y = as.numeric(y)))
# cpdag2 <- pc.stable(df2)
# # Plot the graph
# plot(cpdag2)

# Estimation --------------------------------------------------------------
pc1 <- function(c1_value) {
  pc1_1 <- mean(c1)
  pc1_1^c1_value * (1 - pc1_1)^(1 - c1_value)
}

pa_given_c1_c2 <- function(a_value, c1_value, c2_value) {
  pa_1 <- mean(a[c1 == c1_value & c2 == c2_value])
  pa_1^a_value * (1 - pa_1)^(1 - a_value)
}

pa_given_c1_c2_c3 <- function(a_value, c1_value, c2_value, c3_value) {
  pa_1 <- mean(a[c1 == c1_value & c2 == c2_value & c3 == c3_value])
  pa_1^a_value * (1 - pa_1)^(1 - a_value)
}

pc3_given_c1_c2 <- function(c3_value, c1_value, c2_value) {
  pc3_1 <- mean(c3[c1 == c1_value & c2 == c2_value])
  pc3_1^c3_value * (1 - pc3_1)^(1 - c3_value)
}

pz_given_c1_c2_a <- function(z_value, c1_value, c2_value, a_value) {
  pz_1 <- mean(z[c1 == c1_value & c2 == c2_value &  a == a_value])
  pz_1^z_value * (1 - pz_1)^(1 - z_value)
}

pz_given_c1_c2_c3_a <- function(z_value, c1_value, c2_value, c3_value, a_value) {
  pz_1 <- mean(z[c1 == c1_value & c2 == c2_value & c3 == c3_value & a == a_value])
  pz_1^z_value * (1 - pz_1)^(1 - z_value)
}

py_given_c1_c2_c3_a_z <- function(y_value, c1_value, c2_value, c3_value, a_value, z_value) {
  py_1 <- mean(y[c1 == c1_value & c2 == c2_value & c3 == c3_value & a == a_value & z == z_value])
  py_1^y_value * (1 - py_1)^(1 - y_value)
}

g1_a_c2 <- function(a_value, c2_value) {
   pa_given_c1_c2(a_value = a_value, c1_value = 0, c2_value = c2_value) * pc1(0)  +
   pa_given_c1_c2(a_value = a_value, c1_value = 1, c2_value = c2_value) * pc1(1)
}

g2_a_z_c2 <- function(a_value, z_value, c2_value) {
    pz_given_c1_c2_a(z_value = z_value, c1_value = 0, c2_value = c2_value, 
                               a_value = a_value) *
    pa_given_c1_c2(a_value = a_value, c1_value = 0, c2_value = c2_value) *
    pc1(0) +
    pz_given_c1_c2_a(z_value = z_value, c1_value = 1, c2_value = c2_value, 
                                a_value = a_value) *
    pa_given_c1_c2(a_value = a_value, c1_value = 1, c2_value = c2_value) *
    pc1(1)
}


g3_a_z_c2_c3 <- function(a_value, z_value, c2_value, c3_value) {
    pz_given_c1_c2_c3_a(z_value = z_value, c1_value = 0, c2_value = c2_value, 
                        c3_value = c3_value, a_value = a_value) *
    pa_given_c1_c2_c3(a_value = a_value, 
                      c1_value = 0, c2_value = c2_value, c3_value = c3_value) *
    pc3_given_c1_c2(c3_value = c3_value, c1_value = 0, c2_value = c2_value) *
    pc1(0)  +
    pz_given_c1_c2_c3_a(z_value = z_value, c1_value = 1, c2_value = c2_value, 
                        c3_value = c3_value, a_value = a_value) *
    pa_given_c1_c2_c3(a_value = a_value, 
                   c1_value = 1, c2_value = c2_value, c3_value = c3_value) *
    pc3_given_c1_c2(c3_value = c3_value, c1_value = 1, c2_value = c2_value) *
    pc1(1) 
}


g4_a_c2_c3 <- function(a_value,  c2_value, c3_value) {
    pa_given_c1_c2_c3(a_value = a_value, 
                      c1_value = 0, c2_value = c2_value, c3_value = c3_value) *
    pc3_given_c1_c2(c3_value = c3_value, c1_value = 0, c2_value = c2_value) *
    pc1(0)  +
    pa_given_c1_c2_c3(a_value = a_value, 
                   c1_value = 1, c2_value = c2_value, c3_value = c3_value) *
    pc3_given_c1_c2(c3_value = c3_value, c1_value = 1, c2_value = c2_value) *
    pc1(1) 
}

g5_y_a_z_c2_c3 <- function(y_value, a_value, z_value, c2_value, c3_value) {
   py_given_c1_c2_c3_a_z(y_value = y_value, c1_value = 0, 
                         c2_value = c2_value, c3_value = c3_value, a_value = a_value, z_value = z_value) *
   pz_given_c1_c2_c3_a(z_value = z_value, c1_value = 0, c2_value = c2_value, 
                      c3_value = c3_value, a_value = a_value) *
   pa_given_c1_c2_c3(a_value = a_value, 
                      c1_value = 0, c2_value = c2_value, c3_value = c3_value) *
   pc3_given_c1_c2(c3_value = c3_value, c1_value = 0, c2_value = c2_value) *
   pc1(0)  +
   py_given_c1_c2_c3_a_z(y_value = y_value, c1_value = 1, 
                          c2_value = c2_value, c3_value = c3_value, a_value = a_value, z_value = z_value) *
   pz_given_c1_c2_c3_a(z_value = z_value, c1_value = 1, c2_value = c2_value, 
                        c3_value = c3_value, a_value = a_value ) *
   pa_given_c1_c2_c3(a_value = a_value, 
                   c1_value = 1, c2_value = c2_value, c3_value = c3_value) *
   pc3_given_c1_c2(c3_value = c3_value, c1_value = 1, c2_value = c2_value) *
   pc1(1) 
}

# Initialize the sum
identification_sum <- function(a_value_def, c2_value_def, y_value_def ){
  sum_over_a <- 0
  sum_over_z_c3 <- 0
  # Loop over all combinations of a, z and c3 in {0, 1}
    for (z_value in c(0, 1)) {
      for (c3_value in c(0, 1)){
        for (a_value in c(0, 1)) {
        sum_over_a <- sum_over_a + g5_y_a_z_c2_c3(y_value = y_value_def, 
                                        a_value = a_value, 
                                        z_value = z_value, 
                                        c2_value = c2_value_def, 
                                        c3_value = c3_value)*
          g4_a_c2_c3(a_value = a_value,
                     c2_value = c2_value_def, 
                     c3_value = c3_value)/
          g3_a_z_c2_c3(a_value = a_value, 
                       z_value = z_value, 
                       c2_value = c2_value_def, 
                       c3_value = c3_value) 
        }
        sum_over_z_c3 <- sum_over_z_c3 + sum_over_a*g2_a_z_c2(a_value = a_value_def,
                                                              z_value = z_value, 
                                                              c2_value = c2_value_def)
      }
      
  }
  sum_over_z_c3/g1_a_c2(a_value = a_value_def, 
                        c2_value = c2_value_def)
}

identification_sum(a_value_def = 1, c2_value_def = 1, y_value_def = 1) # should be close to mean(y1)
identification_sum(a_value_def = 1, c2_value_def = 0, y_value_def = 1) # should be close to mean(y1)
mean(y1)

identification_sum(a_value_def = 0, c2_value_def = 1, y_value_def = 1) # should be close to mean(y0)
identification_sum(a_value_def = 0, c2_value_def = 0, y_value_def = 1) # should be close to mean(y0)
mean(y0)








