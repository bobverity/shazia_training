# 04.analytical_posterior.R
#
# Author: Bob Verity
# Date: 2024-06-13
#
# Inputs: (none)
#
# Outputs: (none)
#
# Purpose:
# Example of a model where we can derive the full posterior distribution
# analytically. Uses a Poisson(x | lambda) likelihood, and a Gamma(lambda)
# prior. This prior is conjugate to the likelihood, so they play nice together
# and the posterior ends up being Gamma.
#
# ------------------------------------------------------------------

# lambda is incidence of malaria in a given month

# set prior parameters
alpha <- 3
beta <- 1

# make prior on lambda
lambda <- seq(0, 100, l = 1001)

prior_lambda <- dgamma(lambda, shape = alpha, rate = beta)

plot(lambda, prior_lambda, type = "l", ylim = c(0, 0.1))

# invent some data
x <- 60

# solve for the posterior
post_lambda <- dgamma(lambda, shape = alpha + x, rate = beta + 1)

lines(lambda, post_lambda, col = 2)

# we can also obtain the posterior 95% credible interval very easily
qgamma(p = c(0.025, 0.975), shape = alpha + x, rate = beta + 1)
