# 05.maximum_likelihood_estimation.R
#
# Author: Bob Verity
# Date: 2024-06-21
#
# Inputs: (none)
#
# Outputs: (none)
#
# Purpose:
# Demonstrates how we can write a likelihood function in a way that avoids
# numerical underflow. Demonstrates three different ways of estimating the
# maximum likelihood parameter value: 1) analytical, 2) manual search, 3) hill
# climbing
#
# ------------------------------------------------------------------

# we assume a model of exponential waiting times, for example this could be time
# between first symptoms and becoming hospitalised. We have n samples and lambda
# is the rate of the exponential distribution (1 over the average time).
n <- 10
lambda <- 0.2

x <- rexp(n, rate = lambda)

# we can write the log-likelihood function over a range of values of lambda. If
# we simply exponentiated this function to arrive at the likelihood we would run
# in to underflow issues when n is large, meaning the likelihood gets so small
# it is below R's numerical precision. A trick to avoid this is to first
# normalise the loglikelihood by subtracting its maximum value, meaning at least
# part of the curve will be anchored at loglike=0 (which is like=1). Then we can
# safely exponentiate to get back the likelihood curve
lambda_vec <- seq(0, 1, l = 1001)

loglike <- n*log(lambda_vec) - lambda_vec*sum(x)
like <- exp(loglike - max(loglike))

plot(lambda_vec, like, type = 'l')
abline(v = lambda, col = 2)

# our first maximum likelihood estimate comes from mathematicall solving the
# likelihood to find the point at which the gradient equals zero, which occurs
# at the peak of the distribution. Note that this is only possible for some
# simple models.
math_lambda <- n / sum(x)

# the second approach is to simply manually find the highest point in the curve.
# This is only possible for low-dimensional problems. For example, we can do
# this here as we have calculated the entire likelihood curve, but for a 10
# parameter model we would be unable to produce this grid to search over
manual_lambda <- lambda_vec[which.max(loglike)]

# the third approach is to use a hill-climbing algorithm via optim. This can be
# applied more generally to complex and high-dimensional problems. However, it
# does have some weaknesses, which we will cover in later scripts
optim_lambda <- optim(0.1, fn = function(lambda) {
  -(n*log(lambda) - lambda*sum(x))
}, method = "Brent", lower = 0, upper = 1)

# lets compare all three estimates
manual_lambda
optim_lambda$par
math_lambda