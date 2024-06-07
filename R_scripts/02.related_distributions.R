# 02.related_distributions.R
#
# Author: Bob Verity
# Date: 2024-06-07
#
# Purpose:
# Explores the connection between some common probability distributions, namely
# the exponential and the gamma distribution, and the normal distribution as the
# limit of the sum of many random variables.
#
# ------------------------------------------------------------------

# let's explore the connection between the exponential and the gamma

# draw waiting times from an exponential distribution (assumes constant hazard)
reps <- 1e4
event <- 3
lambda <- 2

X <- rexp(reps*event, rate = lambda)

# sum times together to get the next event
t_event <- matrix(X, ncol = event) |>
  rowSums()

# histogram and pdf of time to event
max_t <- 20
hist(t_event, breaks = seq(0, max_t, l = 101), probability = TRUE)

t_vec <- seq(0, max_t, l = 1001)
ft <- dgamma(t_vec, shape = event, rate = lambda)

lines(t_vec, ft)

# ------------------------------------------------------
# let's prove that everything (nearly everything) ends up looking normal when
# summed together

reps <- 1e4
events <- 1e2

# draw X from almost any distribution you want and we should find that it ends
# up looking pretty normal when events is large enough
# for bonus points, try exploring the cauchy distribution (rcauchy) and see if
# this still works! (it doesn't)
X <- runif(reps*events)

# Z is the sum of lots of random variables (X)
Z <- matrix(X, ncol = events) |>
  rowSums()

hist(Z, breaks = 100)

