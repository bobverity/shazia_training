# 01.probability_mass_density.R
#
# Author: Bob Verity
# Date: 2024-06-07
#
# Purpose:
# Explores questions around the difference between a random variable and it's
# realised value, and between probability mass functions (discrete RVs) and
# probability density functions (continuous RVs).
#
# ------------------------------------------------------------------

# make a pmf
# assume my RV follows a binomial distribution
reps <- 1e2
N <- 10

# draw values of the random variable
X <- rbinom(reps, size = N, prob = 0.5)

# now explore the probability mass function
x <- 0:N
pr_x <- dbinom(x, size = N, prob = 0.5)

# lets compare
X_freq <- tabulate(X + 1, nbins = N + 1) / reps
barplot(X_freq, space = 0, names.arg = 0:N, ylim = c(0, 1.2*max(X_freq)))
points(x + 0.5, pr_x, pch = 20)

# --------------------------------------------------------
# now lets do the same thing but dividing through by the width of the interval
# to get to a density

# make a pmf
# assume my RV follows a binomial distribution
reps <- 1e2
N <- 1e3

# draw values of the random variable
X <- rbinom(reps, size = N, prob = 0.5) / N

# now explore the probability mass function
x <- 0:N / N
pr_x <- dbinom(x*N, size = N, prob = 0.5)

# that's broken, lets calculate the probability density instead
pr_x <- pr_x / (1 / N)

# lets compare
hist(X, breaks = seq(0, 1, l = 101))
points(x , pr_x, pch = 20)

# --------------------------------------------------------
# lets overlay the normal distribution, which is the pdf in the limit of
# inifinite N

# make a pmf
# assume my RV follows a binomial distribution
reps <- 1e2
N <- 1e3

# draw values of the random variable
X <- rbinom(reps, size = N, prob = 0.5) / N

# calculate the corresponding normal distribution (approximation)
x <- seq(0, 1, l = 1001)
fx <- dnorm(x, mean = 0.5, sd = sqrt(0.25/N))

hist(X, breaks = seq(0, 1, l = 101))
lines(x, fx, lwd = 2)

# --------------------------------------------------------
# what is the probability of getting a value of X between two limits? For this
# we need integration when working with pdfs.

# make a pmf
# assume my RV follows a binomial distribution
reps <- 1e5
N <- 1e3

# draw values of the random variable
X <- rbinom(reps, size = N, prob = 0.5) / N

# what is the chance of being between two limits
limit_lower <- 0.25
limit_upper <- 0.48
mean(X >= limit_lower & X <= limit_upper)

# how can I get at this from the continuous pdf approach?
# We need the integral of the normal density between 0.5 and 0.52
pnorm(limit_upper, mean = 0.5, sd = sqrt(0.25/N)) -
  pnorm(limit_lower, mean = 0.5, sd = sqrt(0.25/N))
