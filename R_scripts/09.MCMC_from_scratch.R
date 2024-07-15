# 09.MCMC_from_scratch.R
#
# Author: Bob Verity
# Date: 2024-07-15
#
# Inputs: (none)
#
# Outputs: (none)
#
# Purpose:
# Simple implementation of an MCMC using the Metropolis-Hastings algorithm.
#
# ------------------------------------------------------------------

# define a target distribution that we want to explore. Normally, this would be
# our posterior distribution. Here, we will just assume we are exploring a
# Poisson distribution
target_distribution <- function(x) {
  dpois(x, lambda = 5)
}
barplot(target_distribution(0:20), space = 0, names.arg = 0:20)

# define number of MCMC reps
reps <- 1e4

# probability of proposing a move down vs. up. If we make these probabilities
# equal then we will not need a Hastings correction. But it is completely valid
# to use unequal proposal probabilities, just as long as we use the Hastings
# correction
down_up_prob <- c(1, 2)

# store value in each rep of the MCMC
x <- rep(NA, reps)
x[1] <- 3

# main MCMC loop
for (i in 2:reps) {
  
  # propose a new value
  delta <- sample(c(-1, 1), 1, prob = down_up_prob)
  x_propose <- x[i-1] + delta
  
  # values cannot be negative in this example. Skip to next iteration (i.e.
  # reject proposal) if so
  if (x_propose < 0) {
    x[i] <- x[i-1]
    next
  }
  
  # calculate the target height at the current vs. proposed positions
  y_current <- target_distribution(x[i-1])
  y_propose <- target_distribution(x_propose)
  
  # calculate acceptance probability based on Metropolis ratio
  prob_accept <- y_propose / y_current
  
  # add the Hastings correction to our acceptance probability if we used an
  # unequal proposal probability
  if (delta == 1) {
    hastings_ratio <- down_up_prob[1] / down_up_prob[2]
  } else {
    hastings_ratio <- down_up_prob[2] / down_up_prob[1]
  }
  prob_accept <- prob_accept * hastings_ratio
  
  # draw whether to accept/reject the proposed move
  if (runif(1) < prob_accept) {
    x[i] <- x_propose
  } else {
    x[i] <- x[i-1]
  }
  
} # end MCMC loop

plot(x, type = 'l')


# calculate the proportion of time spent in each position and compare this with
# our target distribution
freq <- tabulate(x + 1, nbins = 21) / reps

barplot(freq, space = 0)
points(0:20 + 0.5, target_distribution(0:20), pch = 20)

