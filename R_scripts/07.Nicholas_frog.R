# 07.Nicholas_frog.R
#
# Author: Bob Verity
# Date: 2024-07-15
#
# Inputs: (none)
#
# Outputs: (none)
#
# Purpose:
# Imagine a frog, Nicholas, who spends all day hopping between two lily pads.
# Each hour, there is a probability that he moves from right to left, or from
# left to right. The question is, over the course of many hours, how much time
# would he spend on each of the two lily pads?
# 
# (Demonstrates the basic logic that underpins the Metropolis algorithm)
#
# ------------------------------------------------------------------

# what is the probability of Nicholas moving right or left on a given hour
R <- 0.05
L <- 0.02
n_hours <- 1e4

# keep track of which pad Nicholas is on each hour
pos <- rep(NA, n_hours)
pos[1] <- 1

# draw position each hour
for (i in 2:n_hours) {
  
  if (pos[i-1] == 1) {  # if on the first pad, draw whether to move right
    if (rbinom(1, 1, prob = R)) {
      pos[i] <- 2
    } else {
      pos[i] <- 1
    }
  } else {  # if on the second pad, draw whether to move left
    if (rbinom(1, 1, prob = L)) {
      pos[i] <- 1
    } else {
      pos[i] <- 2
    }
  }
}

plot(pos, type = 'l')

# how much time is spent on each pad
freq <- tabulate(pos) / n_hours
barplot(freq, space = 0, names.arg = 1:2, ylim = c(0, 1))


# observed and predicted: relative time on pad 1
freq[1]
L / (L + R)

# observed and predicted: how much higher is pad 2 than pad 1
freq[2] / freq[1]
R / L

# this is the main observation - that the time spent on one lily pad vs. another
# is equal to the relative flow in vs. out
