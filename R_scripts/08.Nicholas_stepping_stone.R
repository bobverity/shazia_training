# 08.Nicholas_stepping_stone.R
#
# Author: Bob Verity
# Date: 2024-07-15
#
# Inputs: (none)
#
# Outputs: (none)
#
# Purpose:
# Extends the logic of the previous script to multiple lily pads in a stepping
# stone arrangement.
#
# ------------------------------------------------------------------

# define number of pads and flows to left and right
R <- c(0.2, 0.5, 0.1, 0.15)
L <- c(0.1, 0.25, 0.2, 0.3)
n_pads <- length(R) + 1
n_hours <- 1e3

# make a migration matrix
mig_mat <- matrix(0, n_pads, n_pads)
for (i in 1:(n_pads - 1)) {
  mig_mat[i, i+1] <- R[i]
  mig_mat[i+1, i] <- L[i]
}
diag(mig_mat) <- 1 - rowSums(mig_mat)

# repeatedly draw from migration matrix
pos <- rep(NA, n_hours)
pos[1] <- 1
for (i in 2:n_hours) {
  p <- mig_mat[pos[i-1],]
  pos[i] <- sample(n_pads, 1, prob = p)
}

plot(pos, type = 'l')

# how much time is spent on each pad
freq <- tabulate(pos) / n_hours
barplot(freq, space = 0, names.arg = 1:n_pads, ylim = c(0, 1))

# explore relative times
freq[2] / freq[1]
R[1] / L[1]
