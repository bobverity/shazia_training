# 06.hill_climbing_fails.R
#
# Author: Bob Verity
# Date: 2024-06-21
#
# Inputs: (none)
#
# Outputs: (none)
#
# Purpose:
# An example in which the likelihood surface is complex, containing multiple
# local peaks. The hill climbing algorithm tends to get stuck in these peaks,
# meaning we must run it many times to try and find the best global estimate.
# Even then, it doesn't do a great job. Demonstrates some of the weaknesses of
# simply hill-climbing based ML estimation.
#
# ------------------------------------------------------------------

set.seed(3)

# assume we are looking at something like rainfall data, which has a seasonal
# component but is also noisy. We will assume a model of the form y =
# A*sin(Bx+C) + D + eps, where eps is normally distributed error with standard
# deviation sigma. Lets set the true values of all of these coefficients:
n <- 12
A <- 1
B <- 2
C <- pi / 4
D <- 1
sigma <- 1

# simulate data at a range of values of x
x <- seq(0, 2*pi, l = n)
y <- A*sin(B*x + C) + D + rnorm(n, sd = sigma)

# NB, re-run repeatedly from this point on if you want to test how often the ML
# approach gets it right. You do not want to re-run the lines above this as that
# resets the seed so you'll get the same answer every time.

plot(x, y)

# lets also overlay the true model
x_vec <- seq(0, 2*pi, l = 1001)
y_true <- A*sin(B*x_vec + C) + D
lines(x_vec, y_true)

# ------------------------------

# we want to estimate all four coefficients via maximum likelihood using optim.
# We will do this repeatedly, reps times, each time initialising from a
# different starting position. We will store the final loglikelihood (the height
# of the peak) reached in each run
reps <- 1e3
theta_list <- list()
final_loglike <- rep(NA, reps)
for (i in 1:reps) {
  theta_opt <- optim(runif(4, 0, 10), fn = function(theta) {
    A <- theta[1]
    B <- theta[2]
    C <- theta[3]
    D <- theta[4]
    -sum(dnorm(y, mean = A*sin(B*x + C) + D, sd = sigma, log = TRUE))
  })
  theta_list[[i]] <- theta_opt
  final_loglike[i] <- -theta_opt$value
}

# find the highest loglike and the corresponding parameter estimates
w <- which.max(final_loglike)

theta_opt <- theta_list[[w]]

A_fit <- theta_opt$par[1]
B_fit <- theta_opt$par[2]
C_fit <- theta_opt$par[3]
D_fit <- theta_opt$par[4]

# overlay this fitted curve. Often this will not match the true curve
y_fit <- A_fit*sin(B_fit*x_vec + C_fit) + D_fit
lines(x_vec, y_fit, col = 2)
