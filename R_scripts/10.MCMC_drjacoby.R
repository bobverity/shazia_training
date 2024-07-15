# 10.MCMC_drjacoby.R
#
# Author: Bob Verity
# Date: 2024-07-15
#
# Inputs: data/linelist.rds
#
# Outputs: (none)
#
# Purpose:
# Example of using drjacoby to fit a gamma delay distribution between onset of
# symptoms and recovery from simulated line list data
#
# ------------------------------------------------------------------

library(tidyverse)
#devtools::install_github("mrc-ide/drjacoby@v1.5.4")
library(drjacoby)

# -----------------------------

# read in linelist data
dat <- readRDS("data/linelist.rds")

# filter to mild cases (not hospitalized) and extract number of days between
# onset of symptoms and recovery
days_ill <- dat |>
  filter(is.na(Date_hospitalized),
         !is.na(Date_recovery)) |>
  mutate(days_ill = Date_recovery - Date_onset) |>
  pull(days_ill)

# have a look
hist(days_ill, breaks = seq(0, 20, 0.5))

# -----------------------------

# data must be in a list (or data.frame)
data_list <- list(x = days_ill)

# make parameters in data.frame
# this MUST have columns name, min and max
df_params <- define_params(name = "alpha", min = 0, max = 100,
                           name = "beta", min = 0, max = 20)

# write a LOG-likelihood function
# params is a named vector, with names taken from your parameters data.frame
# data is a list
# misc is a list (often not used)
r_loglike <- function(params, data, misc) {
  
  # unpack parameters
  alpha <- params["alpha"]
  beta <- params["beta"]
  
  sum(dgamma(data$x, shape = alpha, rate = beta, log = TRUE))
}

# write a LOG-prior function
r_logprior <- function(params, misc) {
  
  # unpack parameters
  alpha <- params["alpha"]
  beta <- params["beta"]
  
  dunif(alpha, min = 0, max = 100, log = TRUE) +
    dunif(beta, min = 0, max = 20, log = TRUE)
}

# run the MCMC
set.seed(1)
mcmc <- run_mcmc(data = data_list,
                 df_params = df_params,
                 loglike = r_loglike,
                 logprior = r_logprior,
                 burnin = 1e3,
                 samples = 1e4,
                 chains = 5)

# check diagnostics. We are looking for rhat less than 1.1, and good ESS for all
# parameters
mcmc$diagnostics

# some example plots
plot_trace(mcmc, show = "alpha", phase = "both")

plot_density(mcmc)

plot_credible(mcmc)

plot_scatter(mcmc, parameter1 = "alpha", parameter2 = "beta")

plot_pairs(mcmc)

plot_cor_mat(mcmc)

# -----------------------------

# extract the posterior mean from the output
post_mean <- mcmc$output |>
  filter(phase == "sampling") |>
  select(alpha, beta) |>
  colMeans()

# note that the true values used when generating these data were:
# alpha = 25
# beta = 2.5
post_mean

# overlay this model fit against the data distribution
x_vec <- seq(0, 20, l = 101)
fx <- dgamma(x_vec, shape = post_mean[1], rate = post_mean[2])
hist(days_ill, breaks = seq(0, 20, 0.5))
lines(x_vec, fx*1e2)
