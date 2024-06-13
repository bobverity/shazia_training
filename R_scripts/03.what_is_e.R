
# what_is_e.R
#
# Author: Bob Verity
# Date: 2024-06-13
#
# Inputs: (none)
#
# Outputs: (none)
#
# Purpose:
# Where does the exponential constant "e" come from? Let's explore raising a
# constant "a" to the power x. What happens to the gradient of this function as
# we change the value of "a". Can we choose a value such that the gradient looks
# like the original function itself? What is the gradient of this function as it
# passes through zero? The answer to all these questions.....is e.
#
# ------------------------------------------------------------------

# play with different values of this constant
a <- 2.3

# raise to the power x
x <- seq(-2, 8, l = 1001)

fx <- a^x

# calculate the gradient
grad <- diff(fx) / diff(x)

# plot and compare lines
plot(x, type = 'l', fx, ylim = c(0, 100))
abline(h = 0, v = 0, lty = 2)
lines(x[1:length(grad)], grad, col = 2)

# gradient at 0
(fx[22] - fx[21]) / (x[22] - x[21])
