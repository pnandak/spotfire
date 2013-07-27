# Lecture 7: Tobit model


## 1. Motivation
# Censored variables
# We call a variable “censored” when we can only observe the latent variable in a
# certain range.
#     • hours worked (negative amounts can not be observed)
#     • income (only when relevant for social incurance)
#     • bids in an auction (often only the second highest bid is observed)


## 2. Example
# Let us generate a simple censored variable:
  library(survival)
  set.seed(123)
  n = 120
  sd = 2
  x = sort(runif(n))
  y = x + sd * rnorm(n)
  ycens = y
  ycens[y <= 0] = 0
  ycens[y >= 1] = 1

# plot(y ~ x, col="grey")
# points(ycens[y <= 0] ~ x[y <= 0], col="red")
# points(ycens[y >= 1] ~ x[y >= 1], col="red")
# points(ycens[y >= 0 & y <= 1] ~ x[y >= 0 & y <= 1])
# abline(a=0, b=1)

# As with the logit model we can try to use simple OLS to estimate the
# relationship:
  olsall = lm(ycens ~ x); olsall

# This result is not too convincing, perhaps we should only look at the uncensored
# observations:
  olsrel = lm(ycens ~ x, subset = (ycens > 0 & ycens < 1)); olsrel

# The left part of the following picture shows the true relation as well as the two
# estimates:
par(mfrow = c(1, 3))
plot(ycens ~ x)
abline(a = 0, b = 1, lwd = 3, col = "blue")
abline(olsall, lty = 2, col = 2, lwd = 3)
abline(olsrel, lty = 3, col = "red", lwd = 3)
legend("topleft", c("true", "OLS", "OLS not censored"),
    lty = 1:3, col = c("blue", "red", "red"), bg = "white")
plot(ycens - x ~ x, main = "true residuals", ylab = "ycens-E(y)")
abline(h = 0)
lines(predict(olsall) - x ~ x, lty = 2, col = 2, lwd = 3)
legend("topright", c("true", "OLS"), col = 1:2, lty = 1:2)
plot(olsall, which = 1, main = "estimated residuals")

# The graph in the middle shows the true residuals, the graph on the right shows
# the estimated residuals. While the estimated residuals underestimate the
# problem, it is still visible: E(u|X) != 0 for most X.


## 3. Solution
# Interval regression
# As in the logistic case we use maximum likelihood. The procedure is called
# “interval regression” since the dependent variable is an interval.
#    • [y, y] — known observation
#    • [y, ∞] — observation is larger than y
#    • [∞, y] — observation is smaller than y
#    • [y1, y2] — observation is between y1 and y2

# In R we use missings (NA) to indicate ∞.
  ymin = ycens
  ymax = ycens
  ymin[ycens == 0] = NA
  ymax[ycens == 1] = NA
  intreg = survreg(Surv(ymin, ymax, type = "interval2") ~
      x, dist = "gaussian")
  intreg
 
 library(VGAM)
 ?tobit
 intreg2 <- vglm(ycens ~ x, tobit(Lower=0, Upper=1))
 intreg2
 coef(intreg2, matrix=TRUE)


# The estimated β is not perfect, but much better then the naïve OLS estimates
# above. Let us show the esimated regression line in a graph:

par(mfrow = c(1, 2))
plot(ycens ~ x)
abline(a = 0, b = 1, lwd = 3, col = "blue")
abline(olsall, olsrel, lty = 2, col = "red", lwd = 3)
abline(olsrel, lty = 3, col = "red", lwd = 3)
abline(intreg, lty = 4, col = "green", lwd = 3)
legend("topleft", c("true", "OLS", "OLS not censored",
    "interval"), lty = 1:4, col = c("blue", "red", "red",
    "green"), bg = "white")
plot(ycens - x ~ x, main = "true residuals", ylab = "ycens-E(y)")
abline(h = 0)
lines(predict(olsall) - x ~ x, lty = 2, col = 2, lwd = 3)
lines(predict(intreg) - x ~ x, lty = 3, col = 3, lwd = 3)
legend("topright", c("true", "OLS", "interval"), col = 1:3,
    lty = 1:3)

# The graph on the right side of the figure shows again the true residuals, i.e.
# y − E(y). We should note two things:
#   • The OLS estimate typically underestimates the relationship. Reason: The
#     extreme values are missing (censored)
#   • The Interval regression can overestimate. It is not necessarily very stable.

