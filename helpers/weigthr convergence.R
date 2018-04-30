# Test computation with empty p-value bins

hist(rgamma(10000, shape=3, scale=0.04), xlim=c(0, 1), breaks=100)

d <- rnorm(50, 0.8, 0.1)
v <- rgamma(50, shape=3, scale=0.04)

# quick and dirty
d.sig <- d[d/v > 2.2]
v.sig <- v[d/v > 2.2]

library(weightr)

# all non-significant: no convergence
weightfunct(effect=c(0.4, 0.5, 0.43, 0.6), v=c(0.3, 0.2, 0.5, 0.1), steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE, table = TRUE)

# all significant: convergence
weightfunct(effect=c(0.4, 0.5, 0.43, 0.6), v=c(0.01, 0.01, 0.01, 0.01), steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE, table = TRUE)

# convergence
weightfunct(effect=c(0.4, 0.5, 0.43, 0.6), v=c(0.01, 0.01, 0.01, 0.01), steps = c(0.025, 1), mods = NULL, fe = FALSE, table = TRUE, weights = c(1, .01))


dat <- simMA(k = 100, delta = 0.6, tau = 0.1, empN = TRUE, maxN=500, minN=0, meanN=0, censor="high")

weightfunct(effect=dat$d, v=dat$v, steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE, table = TRUE)


# ---------------------------------------------------------------------
# Three tests

ds <- c(0.8, 0.7, 0.73, 0.76, 0.8, 0.7, 0.73, 0.76)

# significant: converges
weightfunct(effect=ds, v=(ds/2)^2, steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE, table = TRUE)

# all clearly non-significant: converges
weightfunct(effect=ds, v=(ds*2)^2, steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE, table = TRUE)

# in between: no convergence
w <- weightfunct(effect=ds, v=(ds/1.2)^2, steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE, table = TRUE)

threePSM.est(ds, v=(ds/1.2)^2, min.pvalues=0, long=TRUE)

threePSM.est(ds, v=(ds/2)^2, min.pvalues=1, long=TRUE)