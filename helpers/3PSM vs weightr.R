source("../0-start.R", chdir=TRUE)

# Typical 3PSM behavior
set.seed(0xBEEF)
dat <- data.frame(dataMA(k = 100, delta = 0.2, tau = 0.2, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0.6, qrpEnv = "none"))

mm <- estimate.onestep.selection.heterogeneous(z.obs=dat$t, n1=dat$n1, n2=dat$n2, 
                                         alpha=0.05/2, 
                                         theta.init=c(expected.d = 0.3, max.tau= 0.5, p.report = 0.99))
mm
TPSM.est(t=dat$t, n1=dat$n1, n2=dat$n2)

library(weightr)

weightfunct(dat$d, dat$v, steps = c(0.025, 0.5, 1), mods = NULL, weights = NULL, fe = FALSE, table = TRUE)