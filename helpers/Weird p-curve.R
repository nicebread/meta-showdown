source("../0-start.R", chdir=TRUE)

# ---------------------------------------------------------------------
#  Inverstigating Joe's "weird p-curve" issue

load("../dataFiles/summ.RData")
filter(summ, delta == 0, tau == 0, censor == "med", k == 100, method == "pcurve")

# check the raw estimates
load(file="../dataFiles/res.wide.red.RData")

pcurve84 <- res.wide.red %>% filter(condition==84, method == "pcurve")
summary(pcurve84$b0_estimate)

# OK, there are some outliers:
pcurve84 %>% filter(b0_estimate > 1)

outliers <- pcurve84 %>% filter(b0_estimate > 1) %>% pull("id")

# load raw data of these three conditions
load(file="../simPartsRev2/simData_condition_84.RData")

sim.outlier <- sim %>% filter(id == outliers[1])

pcurveEst(t=sim.outlier$t, df=sim.outlier$n1 + sim.outlier$n2 - 2, progress=FALSE, long=TRUE, CI=FALSE)
puniformEst(t.value=sim.outlier$t, n1=sim.outlier$n1, n2=sim.outlier$n2, skipBarelySignificant=TRUE)