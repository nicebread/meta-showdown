source("../start.R", chdir=TRUE)

# ---------------------------------------------------------------------
#  check Citkowicz own data set
# Gives the same results as in the paper.
d <- c(1.01, 0.82, 0.59, 0.44, 0.84, 0.73, 1.12, 0.04, 0.24, 0.32, 1.04, 1.31, 0.59, 0.66, 0.62, 0.47, 1.08, 0.98, 0.26, 0.39, 0.60, 0.94, 0.11)
v <- c(0.2704, 0.2116, 0.0529, 0.0324, 0.0841, 0.0841, 0.1296, 0.1369, 0.0225, 0.1600, 0.1024, 0.3249, 0.0841, 0.0361, 0.0961, 0.0729, 0.1024, 0.1024, 0.0324, 0.0324, 0.0961, 0.2809, 0.0729)

RMA.est(d=d, v=v)
betaSM.est(d=d, v=v, full=TRUE)
betaSM.est(d=d, v=v, full=FALSE)

# ---------------------------------------------------------------------
#  check several simulated data sets
for (i in 1:100) {
	set.seed(i)
	dat <- dataMA(k = 10, delta = 0.8, tau = 0, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0, qrpEnv = "none")
	dat <- data.frame(dat)
	res <- betaSM.est(d=dat$d, v=dat$v, full=TRUE)
	print(paste0(i, ";", res[1, ]))
}
	
TPSM.est(t=dat$t, n1=dat$n1, n2=dat$n2)
RMA.est(d=dat$d, v=dat$v)

