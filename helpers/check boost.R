source("../start.R", chdir=TRUE)
load("../simParts.boost/simData_boost_condition_225.RData")

# check some simulated MAs
MAdat <- sim[sim$id == 101225, ]
MAdat <- sim[sim$id == 102225, ]
MAdat <- sim[sim$id == 103225, ]

MAdat

e.PP <- PETPEESE.est(MAdat$d, MAdat$v, long=TRUE)
e.PP[e.PP$method=="PETPEESE.lm" & e.PP$term=="b0", ]

pcurveEst(t=MAdat$t, df=MAdat$N-2, progress=FALSE, long=TRUE, CI=FALSE)

puniformEst(t.value=MAdat$t, n1=MAdat$n1, n2=MAdat$n2)

# funnel plot
meta4 <- metagen(MAdat$d, MAdat$se)
meta::funnel(meta4, ref=0, contour=c(0.9, 0.95), xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))

# Compare with p-curve.com
cat(paste(paste0("t(", MAdat$N-2, ")=", round(MAdat$t, 4)), collapse="\n"))
