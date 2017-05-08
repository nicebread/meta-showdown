#' @param k the number of studies in the MA
#' @param delta the true effect (or the average of the true effects if heterogeneity exists)
#' @param tau the SD around the true effect
#' @param empN a logical, whether to use the empirical per-group N distribution
#' @param maxN the max possible group size that could be created *this needs to be set higher than what can actually be generated--it doesn't mean you get bigger samples
#' @param minN the min of the truncated normal for sample size
#' @param meanN the average of the truncated normal for sample size
#' @param selProp the proportion of the sample affected by publication bias
#' @param qrpEnv the qrp environment that produced the literature: 'none', 'low', 'med', 'high'


# QRP environments
# none: no QRPs used
# low: 50% no QRP, 40% moderate, 10% agressive
# medium: 30% no QRP, 50% moderate, 20% agressive
# high: 10% no QRP, 40% moderate, 50% agressive
#
# Types of hacking
# none: no QRPs implemented
# moderate: optional DVs, some optional stopping (TBD, maybe 3 rounds of 2 per grp?)
# aggressive: optional DVs, optional moderators, outlier removal, heavy optional stopping (TBD, maybe 6 rounds of 2 per grp?)


# realistic n
dat <- dataMA(k = 40, delta = 0.15, tau = 0.05, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0.9, qrpEnv = "low")
dat <- data.frame(dat)

library(meta)
(meta1 <- metagen(dat$d, dat$se))
(meta2 <- rma(dat$d, dat$se))
meta::funnel(meta1, ref=0, xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))
meta::funnel(meta1, ref=0, contour=c(0.9, 0.95), xlab="Effect size", cex=.5, pch=19, xlim=c(-1, 1.5))

# plot PEt line
PET.lm <- lm(d~se, weights=1/dat$v, dat)
PEESE.lm <- lm(d~v, weights=1/dat$v, dat)

summary(PET.lm)
summary(PEESE.lm)

# plot PET-line
# (flip axis: Intercept.new = -I/Slope; Slope.new = 1/Slope
abline(a=-coef(PET.lm)[1]/coef(PET.lm)[2], b=1/coef(PET.lm)[2], col="red")

u <- par("usr")	# get range of plot coordinates
segments(coef(PET.lm)[1], 0, coef(PET.lm)[1], u[3], col="red", lty="dotted")
points(coef(PET.lm)[1], u[3], cex=1.3, col="red", pch=20)

# plot PEESE-line
Intercept.flip <- -coef(PEESE.lm)[1]/coef(PEESE.lm)[2]
Slope.flip <- 1/coef(PEESE.lm)[2]

range <- seq(0, u[3], length.out=100)
PEESE.p <- predict(PEESE.lm, newdata=data.frame(v=range))
points(0.17, 0, col="blue")
points(PEESE.p, range, col="blue")

curve(Intercept.flip + Slope.flip*(x^2), from=-2, to=2, add=TRUE, col="red")

abline(h=0, col="darkgreen", lwd=1.4)




dat <- data.frame(dataMA(k = 20, delta = 0.4, tau = 0.1, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0.6, qrpEnv = "med"))
d <- dat$d
v <- dat$v

RMA.est(d, v)
PETPEESE.est(d, v)
pc_skew(dat$t, dat$N-2)
puniformEst(dat$t, dat$n1, dat$n2)
topN(d, v, dat$n1, dat$n2, est="fixed", fixed.effect=0.3)
topN(d, v, dat$n1, dat$n2, est="rma")
topN(d, v, dat$n1, dat$n2, est="PEESE")