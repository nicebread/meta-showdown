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
dat <- dataMA(k = 100, delta = 0.15, tau = 0.1, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0.9, qrpEnv = "high")
dat <- data.frame(dat)

library(meta)
(meta1 <- metagen(dat$d, dat$se))
meta::funnel(meta1, ref=0, xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))
meta::funnel(meta1, ref=0, contour=c(0.9, 0.95), xlab="Effect size", cex=.5, pch=19, xlim=c(-1.5, 1.5))
