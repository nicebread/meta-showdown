library(ggplot2)
library(rio)
library(dplyr)

## ======================================================================
## Read Fraley & Vazire n-pact data set
## 
## Fraley, R. C., & Vazire, S. (2014). The N-Pact Factor: Evaluating the Quality of Empirical Journals with Respect to Sample Size and Statistical Power. PLoS ONE, 9(10), e109019. http://doi.org/10.1371/journal.pone.0109019.t005
## https://osf.io/7im3n/
## ======================================================================

dat <- read.csv("npact dat.csv")
summary(dat)

# extract the sample sizes (for full sample)
FV.ns0 <- round(dat$Avg_Coder_N[!is.na(dat$Avg_Coder_N)])
summary(FV.ns)

# remove most extreme outlier (2.5% quantiles)
Q <- quantile(FV.ns0, prob=c(.025, .975))
Q
FV.ns <- FV.ns0[FV.ns0 >= Q[1] & FV.ns0 <= Q[2]]

# These are sample sizes for the whole sample - divide by two to approximate per-group sample sizes
FV.ns.perGroup <- round(FV.ns/2)
summary(FV.ns.perGroup)


## ======================================================================
## Read Uli Schimmack's data set with PsychScience degrees of freedom
## 
## http://www.utstat.toronto.edu/~brunner/data/power/PsychScience.urn3.txt
# See also https://replicationindex.wordpress.com/2018/05/10/an-even-better-p-curve
## ======================================================================

US.ns <- read.table("Schimmack-PsychScience.urn3.txt", header=FALSE, sep=" ") %>% unlist() %>% as.vector()

# These are sample sizes for the whole sample - divide by two to approximate per-group sample sizes (as in Fraley and Vazire)
US.ns.perGroup <- US.ns/2

summary(US)


## ======================================================================
## Read Marszalek data set. Cite as:

# Marszalek, J. M. (2011). Sample size in psychological research over the past 30 years [Data file]. Retrieved from https://mospace.umsystem.edu/xmlui/handle/10355/62220
#
# Marszalek, J. M., Barber, C., Kohlhart, J., & Holmes, C. B. (2011). Sample size in psychological research over the past 30 years. Perceptual and Motor Skills, 112(2), 331-348. doi: 10.2466/03.11.PMS.112.2.331-348
## ======================================================================

M.ns <- import("MarszalekSamSizPsy-Excel.xls")

# pull out the first column of per-group sample sizes (not the others, because they are not independent from the first)

M.n.all.groups0 <- M.ns %>% select(contains("n_")) %>% unlist %>% as.numeric %>% na.omit
M.n.first.group0 <- M.ns %>% pull("n_A") %>% as.numeric() %>% na.omit %>% as.vector

summary(M.n.first.group0)
summary(M.n.all.groups0)

# remove most extreme outlier (1% quantiles)
(Q <- quantile(M.n.first.group0, prob=c(.025, .975)))
M.n.first.group <- M.n.first.group0[M.n.first.group0 >= Q[1] & M.n.first.group0 <= Q[2]]

(Q <- quantile(M.n.all.groups0, prob=c(.025, .975)))
M.n.all.groups <- M.n.all.groups0[M.n.all.groups0 >= Q[1] & M.n.all.groups0 <= Q[2]]

summary(M.n.first.group)
summary(M.n.all.groups)

## ======================================================================
## find the best fitting curve for all samples
## ======================================================================

fitCurve <- function(x, x.max=max(x)) {
	
	library(fitdistrplus)
	library(invgamma)

	x <- x[x <= x.max]

	lnorm <- fitdist(x, "lnorm")
	nbinom <- fitdist(x, "nbinom")
	gamma <- fitdist(x, "gamma")
	invgamma <- fitdist(x, "invgamma")
	weibull <- fitdist(x, "weibull")
	
	print(summary(lnorm)); print(summary(nbinom)); print(summary(gamma)); print(summary(invgamma)); print(summary(weibull));

	# overlay histogram and normal density

	round_dnbinom <- function(x, ...) dnbinom(round(x), ...)

	p1 <- ggplot(data.frame(x=x), aes(x=x)) + theme_bw() +
	  geom_histogram(aes(y = ..density..), binwidth=7) +
		stat_function(fun = round_dnbinom, args = list(size = nbinom$estimate["size"], mu = nbinom$estimate["mu"]), lwd = 1.5, col = 'red') + 
		stat_function(fun = dlnorm, args = list(meanlog = lnorm$estimate["meanlog"], sdlog = lnorm$estimate["sdlog"]), lwd = 1.5, col = 'green') + 
		stat_function(fun = dinvgamma, args = list(shape = invgamma$estimate["shape"], scale = invgamma$estimate["scale"]), lwd = 1.5, col = 'blue') +
		#scale_colour_manual(name="Fitting function", breaks=c("red", "green", "blue"), labels=c("Negative binomial", "gamma", "inverse gamma")) +
		coord_cartesian(xlim=c(1, 400))+ 
		xlab("Per group sample size") + ylab("Density")
		
	return(p1)	
}

fitCurve(FV.ns.perGroup)
fitCurve(M.n.first.group)
fitCurve(M.n.all.groups)

## ======================================================================
## Combine data sets
## ======================================================================
	
summary(c(FV.ns.perGroup, M.n.all.groups))
fitCurve(c(FV.ns.perGroup, M.n.all.groups))


# ---------------------------------------------------------------------
# Simulate sample sizes from inverse gamma distribution
# size = dispersion parameter

n.sim0 <- round(rinvgamma(100000, shape=1.51, scale=0.034))
summary(n.sim0)
(Q <- quantile(n.sim0, prob=c(.01, .99)))
n.sim <- n.sim0[n.sim0 >= Q[1] & n.sim0 <= Q[2]]

summary(n.sim)

hist(n.sim, probability=TRUE)
x <- seq(0, 500, by=1)
lines(x, dinvgamma(x, shape=1.51, scale=0.034), type="l", col="red")