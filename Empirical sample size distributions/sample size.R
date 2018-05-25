library(ggplot2)
library(rio)
library(dplyr)

# trim at lower end of distribution
n.min <- 5

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
summary(FV.ns0)

# These are sample sizes for the whole sample - divide by two to approximate per-group sample sizes
FV.ns.perGroup0 <- round(FV.ns/2)

# trim lower end at n=5
FV.ns.perGroup <- FV.ns.perGroup0[FV.ns.perGroup0 >= n.min]

summary(FV.ns.perGroup)


## ======================================================================
## Read Uli Schimmack's data set with PsychScience degrees of freedom
## 
## http://www.utstat.toronto.edu/~brunner/data/power/PsychScience.urn3.txt
# See also https://replicationindex.wordpress.com/2018/05/10/an-even-better-p-curve
## ======================================================================

US.ns0 <- read.table("Schimmack-PsychScience.urn3.txt", header=FALSE, sep=" ") %>% unlist() %>% as.vector()

# These are sample sizes for the whole sample - divide by two to approximate per-group sample sizes (as in Fraley and Vazire)
US.ns.perGroup0 <- round(US.ns0/2)

US.ns.perGroup <- US.ns.perGroup0[US.ns.perGroup0 >= n.min]
summary(US.ns.perGroup)


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

# remove most extreme outliers
M.n.first.group <- M.n.first.group0[M.n.first.group0 >= n.min]
M.n.all.groups <- M.n.all.groups0[M.n.all.groups0 >= n.min]

summary(M.n.first.group)
summary(M.n.all.groups)

## ======================================================================
## find the best fitting curve for all samples
## ======================================================================

fitTruncDist <- function(x, spec, lower, upper, verbose=FALSE) {
	
	x <- x[x>= lower & x <= upper]
	
	log_likelihood <- function(params) { 		
		LL0 <- function(params) {
			d=do.call(dtrunc, append(params, list(x=x, spec=spec, a=lower, b=upper)))
			d=d[d!=0]
			return(-sum(log(d)))
		}
		
		LL <- tryCatch(LL0(params), error=function(e) return(123456789))
		if (verbose==TRUE) print(append(params, list(LL=LL)))
		return(LL)
	}
	
	# get initial untruncated fit for starting values
	fit1 <- fitdist(x, spec)	
	
	# optimize the truncated fit
	fit2 <- optim(fit1$estimate, log_likelihood)
	return(list(family=spec, estimate=fit2$par, LL=fit2$value))
}



fitCurve <- function(x, x.min=min(x), x.max=max(x)) {
	
	library(fitdistrplus)
	library(invgamma)
	library(truncdist)

	x <- x[x <= x.max & x >= x.min]

	# lnorm <- fitdist(x, "lnorm")
# 	gamma <- fitdist(x, "gamma")
# 	nbinom <- fitdist(x, "nbinom")
# 	invgamma <- fitdist(x, "invgamma")
# 	weibull <- fitdist(x, "weibull")
#
# 	print(summary(lnorm)); print(summary(nbinom)); print(summary(gamma)); print(summary(invgamma)); print(summary(weibull));

	print(invgamma <- fitTruncDist(x, spec="invgamma", lower=x.min, upper=x.max))
	print(gamma <- fitTruncDist(x, spec="gamma", lower=x.min, upper=x.max))
	print(weibull <- fitTruncDist(x, spec="weibull", lower=x.min, upper=x.max))
	print(nbinom <- fitTruncDist(x, spec="nbinom", lower=x.min, upper=x.max))
	print(lnorm <- fitTruncDist(x, spec="lnorm", lower=x.min, upper=x.max))


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

fitCurve(M.n.first.group, x.min=5, x.max=1905)
fitCurve(M.n.first.group0, x.min=5, x.max=1905)

fitCurve(M.n.all.groups, x.min=5, x.max=1905)
fitCurve(M.n.all.groups0, x.min=5, x.max=1905)

fitCurve(FV.ns.perGroup, x.min=5, x.max=1905)
fitCurve(US.ns.perGroup, x.min=5, x.max=1905)


# ---------------------------------------------------------------------
# Simulate sample sizes from truncated inverse gamma distribution
# size = dispersion parameter

n.sim <- round(rtrunc(100000, spec="invgamma", a=5, b=1905, shape=1.15326986, scale=0.04622745))
summary(n.sim)
summary(M.n.first.group)

round(quantile(n.sim, prob=c(.25, .5, .75)))
round(quantile(M.n.first.group, prob=c(.25, .5, .75)))


#hist(n.sim, probability=TRUE, breaks=200)
x.max <- 1000
hist(M.n.first.group, probability=TRUE, breaks=200, xlim=c(5, x.max), xlab="sample size", main="")
x <- seq(5, x.max, by=1)
lines(x, dtrunc(x, spec="invgamma", shape=1.15326986, scale=0.04622745, a=5, b=1905), type="l", col="red")