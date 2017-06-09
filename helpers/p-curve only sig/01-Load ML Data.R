source("../../0-start.R", chdir=TRUE)

# Many Labs Replication
library(dplyr)
library(rio)
d0 <- import("ML1_data/Data/CleanedDataset.sav")



# load qwuote attribution
d0 <- d0[d0$us_or_international==1, ]
df <- data.frame(
	condition = d0$quoteGroup,
	DV = d0$quote,
	group = d0$referrer
)



# load flag priming

df <- data.frame(
	condition = d0$flagGroup[d0$flagfilter==1],
	DV = d0$flagdv[d0$flagfilter==1],
	group = d0$referrer[d0$flagfilter==1]
)


# name <- "Flag Priming"
# D[[name]]$label <- name
# D[[name]]$d.original <- 0.50
# D[[name]]$d.original.lower <- 0.01
# D[[name]]$d.original.lower80 <- t2(D[[name]]$d.original, D[[name]]$d.original.lower)[[1]]
# D[[name]]$d.final <- 0.03
# D[[name]]$X <- na.omit(d0$flagdv[d0$flagGroup==levels(d0$flagGroup)[2] & d0$flagfilter=="include"])
# D[[name]]$Y <- na.omit(d0$flagdv[d0$flagGroup==levels(d0$flagGroup)[1] & d0$flagfilter=="include"])
# (D[[name]]$t0 <- t.test(D[[name]]$X, D[[name]]$Y, var.equal=TRUE))
# D[[name]]$d.pooled <- tes(D[[name]]$t0$statistic, length(D[[name]]$X), length(D[[name]]$Y), level=99)$d



# compute t-test within each sample
ds <- data.frame()
for (i in unique(df$group)) {
	s <- df[df$group == i, ]
	t0 <- t.test(s$DV[s$condition==1], s$DV[s$condition==0])
	d0 <- tes(t0$statistic, n.1=table(s$condition)[1], n.2=table(s$condition)[2], verbose=FALSE, dig=10)
	ds <- rbind(ds, data.frame(
		location = i,
		d = d0$d,
		d.var = d0$var.d,
		t = t0$statistic,
		df = t0$parameter,
		n1 = table(s$condition)[1],
		n2 = table(s$condition)[2],
		p.value = t0$p.value
	))
}

# meta-analytic integration
library(metafor)
r1 <- rma(yi=ds$d, vi=ds$d.var)
r1

metafor::funnel(r1, pch=ifelse(ds$p.value < .05, 1, 2))


# ---------------------------------------------------------------------
# p-curve
pcurveEst(t=ds$t, df=ds$df, progress=FALSE, long=TRUE, CI=FALSE)
TPSM.est(t=ds$t, n1=ds$n1, n2=ds$n2, long=TRUE)





# ---------------------------------------------------------------------
# Moving target:
# Code adapted from Uri Simonsohn

set.seed(0xBEEF)

DELTA <- 0
TAU <- 0.2

res <- data.frame()
for (n in seq(30, 5000, by=50)) {

	print(n)
	
	#All possible designs
	d=rnorm(1000, mean=DELTA, sd=TAU)  #true mean is .2, but Uri says this does not exist.

	#Studies
	#Generate one study from each true design
	#n=20 #Not that it matters
	t.obs=mapply(rt,1,df=2*n-2,ncp=sqrt(n/2)*d)  		# note:ncp is sqrt(n/2)*d, so this takes each element of d.pos and draws an estimate at random
	p.obs=2*(1-pt(abs(t.obs),df=2*n-2))             # p-value of run studies
	t.sig=t.obs[p.obs<.05]
	d.sig=2*t.sig/sqrt(2*n)                          #observed effect size (biased upwards)
	
	true.d.sig=d[p.obs<.05 & d > 0]  #True effect of observed and directionally consistent studies
	
	# actual p-curve estimate
	PC.est <- pcurveEst(t=t.sig, df=2*n-2, progress=FALSE, long=TRUE, CI=FALSE)
	
	# ignore negative estimates (they are not plausible):
	PC.est[1, "value"][PC.est[1, "value"] < 0] <- NA
	
	# 3PSM estimate
	TPSM <- TPSM.est(t=t.obs, n1=rep(n, length(t.obs)), n2=rep(n, length(t.obs)), long=TRUE)
	
	# 1PSM estimate: unstable, often not identified
	# onePSM <- NA
# 	try({
# 		onePSM <- estimate.extreme.selection.homogeneous(t.obs=t.obs, n1=rep(n, length(t.obs)), n2=rep(n, length(t.obs)), alpha=0.025, interval=c(-1, 2))[1]
# 	})
	
	res <- rbind(res, data.frame(
		n = n, 
		d.sig.obs.mean = mean(d.sig),	#Observed effect among significant studies, biased upwards
		d.sig.true.mean = mean(true.d.sig),   #True effect of observed studies (what we want to estimate and we believe p-curve estimates correctly)
		d.mean = mean(d),
		PC.est = PC.est[1, "value"],
		TPSM.est = TPSM[1, "value"]#,
		#onePSM.est = onePSM
	))
}

plot(res$n, res$d.sig.true.mean, col="blue", type="l", ylim=c(0, .50), ylab="d", xlab="Sample size in each primary study")
lines(res$n, res$PC.est, col="red")
lines(res$n, res$TPSM.est, col="darkgreen")
legend("topright", lty=c("solid", "solid", "solid", "dotted"), col=c("blue", "red", "darkgreen", "black"), legend=c("Mean of sign. studies (p-curve's estimand)", "Meta-analytic p-curve estimate", "Meta-analytic 3PSM estimate", "true mean of all studies"))
abline(h = DELTA, lty="dotted")










# ---------------------------------------------------------------------
# Check bias of pcurve
# Code adapted from Uri Simonsohn

set.seed(0xBEEF)

DELTA <- 0
TAU <- 0
K <- 30
n <- 80

res <- data.frame()
for (i in 1:1000) {

	print(i)
	
	#All possible designs
	d=rnorm(1000, mean=DELTA, sd=TAU)  #true mean is .2, but Uri says this does not exist.

	#Studies
	#Generate one study from each true design

	t.obs=mapply(rt,1,df=2*n-2,ncp=sqrt(n/2)*d)  		# note:ncp is sqrt(n/2)*d, so this takes each element of d.pos and draws an estimate at random
	p.obs=2*(1-pt(abs(t.obs),df=2*n-2))             # p-value of run studies
	t.sig=t.obs[p.obs<.05]
	d.sig=2*t.sig/sqrt(2*n)                          #observed effect size (biased upwards)
	
	true.d.sig=d[p.obs<.05 & d > 0]  #True effect of observed and directionally consistent studies
	
	# actual p-curve estimate
	PC.est <- pcurveEst(t=t.obs[1:K], df=2*n-2, progress=FALSE, long=TRUE, CI=FALSE)
	
	# ignore negative estimates (they are not plausible):
	#PC.est[1, "value"][PC.est[1, "value"] < 0] <- NA
	
	# 3PSM estimate
	TPSM <- TPSM.est(t=t.obs[1:K], n1=rep(n, K), n2=rep(n, K), long=TRUE)
	
	# 1PSM estimate: unstable, often not identified
	# onePSM <- NA
# 	try({
# 		onePSM <- estimate.extreme.selection.homogeneous(t.obs=t.obs, n1=rep(n, length(t.obs)), n2=rep(n, length(t.obs)), alpha=0.025, interval=c(-1, 2))[1]
# 	})
	
	res <- rbind(res, data.frame(
		n = n, 
		d.sig.obs.mean = mean(d.sig),	#Observed effect among significant studies, biased upwards
		d.sig.true.mean = mean(true.d.sig),   #True effect of observed studies (what we want to estimate and we believe p-curve estimates correctly)
		d.mean = mean(d),
		PC.est = PC.est[1, "value"],
		PC.kSig = PC.est[4, "value"],
		TPSM.est = TPSM[1, "value"]#,
		#onePSM.est = onePSM
	))
}

summary(res$PC.est)
plot(res$PC.est, res$PC.kSig)

plot(res$n, res$d.sig.true.mean, col="blue", type="l", ylim=c(0, .50), ylab="d", xlab="Sample size in each primary study")
lines(res$n, res$PC.est, col="red")
lines(res$n, res$TPSM.est, col="darkgreen")
legend("topright", lty=c("solid", "solid", "solid", "dotted"), col=c("blue", "red", "darkgreen", "black"), legend=c("Mean of sign. studies (p-curve's estimand)", "Meta-analytic p-curve estimate", "Meta-analytic 3PSM estimate", "true mean of all studies"))
abline(h = DELTA, lty="dotted")