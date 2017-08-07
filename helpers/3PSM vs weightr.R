source("../0-start.R", chdir=TRUE)
library(weightr)


## ======================================================================
## Test the 3PSM (compare with McShane)
## ======================================================================


set.seed(0xBEEF)

res <- data.frame()

for (i in 1:100) {
	print(i)
	dat <- data.frame(dataMA(k = 100, delta = 0.3, tau = 0.2, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0.7, qrpEnv = "none"))

	t1 <- estimate.onestep.selection.heterogeneous(z.obs=dat$t, n1=dat$n1, n2=dat$n2,
	                                         alpha=0.05/2,
	                                         theta.init=c(expected.d = 0.3, max.tau= 0.5, p.report = 0.99))
	#t1 <- TPSM.McShane.est(t=dat$t, n1=dat$n1, n2=dat$n2, long=FALSE)

	w1 <- weightfunct(dat$d, dat$v, steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE, table = TRUE)
	kSig <- sum(dat$p < .05)
	
	res <- rbind(res, data.frame(
		t(c(t1$est, w1[[2]]$par[c(2, 1, 3)], kSig, t1$ll, w1[[2]]$value))
	))
}
colnames(res) <- c("t.est", "t.tau", "t.p", "w.est", "w.tau", "w.p", "kSig", "t.ll", "w.ll")
res$w.tau <- sqrt(res$w.tau)

# estimate
plot(res$t.est, res$w.est, xlab="McShane estimate", ylab="weightr estimate")
abline(a=0, b=1, lty="dotted")

mean(res$t.est)
mean(res$w.est)

# tau
plot(res$t.tau, res$w.tau, xlab="McShane tau", ylab="weightr tau")
abline(a=0, b=1, lty="dotted")

mean(res$t.tau)
mean(res$w.tau)


# step-p
library(ggplot2)
ggplot(res, aes(x=t.p, y=w.p, color=kSig)) + geom_point() + xlab("McShane step p") + ylab("weightr step p")
 + geom_abline()

mean(res$t.p)
mean(res$w.p)

library(lattice)
splom(~res[, 1:6], scales=list(cex=0.5))
pairs(res[, 1:6])



## ======================================================================
## Test the 4PSM (compare with McShane)
## --> use another censoring function
## ======================================================================


set.seed(0xBEEF)

res <- data.frame()

for (i in 1:1000) {
	print(i)
	dat <- data.frame(dataMA(k = 30, delta = 0.2, tau = 0.3, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0, qrpEnv = "none"))
	dat$p.onesided <- pt(dat$t, dat$N-2, lower.tail=FALSE)

	# three batches: (A) sig (correct direction), (B) non-sig (correct dir), (C) wrong direction
	A <- dat[dat$p.onesided < .025, ]
	B <- dat[dat$p.onesided > .025 & dat$d > 0, ]
	C <- dat[dat$d < 0, ]
	
	
	dat.PB <- A
	dat.PB <- rbind(dat.PB, B[runif(nrow(B)) < .35, ])	# 35% chance to enter a non-sig, directionally consistent
	dat.PB <- rbind(dat.PB, C[runif(nrow(C)) < .1, ])   # 10% chance to enter a directionally inconsistent
	
	t1 <- TPSM.McShane.est(t=dat.PB$t, n1=dat.PB$n1, n2=dat.PB$n2, long=FALSE)
	w1 <- weightfunct(dat.PB$d, dat.PB$v, steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE, table=TRUE)
	w2 <- weightfunct(dat.PB$d, dat.PB$v, steps = c(0.025, 0.5, 1), mods = NULL, weights = NULL, fe = FALSE, table=TRUE)
	
	res <- rbind(res, data.frame(
		t(c(t1[c(1, 6, 11), "value"], w1[[2]]$par[c(2, 1, 3)], w2[[2]]$par[c(2, 1, 3, 4)]))
	))
}
colnames(res) <- c("t.est", "t.tau", "t.p", "w.est", "w.tau", "w.p", "w2.est", "w2.tau", "w2.p1", "w2.p2")
res$w.tau <- sqrt(res$w.tau)
res$w2.tau <- sqrt(res$w2.tau)

# estimate
plot(res$t.est, res$w.est, xlab="McShane estimate", ylab="weightr estimate")
abline(a=0, b=1, lty="dotted")

mean(res$t.est)
mean(res$w.est)
mean(res$w2.est)

# tau
plot(res$t.tau, res$w.tau, xlab="McShane tau", ylab="weightr tau")
abline(a=0, b=1, lty="dotted")

mean(res$t.tau)
mean(res$w.tau)
mean(res$w2.tau)


# step-p
plot(res$t.p, res$w.p, xlab="McShane step p", ylab="weightr step p")
abline(a=0, b=1, lty="dotted")

mean(res$t.p)
mean(res$w.p)
mean(res$w2.p1)
mean(res$w2.p2)


## ======================================================================
## Test some boundary cases for weightr
## ======================================================================

library(weightr)
weightfunct(c(0.8, 0.4, 0.4, 0.5, 0.3), c(0.04, 0.04, 0.02, 0.02, 0.01), steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE, table = TRUE)

weightfunct(c(0.8, 0.4, 0.4, 0.5, 0.3), c(0.04, 0.04, 0.02, 0.02, 0.01), steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE, table = TRUE)