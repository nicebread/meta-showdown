res <- data.frame()

for (i in 1:1000) {
	print(i)
	
	# generate 100 studies, then reduce to 50 later to match Uri's simulations
	dat1 <- data.frame(dataMA(k=150, delta=0, tau=0, empN=T, maxN=500, minN=0, meanN=0, selProp=.60, qrpEnv='none'))
	dat1r <- dat1[dat1$d > 0, ]

	# select 30/20 random studies from dat1r
	dat2 <- rbind(dat1r[dat1r$sel==1, ][sample(1:sum(dat1r$sel==1), 30),], dat1r[dat1r$sel==0, ][sample(1:sum(dat1r$sel==0), 20),])

	r1 <- TPSM.est(t=dat1$t, n1=dat1$n1, n2=dat1$n2, long=TRUE)
	r2 <- TPSM.est(t=dat2$t, n1=dat2$n1, n2=dat2$n2, long=TRUE)
	
	res <- rbind(res, data.frame(
		r1.est = r1[1, "value"],
		r2.est = r2[1, "value"],
		r1.p = r1[3, "value"],
		r2.p = r2[3, "value"]
	))
}

summary(res)
table(res$r1.p < .05)
table(res$r2.p < .05)