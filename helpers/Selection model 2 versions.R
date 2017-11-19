B <- 10000
df <- 100
t <- rt(B, df)

dat <- data.frame(
	t = t,
	d = 2*t / sqrt(df),
	p = 2*(1-pt(abs(t), df=df))
)

dat$sig <- dat$p <= 0.05
dat$posSign <- dat$t > 0
dat$expected <- dat$sig & dat$posSign

dat$OLD_pubBias <- rbinom(B, 1, 0.9)	
dat$OLD_published <- (dat$OLD_pubBias == 1 & dat$expected == TRUE) | dat$OLD_pubBias == 0
table("published" = dat$OLD_published, "expected" = dat$expected, "pubBias" = dat$OLD_pubBias)


dat$NEW_publishNS <- NA
dat$NEW_publishNS[dat$expected == FALSE] <- rbinom(sum(dat$expected == FALSE), 1, 0.1)
dat$NEW_published <- dat$expected == TRUE | (dat$expected == FALSE & dat$NEW_publishNS == 1)

table("published" = dat$NEW_published, "expected" = dat$expected)

table("published" = dat$NEW_published, "publishNS" = dat$NEW_publishNS, "expected" = dat$expected, useNA="ifany")

addmargins(table("OLD" = dat$OLD_published, "NEW" = dat$NEW_published))

mean(abs(dat$d[dat$OLD_published == TRUE]))
mean(abs(dat$d[dat$NEW_published == TRUE]))