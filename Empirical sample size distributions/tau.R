# Data set from https://osf.io/jt9tc/
# van Erp, S., Verhagen, J., Grasman, R. P. P. P., & Wagenmakers, E.-J. (2017). Estimates of between-study heterogeneity for 705 meta-analyses reported in Psychological Bulletin from 1990-2013. Retrieved from osf.io/myu9c


library(rio)
dat <- import("Data 1990-2013 with tau values.xlsx")

sel <- dat[dat[,"Type of ES"] %in% c("Cohen's d", "Hedges' g"), ]
hist(sel$tau, xlab="tau", main="")

length(sel$tau)

prop.table(table(sel$tau > .1))
prop.table(table(sel$tau >= .2))
prop.table(table(sel$tau < .2))
prop.table(table(sel$tau < .4))


# ---------------------------------------------------------------------
#  Investigate I^2 across all meta-analyses

# Compute I^2 from Q
dat$I2 <- (dat$Q - (dat[, "# of effect sizes"] - 1))/dat$Q
dat$I2[dat$Q <= (dat[, "# of effect sizes"] - 1)] <- 0

prop.table(table(dat$I2 > .25))
prop.table(table(dat$I2 > .75))