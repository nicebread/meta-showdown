# Data set from https://osf.io/jt9tc/
# van Erp, S., Verhagen, J., Grasman, R. P. P. P., & Wagenmakers, E.-J. (2017). Estimates of between-study heterogeneity for 705 meta-analyses reported in Psychological Bulletin from 1990-2013. Retrieved from osf.io/myu9c


library(rio)
dat <- import("Data 1990-2013 with tau values.xlsx")

taus <- dat$tau[dat[,"Type of ES"] %in% c("Cohen's d", "Hedges' g")]
hist(taus, xlab="tau", main="")

length(taus)

prop.table(table(taus < .2))
prop.table(table(taus < .4))