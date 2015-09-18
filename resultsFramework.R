source("start.R")
# load the results file
load("analysisData/analysisStanley3.RData")

# Show conditions
tab <- res %>% group_by(HET, kPer, EFF, BIAS) %>% summarise(n=n())
print(tab, n=50)

## reduce to relevant variables, drop unused factor levels
#res2 <- res %>% select(-batch, -replication, -condition) %>% filter(variable != "tau", method!="FAT") %>% droplevels()

#res.wide <- dcast(res2, HET + kPer + EFF + BIAS + method + unique ~ variable, value.var="value")
#head(res.wide)
#save(res.wide, file="analysisData/analysisStanley3.wide.RData")
load("analysisData/analysisStanley2.wide.RData")

# compute summary statistics using dplyr:
# TODO: I guess EFF is actually EFF/100?
res.wide <- res.wide %>% mutate(d_true = EFF/100)

summ <- res.wide %>% filter(method!="FAT") %>% group_by(HET, kPer, d_true, BIAS, method) %>% summarise(
	meanEst		= round(mean(d, na.rm=TRUE), 3),
	ME 			= round(mean(d - d_true, na.rm=TRUE), 3),
	MSE			= round(mean(d - d_true, na.rm=TRUE)^2, 3),
	coverage 	= round(sum(d_true > lb & d_true < ub)/sum(!is.na(lb)), 3),
	consisZero      = round(sum(0 > lb & 0 < ub)/n(), 3),
	n.simulations = n()
)

summ$d_true.label <- factor(summ$d_true, labels=paste0("d_true = ", unique(summ$d_true)))

print(summ, n=nrow(summ))

# visualize
library(ggplot2)
ggplot(summ, aes(x=HET, y=meanEst, color=method)) + geom_point() + facet_wrap(~d_true.label) + geom_hline(aes(yintercept=d_true)) + theme_bw()


# ---------------------------------------------------------------------
# pcurve follow up

res.wide %>% filter(method=="pcurve") %>% group_by(HET, kPer, d_true, BIAS, method) %>% summarise(
	nStudies=round(mean(nStudies, na.rm=TRUE))
	)