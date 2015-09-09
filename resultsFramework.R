source("start.R")
# load the results file
load("analysisData/analysisStanley2.RData")

tab <- res %>% group_by(HET, EFF) %>% summarise(n=n())
print(tab, n=50)

r1 <- res %>% filter(EFF > 0)

## reduce to relevant variables, drop unused factor levels
res2 <- res %>% select(-batch, -replication, -condition) %>% filter(variable != "tau", method!="FAT") %>% droplevels()

res.wide <- dcast(res2, unique + HET + kPer + EFF + BIAS + method ~ variable, value.var="value")
#save(res.wide, file="analysisData/analysisStanley2.wide.RData")
load("analysisData/analysisStanley1.wide.RData")

# compute summary statistics using dplyr:
# TODO: I guess EFF is actually EFF/100?
res.wide <- res.wide %>% mutate(d_true = EFF/100)

summ <- res.wide %>% filter(method!="FAT") %>% group_by(HET, kPer, EFF, BIAS, method) %>% summarise(
	meanEst		= round(mean(d), 3),
	ME 			= round(mean(d - d_true), 3),
	MSE			= round(mean((d - d_true)^2), 3),
	coverage 	= round(sum(d_true > lb & d_true < ub)/n(), 3)
)

print(summ, n=nrow(summ))