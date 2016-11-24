library(dplyr)
library(reshape2)
load("../analysisParts/analysis_YsimData_condition_1.RData")

res2 <- res %>% filter(method=="3PSM", term=="b0") %>% dcast(id ~ variable, value.var="value") %>% 
	mutate(
		zeroInCI=conf.low < 0 & conf.high > 0,
		reject = p.value < .05
	)

table(res2$zeroInCI, res2$reject)


res2 <- res %>% filter(method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack", "pcurve", "puniform"), term=="kSig") %>% dcast(id ~ method, value.var="value") %>% 
	mutate(
		zeroInCI=conf.low < 0 & conf.high > 0,
		reject = p.value < .05
	)

