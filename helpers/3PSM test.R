source("../start.R", chdir=TRUE)

load("../simParts/YsimData_condition_100.RData")

MAdat <- sim[sim$id==103100, ]
MAdat

RMA.est(d=MAdat$d, v=MAdat$v, long=TRUE)

TPSM.est(t=MAdat$t, n1=MAdat$n1, n2=MAdat$n2, long=TRUE)



# number of sign. studies per condition

tab <- res.wide.red %>% filter(method=="puniform") %>% group_by(condition, k, delta, qrpEnv, selProp, tau) %>% dplyr::summarise(
	MIN.sig = min(kSig_estimate),
	MAX.sig = max(kSig_estimate),
	MEDIAN.sig = median(kSig_estimate)
)

print(tab, n=432)