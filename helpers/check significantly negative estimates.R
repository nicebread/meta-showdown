load(file="../summ.RData")

sel <- summ %>% select(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, selProp.label, tau, tau.label, method,  H0.reject.wrongSign.rate) %>% filter(!method %in% c("pcurve.hack", "pcurve.lack", "pcurve"), H0.reject.wrongSign.rate > .30)

print(sel, n=1000)