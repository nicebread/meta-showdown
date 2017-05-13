load(file="../summ.RData")

sel <- summ %>% select(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, selProp.label, tau, tau.label, method,  H0.reject.wrongSign.rate) %>% filter(!method %in% c("pcurve.hack", "pcurve.lack", "pcurve"))

print(sel %>% filter(H0.reject.wrongSign.rate > .30), n=1000)

sel %>% group_by(method) %>% summarise(mean(H0.reject.wrongSign.rate))