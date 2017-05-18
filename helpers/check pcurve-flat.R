load("../dataFiles/res.wide.red.RData")

# What combinations of sign. right skew // sign. flatter skew do exists?
# How often is a pcurve BOTH skewed and flatter than the reference line?

res.pcurve <- res.wide.red %>% filter(method %in% c("pcurve.lack", "pcurve.evidence")) %>% dplyr::select(1:8, p.value)

res.pcurve.wide <- dcast(res.pcurve, id+condition+k+delta+qrpEnv+selProp+tau ~ method, value.var="p.value")

res.pcurve.wide$combination <- "NA"
res.pcurve.wide$combination[res.pcurve.wide$pcurve.evidence < .05 & res.pcurve.wide$pcurve.lack > .05] <- "skewed & non-flat"
res.pcurve.wide$combination[res.pcurve.wide$pcurve.evidence < .05 & res.pcurve.wide$pcurve.lack < .05] <- "skewed & flat"
res.pcurve.wide$combination[res.pcurve.wide$pcurve.evidence > .05 & res.pcurve.wide$pcurve.lack < .05] <- "non-skewed & flat"
res.pcurve.wide$combination[res.pcurve.wide$pcurve.evidence > .05 & res.pcurve.wide$pcurve.lack > .05] <- "non-skewed & non-flat"

round(prop.table(table(res.pcurve.wide$combination))*100, 1)

round(prop.table(table(res.pcurve.wide$combination, res.pcurve.wide$k))*100, 1)

round(prop.table(table(res.pcurve.wide$combination, res.pcurve.wide$delta, res.pcurve.wide$tau), margin=c(3))*100, 1)