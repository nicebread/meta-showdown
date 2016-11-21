source("../start.R", chdir=TRUE)

load("../simParts/YsimData_condition_100.RData")

MAdat <- sim[sim$id==103100, ]
MAdat

RMA.est(d=MAdat$d, v=MAdat$v, long=TRUE)

TPSM.est(t=MAdat$t, n1=MAdat$n1, n2=MAdat$n2, long=TRUE)

t=MAdat$t
n1=MAdat$n1
n2=MAdat$n2