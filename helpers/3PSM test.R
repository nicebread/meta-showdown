source("../0-start.R", chdir=TRUE)

# condition 104 is the condition that has the lowest 3PSM convergence rate
load("../simPartsRev2/simData_condition_104.RData")

MAdat <- sim[sim$id==1004104, ]
MAdat

RMA.est(d=MAdat$d, v=MAdat$v, long=TRUE)

# 3PSM fails
threePSM.est(d=MAdat$d, v=MAdat$v, min.pvalues=0, long=TRUE)
weightfunct(effect=MAdat$d, v=MAdat$v, steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE)