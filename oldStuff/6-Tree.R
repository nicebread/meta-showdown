library(party)

# plotting of results
library(ggplot2)
library(dplyr)
library(tidyr)

load(file="res.hyp.RData")

glimpse(res.hyp)

set <- res.hyp %>% filter(delta %in% c(0, 0.5))
set$trueEffect <- set$delta != 0
#run the tree

set.wide <- dcast(set, id + k + delta + trueEffect + qrpEnv + selProp + tau ~ method, value.var="H0.reject")

myTree <- ctree(trueEffect ~ PETPEESE.lm + pcurve.evidence + pcurve.lack + puniform + McShane, data=set.wide, controls=ctree_control(mincriterion=.9999, minsplit=10000, minbucket = 10000)) 
myTree
plot(myTree, type="simple")