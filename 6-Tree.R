library(party)

# plotting of results
library(ggplot2)
library(dplyr)
library(tidyr)

load(file="res.wide.RData")

glimpse(res.wide)

set <- res.wide %>% filter(delta %in% c(0, 0.5))
#run the tree

set.wide <- dcast(set, k + delta + qrpEnv + selProp + tau ~ method, value.var="consisZero")

meTree = ctree(delta ~ RE+Tau+ TF+PEESE+PET+PET-PEESE+pcurve, data=summ.wide %>% filter(delta %in% c(0, 0.5))) 
plot(meTree)