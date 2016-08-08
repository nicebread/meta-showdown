library(ggplot2)
library(dplyr)
library(tidyr)

load(file="res.wide.RData")

glimpse(res.wide)

set <- res.wide %>% filter(delta %in% c(0, 0.5), !method %in% c("Tau", "fill"))

# for testing: smaller set
set2 <- set[1:100000, ]

set2.long <- dcast(set2, )

set2.summ <- set2 %>% group_by(id) %>% summarise(
	delta=delta[1],
	k=k[1],
	qrpEnv=qrpEnv[1],
	selProp=selProp[1],
	tau=tau[1],
	consisZero = 0 > lb & 0 < ub)/n()
	I1 = 
)