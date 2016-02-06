## ======================================================================
## FELIX
## ======================================================================

load("summ.RData")

# ---------------------------------------------------------------------
#  visualize
library(ggplot2)
ggplot(summ %>% filter(tau==0, selProp==0.6), aes(x=k.label, y=meanEst, color=method)) + geom_point() + facet_grid(qrp.label~delta.label) + geom_hline(aes(yintercept=delta)) + theme_bw()

ggplot(summ %>% filter(tau==0, selProp==0.6), aes(x=k.label, y=ME, color=method)) + geom_point() + facet_grid(qrp.label~delta.label) + theme_bw() + geom_hline(yintercept=0)

ggplot(summ %>% filter(tau==0, selProp==0.6), aes(x=k.label, y=MSE, color=method)) + geom_point() + facet_grid(qrp.label~delta.label) + theme_bw()

# ---------------------------------------------------------------------
#  show violin plots in partly loop style
library(gtools)

# order two variables into one loop
res.wide <- res.wide %>% mutate(k_method=paste0(k, "_", method))

# order loop factor alphabetically
res.wide$k_method <- factor(res.wide$k_method, levels=mixedsort(unique(res.wide$k_method)))

# select relevant data
sel <- res.wide %>% filter(method!="FAT", tau==0, selProp==1)

ggplot(sel, aes(x=k_method, y=d, color=method, group=k_method)) + geom_violin() + facet_grid(qrp.label~delta.label) + geom_hline(aes(yintercept=delta)) + theme_bw()+ theme(axis.text.x = element_text(angle = 90, size=3))

# without loop
ggplot(sel, aes(x=k.label, y=d, color=method, group=k_method)) + geom_violin(position=position_dodge()) + facet_grid(qrp.label~delta.label) + geom_hline(aes(yintercept=delta)) + theme_bw()


# control condition with tau=0 and selProp=0
ggplot(summ %>% filter(tau==0, selProp==0), aes(x=k, y=meanEst, color=method)) + geom_point() + facet_grid(qrp.label~delta.label) + geom_hline(aes(yintercept=delta)) + theme_bw()



# ---------------------------------------------------------------------
# pcurve follow up

res.wide %>% filter(method=="pcurve") %>% group_by(k, delta, qrpEnv, selProp, tau) %>% summarise(
	nStudies=round(mean(sig.studies, na.rm=TRUE), 2)
)
	
head(res.wide %>% filter(method=="pcurve"))	


## ======================================================================
## JOE
## ======================================================================

