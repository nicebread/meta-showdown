## ======================================================================
## FELIX
## ======================================================================
library(ggplot2)
library(dplyr)

load("summ.RData")

# ---------------------------------------------------------------------
# Show estimate

# remove selProp = 0 (unrealistic; show in Appendix)
summ2 <- summ %>% filter(
	!method %in% c("PET.lm", "PEESE.lm", "PET.rma", "PEESE.rma", "PETPEESE.rma", "pcurve.hack", "pcurve.evidence", "pcurve.lack"),
	selProp != 0
	)

summ2 %>% filter(selProp==0.6) %>% 
	ggplot(aes(x=k.label, y=meanEst, ymin=perc2.5, ymax=perc97.5, color=factor(delta))) + 
	geom_pointrange(position=position_dodge(width=0.3)) + 
	geom_hline(aes(yintercept=delta, color=factor(delta))) + 
	coord_flip(ylim=c(-0.3, 1)) +
	facet_grid(qrp.label~tau.label~method) + 
	theme_bw()


summ2 %>% filter(selProp==0.6) %>% 
	ggplot(aes(x=qrp.label, y=MAD, color=factor(method))) + 
	geom_point() + 
	coord_flip(ylim=c(-0.3, 1)) +
	facet_grid(delta~tau.label~k.label) + 
	theme_bw()


## ======================================================================
## JOE
## ======================================================================
library(gtools)

#  show dot plots in partly loop style

# order two variables into one loop
summ2 <- summ2 %>% 
  mutate(k_method = paste0(k, "_", method))

# order loop factor alphabetically
summ2$k_method <- factor(summ2$k_method, 
                        levels = mixedsort(unique(summ2$k_method)))

# order two variables into one loop
summ2 <- summ2 %>% 
  mutate(tau_k = paste0(tau, "_", k))

# order loop factor alphabetically
summ2$tau_k <- factor(summ2$tau_k, 
                        levels = mixedsort(unique(summ2$tau_k)))

summ2 %>% 
  filter(delta == 0, qrpEnv == "none") %>% 
  ggplot(aes(x = tau_k, y = ME, color = method)) + 
  geom_point() + 
  facet_grid(selProp ~ .) + 
  geom_hline(aes(yintercept = 0)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, size=10))

summ2 %>% 
  filter(delta == 0, qrpEnv == "none") %>% 
  ggplot(aes(x = tau_k, y = RMSE, color = method)) + 
  geom_point() + 
  facet_grid(selProp ~ .) + 
  geom_hline(aes(yintercept = 0)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, size=10))