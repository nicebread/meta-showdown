## ======================================================================
## FELIX
## ======================================================================
library(ggplot2)
library(dplyr)


load("summ.RData")
# Discard method == "fill" and method == "Tau"
summ = summ %>% 
  filter(method != "fill", method != "Tau")

# ---------------------------------------------------------------------
#  visualize

summ %>% 
  filter(tau == 0, selProp == 0.6) %>% 
  ggplot(aes(x = k.label, y = meanEst, color = method)) + 
  geom_point() + 
  facet_grid(qrp.label ~ delta.label) + 
  geom_hline(aes(yintercept = delta)) + 
  theme_bw()

summ %>% 
  filter(tau == 0, selProp == 0.6) %>% 
  ggplot(aes(x = k.label, y = ME, color = method)) + 
  geom_point() +
  facet_grid(qrp.label ~ delta.label) + 
  theme_bw() + 
  geom_hline(yintercept = 0)

summ %>% 
  filter(tau == 0, selProp == 0.6) %>% 
  ggplot(aes(x = k.label, y = MSE, color = method)) + 
  geom_point() + 
  facet_grid(qrp.label ~ delta.label) + 
  theme_bw()

# Consider adding slight horizontal jitter to avoid overplotting
# Makes it hard to follow technique across several facets, though :-\
summ %>% 
  filter(tau == 0, selProp == 0.6) %>% 
  ggplot(aes(x = k.label, y = ME, color = method)) + 
  geom_point(size = 3,
             position = position_jitter(width = .15)) +
  facet_grid(qrp.label ~ delta.label) + 
  theme_bw() + 
  geom_hline(yintercept = 0)

# ---------------------------------------------------------------------
#  show violin plots in partly loop style
library(gtools)

# order two variables into one loop
summ <- summ %>% 
  mutate(k_method = paste0(k, "_", method))

# order loop factor alphabetically
summ$k_method <- factor(summ$k_method, 
                            levels = mixedsort(unique(summ$k_method)))

# with loop
summ %>% 
  filter(method != "FAT", tau == 0, selProp == 1) %>% 
  ggplot(aes(x = k_method, y = d, color = method, group = k_method)) + 
  geom_violin() + 
  facet_grid(qrp.label ~ delta.label) + 
  geom_hline(aes(yintercept = delta)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size=3))

# without loop
summ %>% 
  filter(method != "FAT", tau == 0, selProp == 1) %>% 
  ggplot(aes(x = k.label, y = d, color = method, group = k_method)) +
  geom_violin(position = position_dodge()) +
  facet_grid(qrp.label ~ delta.label) +
  geom_hline(aes(yintercept = delta)) +
  theme_bw()


# control condition with tau=0 and selProp=0
summ %>% 
  filter(tau == 0, selProp == 0) %>% 
  ggplot(aes(x = k, y = meanEst, color = method)) + 
  geom_point() + 
  facet_grid(qrp.label ~ delta.label) +
  geom_hline(aes(yintercept = delta)) + 
  theme_bw()



# ---------------------------------------------------------------------
# pcurve follow up

summ %>% 
  filter(method=="pcurve") %>% 
  group_by(k, delta, qrpEnv, selProp, tau) %>% 
  summarise(
    nStudies = round(mean(sig.studies, na.rm=TRUE), 2)
  )

summ %>% 
  filter(method=="pcurve") %>% 
  head()	


## ======================================================================
## JOE
## ======================================================================
library(gtools)

#  show dot plots in partly loop style

# order two variables into one loop
summ <- summ %>% 
  mutate(k_method = paste0(k, "_", method))

# order loop factor alphabetically
summ$k_method <- factor(summ$k_method, 
                        levels = mixedsort(unique(summ$k_method)))

# order two variables into one loop
summ <- summ %>% 
  mutate(tau_k = paste0(tau, "_", k))

# order loop factor alphabetically
summ$tau_k <- factor(summ$tau_k, 
                        levels = mixedsort(unique(summ$tau_k)))

summ %>% 
  filter(delta == 0, qrpEnv == "none") %>% 
  ggplot(aes(x = tau_k, y = ME, color = method)) + 
  geom_point() + 
  facet_grid(selProp ~ .) + 
  geom_hline(aes(yintercept = 0)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, size=10))

summ %>% 
  filter(delta == 0, qrpEnv == "none") %>% 
  ggplot(aes(x = tau_k, y = MSE, color = method)) + 
  geom_point() + 
  facet_grid(selProp ~ .) + 
  geom_hline(aes(yintercept = 0)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, size=10))