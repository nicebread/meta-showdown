# plotting of results
library(ggplot2)
library(dplyr)
library(tidyr)

load("summ.RData")

# drop method == "fill"
summ = summ %>% 
  filter(method != "fill")

# fix k.label
summ$k.label = factor(summ$k, 
                      levels = c(10, 30, 60, 100), 
                      labels = c("k = 10", "k = 30", "k = 60", "k = 100"))
# check accuracy
table(summ$k, summ$k.label)

# add selProp label column
summ$selProp.label = factor(summ$selProp, 
                            levels = c(0, .6, 1), 
                            labels = c("No file-drawer", "60% file-drawer", "All file-drawer"))
# Check accuracy
table(summ$selProp, summ$selProp.label)
  
glimpse(summ)

# acquainting myself with the data
table(summ$condition)
table(summ$selProp)
table(summ$k.label)
table(summ$delta.label)
table(summ$qrpEnv)
table(summ$qrp.label)
table(summ$method) # Q: what is "fill"? Is it trim-and-fill? and is it different from "TF"?
table(summ$tau) # Heterogeneity

# General thoughts:
# I'm kind of opposed to line graphs a la Moreno et al. because we don't have continuous sampling
# over dimensions such as d, tau, selProp, k; rather, we have a few levels.

# Factors: method, selprop, k, delta, qrp, tau -> 6 variables
# We probably want to keep method on either col/fill or x for ease of comparison
# p-curve's downward bias can throw off scale of y-axis, as can bias moving from selProp = 0 to selProp = 1

# so assume col/fill = method, y = outcome. we have three more aesthetics to play with:
# x,
# facet_grid(. ~), and
# facet_grid(~ .)
# so two variables must be either mapped to something else or conditioned upon
 
# Plausible scenario: Some pub bias, minimal heterogeneity, unknown QRPs
# map delta, k, qrp
# condition: selprop == .6, tau == 0
summ %>% 
  filter(selProp == .6, tau == 0) %>% 
  ggplot(aes(x = delta, y = ME, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(qrp.label ~ k.label)

# Plausible scenario: minimal heterogeneity, k = 30
# Unknown delta, unknown qrp, unknown selProp

# Given some publication bias, does QRP change anything?
# Assume tau == 0 and selprop = 0.6
summ %>% 
  filter(tau == 0, selProp == 0.6) %>% 
  ggplot(aes(x = method, y = ME, fill = qrp.label)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(delta.label ~ k.label)
  
# Given no pub bias, does QRP have any effect?
# Assume tau == 0, selprop == 0
summ %>% 
  filter(tau == 0, selProp == 0) %>% 
  ggplot(aes(x = method, y = ME, fill = qrp.label)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(delta.label ~ k.label)

# can make a very basic plot by conditioning on four factors
summ %>% 
  filter(delta == 0, tau == 0, selProp == .6, k == 30) %>% 
  ggplot(aes(x = qrp.label, y = ME, fill = method)) +
  geom_bar(stat = "identity", position = "dodge")

# introducing the problem: degrees of bias in RE per pub bias & QRP
summ %>% 
  filter(method == "RE", tau == 0) %>% 
  ggplot(aes(x = delta, y = ME, fill = qrp.label)) +
  facet_grid(selProp.label ~ k.label) +
  geom_bar(stat = "identity", position = "dodge")

# Do methods recover the null in presence of qrp/bias?
# Conditioning on the null w/ only 10 studies
# heterogeneity increasing
summ %>% 
  filter(delta == 0, k == 10) %>% 
  ggplot(aes(x = qrp.label, y = ME, fill = method)) +
  facet_grid(selProp.label ~ tau) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  ggtitle("Bias, k = 10, null effect")

summ %>% 
  filter(delta == 0, k == 10) %>% 
  ggplot(aes(x = qrp.label, y = MSE, fill = method)) +
  facet_grid(selProp.label ~ tau) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  ggtitle("MSE, k = 10, null effect")

# Bigger literature
summ %>% 
  filter(delta == 0, k == 60) %>% 
  ggplot(aes(x = qrp.label, y = ME, fill = method)) +
  facet_grid(selProp.label ~ tau) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  ggtitle("Bias, k = 60, null effect")

summ %>% 
  filter(delta == 0, k == 60) %>% 
  ggplot(aes(x = qrp.label, y = MSE, fill = method)) +
  facet_grid(selProp.label ~ tau) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  ggtitle("MSE, k = 60, null effect")