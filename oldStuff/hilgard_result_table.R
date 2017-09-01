# power check
source("0-start.R")
load(file="./dataFiles/summ.RData")
library(tidyr)
library(ggplot2)

summ <- summ %>% 
  ungroup() %>% 
  filter(method %in% c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "WAAP-WLS", 
                       "pcurve.evidence", "pcurve", "puniform", "1PSM", "3PSM", "4PSM"))

summ.pcurve.evi <- summ %>% 
  filter(method == "pcurve.evidence") %>% 
  mutate(method = "pcurve") %>% 
  select(condition:method, 
         consisZero.rate, consisZero.rate.pos, 
         H0.reject.rate, H0.reject.pos.rate,
         H0.reject.wrongSign.rate, n.p.values)

summ.pcurve.est <- summ %>% 
  filter(method == "pcurve") %>% 
  select(condition:method, ME, RMSE, ME.pos, RMSE.pos)

summ.pcurve <- full_join(summ.pcurve.est, summ.pcurve.evi)

summ2 <- filter(summ, !(method %in% c("pcurve", "pcurve.evidence", "pcurve.lack", "pcurve.hack"))) %>% 
  bind_rows(summ.pcurve)  

# No pub bias ----
# ME.pos
summ2 %>%   
  filter(qrpEnv == "none", censor == 0) %>% 
  select(k, delta, tau, qrpEnv, censor, method, ME.pos) %>% 
  mutate(ME.pos = round(ME.pos, 3)) %>% 
  spread(key = method, value = ME.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == 0) %>% 
  select(k:method, ME.pos) %>% 
  spread(key = method, value = ME.pos) %>%
  gather(key = method, value = ME.pos, PEESE.lm:TF) %>% 
  ggplot(aes(x = ME.pos, y = `3PSM`, col = delta.label)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~method) +
  scale_x_continuous(limits = c(0, 0.6)) +
  scale_y_continuous(limits = c(0, 0.6)) +
  ggtitle("ME.pos for censor = 0%")

# RMSE.pos
summ2 %>%   
  filter(qrpEnv == "none", censor == 0) %>% 
  select(k, delta, tau, qrpEnv, censor, method, RMSE.pos) %>% 
  mutate(RMSE.pos = round(RMSE.pos, 3)) %>% 
  spread(key = method, value = RMSE.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == 0) %>% 
  select(k:method, RMSE.pos) %>% 
  spread(key = method, value = RMSE.pos) %>%
  gather(key = method, value = RMSE.pos, PEESE.lm:TF) %>% 
  ggplot(aes(x = RMSE.pos, y = `3PSM`, col = delta.label)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~method) +
  scale_x_continuous(limits = c(0, 0.6)) +
  scale_y_continuous(limits = c(0, 0.6)) +
  ggtitle("RMSE.pos for censor = 0%")

# Power estimates
summ2 %>%   
  filter(qrpEnv == "none", censor == 0) %>% 
  select(k, delta, tau, qrpEnv, censor, method, H0.reject.pos.rate) %>% 
  spread(key = method, value = H0.reject.pos.rate) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == 0) %>% 
  select(k:method, H0.reject.pos.rate) %>% 
  spread(key = method, value = H0.reject.pos.rate) %>%
  gather(key = method, value = H0.reject.pos.rate, PEESE.lm:TF) %>% 
  ggplot(aes(x = H0.reject.pos.rate, y = `3PSM`, col = delta.label)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~method) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("H0.reject.pos.rate for censor = 0%")

# Coverage
summ2 %>%   
  filter(qrpEnv == "none", censor == 0) %>% 
  select(k, delta, tau, qrpEnv, censor, method, coverage) %>% 
  spread(key = method, value = coverage) %>% 
  arrange(tau, delta, k) %>% 
  View()

# Some pub bias ----
# ME.pos
summ2 %>%   
  filter(qrpEnv == "none", censor == "A") %>% 
  select(k, delta, tau, qrpEnv, censor, method, ME.pos) %>% 
  mutate(ME.pos = round(ME.pos, 3)) %>% 
  spread(key = method, value = ME.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

# effects of tau
summ2 %>%   
  filter(qrpEnv == "none", censor == "A") %>% 
  select(k, delta, tau, qrpEnv, censor, method, ME.pos) %>% 
  mutate(ME.pos = round(ME.pos, 3)) %>% 
  spread(key = method, value = ME.pos) %>% 
  arrange(delta, k, tau) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "A") %>% 
  select(k:method, ME.pos) %>% 
  spread(key = method, value = ME.pos) %>%
  gather(key = method, value = ME.pos, PEESE.lm:TF) %>% 
  ggplot(aes(x = ME.pos, y = `3PSM`, col = delta.label)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~method) +
  scale_x_continuous(limits = c(0, 0.55)) +
  scale_y_continuous(limits = c(0, 0.55)) +
  ggtitle("ME.pos for censor = A")

# RMSE.pos
summ2 %>%   
  filter(qrpEnv == "none", censor == "A") %>% 
  select(k, delta, tau, qrpEnv, censor, method, RMSE.pos) %>% 
  mutate(RMSE.pos = round(RMSE.pos, 3)) %>% 
  spread(key = method, value = RMSE.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "A") %>% 
  select(k:method, RMSE.pos) %>% 
  spread(key = method, value = RMSE.pos) %>%
  gather(key = method, value = RMSE.pos, PEESE.lm:TF) %>% 
  ggplot(aes(x = RMSE.pos, y = `3PSM`, col = delta.label)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~method) +
  scale_x_continuous(limits = c(0, 0.55)) +
  scale_y_continuous(limits = c(0, 0.55)) +
  ggtitle("RMSE.pos for censor = A")

# Power estimates
summ2 %>%   
  filter(qrpEnv == "none", censor == "A") %>% 
  select(k, delta, tau, qrpEnv, censor, method, H0.reject.pos.rate) %>% 
  spread(key = method, value = H0.reject.pos.rate) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "A") %>% 
  select(k:method, H0.reject.pos.rate) %>% 
  spread(key = method, value = H0.reject.pos.rate) %>%
  gather(key = method, value = H0.reject.pos.rate, PEESE.lm:TF) %>% 
  ggplot(aes(x = H0.reject.pos.rate, y = `3PSM`, col = delta.label)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~method) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("H0.reject.pos.rate for censor = A")

# Coverage
summ2 %>%   
  filter(qrpEnv == "none", censor == "A") %>% 
  select(k, delta, tau, qrpEnv, censor, method, coverage) %>% 
  spread(key = method, value = coverage) %>% 
  arrange(tau, delta, k) %>% 
  View()

# Strong pub bias ----
# ME.pos
summ2 %>%   
  filter(qrpEnv == "none", censor == "B") %>% 
  select(k, delta, tau, qrpEnv, censor, method, ME.pos) %>% 
  mutate(ME.pos = round(ME.pos, 3)) %>% 
  spread(key = method, value = ME.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "B") %>% 
  select(k:method, ME.pos) %>% 
  spread(key = method, value = ME.pos) %>%
  gather(key = method, value = ME.pos, PEESE.lm:TF) %>% 
  ggplot(aes(x = ME.pos, y = `3PSM`, col = delta.label)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~method) +
  scale_x_continuous(limits = c(0, 0.6)) +
  scale_y_continuous(limits = c(0, 0.6)) +
  ggtitle("ME.pos for censor = B")

# RMSE.pos
summ2 %>%   
  filter(qrpEnv == "none", censor == "B") %>% 
  select(k, delta, tau, qrpEnv, censor, method, RMSE.pos) %>% 
  mutate(RMSE.pos = round(RMSE.pos, 3)) %>% 
  spread(key = method, value = RMSE.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "B") %>% 
  select(k, delta, tau, qrpEnv, censor, method, RMSE.pos) %>% 
  mutate(RMSE.pos = round(RMSE.pos, 3)) %>% 
  spread(key = method, value = RMSE.pos) %>% 
  arrange(reMA - `3PSM`) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "B") %>% 
  select(k:method, RMSE.pos) %>% 
  spread(key = method, value = RMSE.pos) %>%
  gather(key = method, value = RMSE.pos, PEESE.lm:TF) %>% 
  ggplot(aes(x = RMSE.pos, y = `3PSM`, col = delta.label)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~method) +
  scale_x_continuous(limits = c(0, 0.6)) +
  scale_y_continuous(limits = c(0, 0.6)) +
  ggtitle("RMSE.pos for censor = B")

# Power estimates
summ2 %>%   
  filter(qrpEnv == "none", censor == "B") %>% 
  select(k, delta, tau, qrpEnv, censor, method, H0.reject.pos.rate) %>% 
  spread(key = method, value = H0.reject.pos.rate) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "B") %>% 
  select(k:method, H0.reject.pos.rate) %>% 
  spread(key = method, value = H0.reject.pos.rate) %>%
  gather(key = method, value = H0.reject.pos.rate, PEESE.lm:TF) %>% 
  ggplot(aes(x = H0.reject.pos.rate, y = `3PSM`, col = delta.label)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~method) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("H0.reject.pos.rate for censor = B")

# Coverage
summ2 %>%   
  filter(qrpEnv == "none", censor == "B") %>% 
  select(k, delta, tau, qrpEnv, censor, method, coverage) %>% 
  spread(key = method, value = coverage) %>% 
  arrange(tau, delta, k) %>% 
  View()

# OK, 3PSM coverage isn't great. but aren't the alternatives even worse?
summ2 %>%   
  filter(qrpEnv == "none", censor == "B") %>% 
  select(k, delta, tau, qrpEnv, censor, method, coverage) %>% 
  spread(key = method, value = coverage) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "B") %>% 
  select(k:method, coverage) %>% 
  spread(key = method, value = coverage) %>%
  gather(key = method, value = coverage, PEESE.lm:TF) %>% 
  ggplot(aes(x = coverage, y = `3PSM`, col = delta.label)) +
  geom_point() +
  geom_hline(yintercept = .95) +
  geom_abline(slope = 1) +
  facet_wrap(~method) +
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,1))

# How often does 3PSM coverage outperform other coverage?
superiority <- summ2 %>%   
  filter(qrpEnv == "none", censor == "B") %>% 
  select(k:method, coverage) %>% 
  spread(key = method, value = coverage) %>%
  gather(key = method, value = coverage, PEESE.lm:TF) %>% 
  mutate(superior3PSM = `3PSM` > coverage & `3PSM` < .97) # some overcoverage of up to 96%
with(superiority, table(superior3PSM, method))

# effect of QRPs on false positives
summ2 %>%   
  filter(delta == 0, censor == "B") %>% 
  select(k, delta, tau, qrpEnv, censor, method, H0.reject.pos.rate) %>% 
  spread(key = method, value = H0.reject.pos.rate) %>% 
  arrange(tau, delta, k) %>% 
  View()

# even older stuff -----
{
# Thinking about tables...
source("0-start.R")
# install.packages('tidyr')
library(tidyr)
library(ggplot2)

load("summ.Rdata")

summ <- summ %>% 
  mutate(ME.pos = meanEst.pos - delta)

summ2 <- summ %>% 
  ungroup() %>% 
  select(-condition, -k.label, -delta.label, -qrp.label,
         -censor.label, -tau.label, -meanEst) %>% 
  filter(!(method %in% c("pcurve.evidence", "pcurve.lack", "pcurve.hack")))

# maybe comparison against RE is useful, e.g. method - RE difference in ME, RMSE, etc?

# maybe restriction to one combination of factors at a time?
# consider use of spread() to spread ME across multiple methods.

summ.me <- summ2 %>% 
  select(k:method, ME) %>% 
  spread(key = method, value = ME) %>% 
  mutate_each(funs(round(., 3)), reMA:`3PSM`)
summ.me.pos <- summ2 %>% 
  select(k:method, ME.pos) %>% 
  spread(key = method, value = ME.pos) %>% 
  mutate_each(funs(round(., 3)), reMA:`3PSM`)
summ.rmse <- summ2 %>% 
  select(k:method, RMSE) %>% 
  spread(key = method, value = RMSE) %>% 
  mutate_each(funs(round(., 3)), reMA:`3PSM`)
summ.ci <- summ2 %>% 
  select(k:method, coverage) %>% 
  spread(key = method, value = coverage) %>% 
  mutate_each(funs(round(., 3)), reMA:`3PSM`)

# How should I filter these? How should I arrange these?
# Could start by filtering for some delta, censor, tau

# No pub bias and no QRP ----

summ.me %>% 
  filter(censor == 0,
         qrpEnv == "none") %>% 
  arrange(tau, delta) %>% 
  View()
# RE and TopN are unbiased
# PET is downward biased when d > 0
# PEESE is unbiased at d = 0, 
#   shows slight downward bias when d is large (maybe due to SE being a function of d)
#   downward bias increases as tau increases
# p-curve / p-uniform are unbiased but become upward biased as tau increases
# 3PSM is perfectly unbiased

summ.rmse %>% 
  filter(censor == 0,
         qrpEnv == "none") %>% 
  arrange(tau, delta) %>% 
  View()
# everything is strictly less efficient than RE, of course
# PET and PEESE are less efficient even under best-case scenario
# no difference between lm and rma
# p-curve and p-uniform quite inefficient, particularly when delta is small
# TopN loses efficiency, of course
# 3PSM's loss of efficiency is not bad -- more efficient than TF

# censor = .6, no QRP ----
# Bias
summ.me %>% 
  filter(censor == "A",  
         qrpEnv == "none") %>% 
  arrange(delta, k, tau) %>% 
  View

# PET and tau
summ.me %>% 
  filter(censor == "A",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PET.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# PEESE and tau
summ.me %>% 
  filter(censor == "A",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PEESE.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# PETPEESE and tau
summ.me %>% 
  filter(censor == "A",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PETPEESE.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# RMSE
summ.rmse %>% 
  filter(censor == "A",  
         qrpEnv == "none") %>% 
  arrange(delta, tau, k) %>% 
  View

# metaregression vs naive
summ.rmse %>% 
  filter(censor == "A",  
         qrpEnv == "none") %>% 
  gather(key = method, value = rmse, PETPEESE.lm, reMA) %>% 
  ggplot(aes(x = k, y = rmse, col = method)) +
  geom_jitter(height = 0, width = 2, size = 3) +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# loss of efficiency from TopN vs naive
summ.rmse %>% 
  filter(censor == "A",  
         qrpEnv == "none") %>% 
  gather(key = method, value = rmse, TF, topN.fixed, PETPEESE.lm, reMA) %>% 
  ggplot(aes(x = k, y = rmse, col = method)) +
  geom_jitter(height = 0, width = 4, size = 2) +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# does moving from censor .6 to .9 change bias of techniques? ----
summ.me %>% 
  filter(censor %in% c(0.6, 0.9),
         qrpEnv == 'none',
         tau == 0) %>% 
  gather(key = method, value = bias, reMA, topN.fixed, 
         TF, puniform, `3PSM`, PETPEESE.lm) %>% 
  ggplot(aes(x = as.factor(censor), y = bias, col = as.factor(delta))) +
  geom_point() +
  facet_wrap(~method)
# just makes bias moreso in reMA, TF, topN,
# can lead to more wild underadjustments in PETPEESE,
# actually helps pcurve/uniform

summ.rmse %>% 
  filter(censor %in% c(0.6, 0.9),
         qrpEnv == 'none',
         tau == 0) %>% 
  gather(key = method, value = bias, reMA, topN.fixed, 
         TF, puniform, `3PSM`, PETPEESE.lm) %>% 
  ggplot(aes(x = as.factor(censor), y = bias, col = as.factor(delta))) +
  geom_jitter(height = 0, width = .1) +
  facet_wrap(~method)

# censor == "B", no QRP ---
summ.me %>% 
  filter(censor == "B",  
         qrpEnv == "none") %>% 
  arrange(delta, k, tau) %>% 
  View

# PET and tau
summ.me %>% 
  filter(censor == "B",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PET.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# PEESE and tau
summ.me %>% 
  filter(censor == "B",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PEESE.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# PETPEESE and tau
summ.me %>% 
  filter(censor == "B",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PETPEESE.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# And the rest
summ.me %>% 
  filter(censor == "B",  
         qrpEnv == "none") %>% 
  gather(key = method, value = bias, reMA, topN.fixed, TF, puniform, `3PSM`, PETPEESE.lm) %>% 
  #mutate(method = relevel(method, "reMA")) %>% 
  ggplot(aes(x = k, y = bias, col = method)) +
  geom_jitter(width = 3, height = 0, size = 3) +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# RMSE
summ.rmse %>% 
  filter(censor == "B",  
         qrpEnv == "none") %>% 
  arrange(delta, tau, k) %>% 
  View

# metaregression vs naive
summ.rmse %>% 
  filter(censor == "B",  
         qrpEnv == "none") %>% 
  gather(key = method, value = rmse, PETPEESE.lm, reMA) %>% 
  ggplot(aes(x = k, y = rmse, col = method)) +
  geom_jitter(height = 0, width = 2, size = 3) +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# loss of efficiency from TopN vs naive
summ.rmse %>% 
  filter(censor == "B",  
         qrpEnv == "none") %>% 
  gather(key = method, value = rmse, TF, topN.fixed, PETPEESE.lm, reMA) %>% 
  ggplot(aes(x = k, y = rmse, col = method)) +
  geom_jitter(height = 0, width = 4, size = 3) +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)


# delta = .5, censor = .6, tau = .2. ----
# What's influence of k, QRP? 
summ.me %>% 
  filter(delta == .5, 
         censor == "A",
         tau == .2) %>% 
  View
# RE actually less biased w/ more QRP
# I guess if you're gonna have k studies,better that they be p = .048 than p = 0.25
# PET shows medium-bad downward bias, exacerbated by QRP
# PEESE shows some slight downward bias due to QRP
# p-curve/p-uniform slight upward bias due to tau cancels out w/ downward bias of QRP
# topN retains some of upward bias from RE
# 3PSM starts unbiased but QRPs cause downward bias

summ.rmse %>% 
  filter(delta == .5, 
         censor == "A",
         tau == .2) %>% 
  View
# TF has better RMSE than RE
# PET has terrible RMSE due to downward bias + variance
# PEESE has greater RMSE than RE until k = 60, 
#   at which point reducing bias is worth loss of efficiency
# p-curve suffers poor RMSE at k = 10, but is an improvement at k => 30
# p-unif may be a smidge more efficient somehow
# TopN isn't worth the loss of efficiency
# 3PSM... damn. even in absence of QRPs (hence no downward bias) RMSE is big
#   bias/variance tradeoff doesn't break even until k = 30, profit until k = 60
#   even then, downward bias caused by QRPs is quite bad!

# delta = 0, tau = .2, censor = .6 ----
# Is tau = .2 appropriate for delta = 0? Maybe not
summ.me %>% 
  filter(delta == 0, 
         censor == "A",
         tau == .2) %>% 
  View

summ.rmse %>% 
  filter(delta == .5, 
         censor == "A",
         tau == .2) %>% 
  View

# delta = 0, tau = 0 ----
# e.g., the effect is definitely zero
summ.me %>% 
  filter(delta == 0, 
         tau == 0) %>% 
  arrange(censor, qrpEnv) %>% 
  View
# TF fails miserably at adjusting to zero
# PET adjusts to zero, overadjusts when QRPs present
# PEESE overestimates, but QRPs inflict compensating underestimation
# PETPEESE is worst of both worlds...
# p-curve is on the money, returns < 0 b/c of QRP
# p-uniform as above
# TopN never gets below d = .10, sadly
# 3PSM is on the money but returns < 0 b/c of QRPs

summ.rmse %>% 
  filter(delta == 0, 
         censor == "A",
         tau == 0) %>% 
  View
# TF is less biased than, and roughly as efficient as, RE, so it's an improvement
# PET starts out quite variable, isn't more accurate until k => 30
# PEESE is more accurate than TF, fairly efficient. A question of bias.
# PETPEESE is an improvement at k >= 30. 
#   Seems more efficient than PET alone somehow? Maybe fewer dramatic undershoots?
# p-curve starts high-variance and remains a little moreso than PET or PEESE
#   makes sense b/c PET and PEESE consider data that p-curve ignores
# p-uniform is somehow more efficient than p-curve
# TopN really does help a bit, moreso than trim-and-fill (but not PEESE)
# 3PSM looks beautiful

# Under what conditions does PEESE outperform reMA, TF, puniform, topN?
summ.me %>% 
  filter(abs(PEESE.lm) < abs(TF), abs(PEESE.lm) < abs(reMA)) %>% 
  arrange(censor, delta) %>% 
  View()
# Casual inspection, may not apply to all cases
# PEESE is less biased than reMA and TF when 
# 1) The effect size is small (d =< .5) and there is pub bias (censor >= .6)
#     a) PEESE's advantage increases with censor, decreases with delta and QRP
summ.rmse %>% 
  filter(abs(PEESE.lm) < abs(TF), abs(PEESE.lm) < abs(reMA)) %>% 
  arrange(censor, delta) %>% 
  View()
# PEESE has lower RMSE than reMA and TF when when its bias is small, 
# e.g. when the advantage of accounting for censor outweights downward bias of 
# high delta or QRP
# I guess it does not perform well at delta = .8
# Seems to always guess a smallish d.

# Under what conditions do lm and rma differ?
# first, in ME
ggplot(summ.me, aes(x = PET.lm, y = PET.rma)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

ggplot(summ.me, aes(x = abs(PET.lm), y = abs(PET.rma))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) 
# I think this indicates PET.rma has greater downward bias than PET.lm?

ggplot(summ.me, aes(x = abs(PET.lm), y = abs(PET.rma), col = as.factor(delta))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~tau)

ggplot(summ.me, aes(x = abs(PET.lm), y = abs(PET.rma), col = as.factor(delta))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  facet_grid(censor~tau)
# Difference between PET.lm and PET.rma seems to depend on tau

ggplot(summ.me, aes(x = PEESE.lm, y = PEESE.rma)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

ggplot(summ.me, aes(x = abs(PEESE.lm), y = abs(PEESE.rma))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
# Difference between PEESE.lm and PEESE.rma seems quite subtle

# Second, in RMSE
ggplot(summ.rmse, aes(x = PET.lm, y = PET.rma)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
# Seem very comparable

ggplot(summ.rmse, aes(x = PEESE.lm, y = PEESE.rma)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
# PEESE.lm may be slightly more efficient than PEESE.rma


# Posified estimates? ----
# when do ME and ME.pos substantially differ?
hist(summ$ME.pos - summ$ME)
arrange(summ, desc(ME.pos - ME)) %>% 
  select(k:tau, method, ME, ME.pos, everything()) %>% 
  View()
# Posification seems to dramatically reduce bias from undershoots for 
#   PET, PETPEESE, and p-curve/p-uniform
# These undershoots happened mostly when delta = 0, k is small, there's heavy QRP,
# and in the case of PET/PETPEESE, heterogeneity

summ %>% 
  filter(method %in% c("reMA", "PET.lm", "PEESE.lm", "PETPEESE.lm", 
                       "pcurve", "puniform", "3PSM")) %>% 
  ggplot(aes(x = ME, y = ME.pos, col = qrpEnv)) +
  geom_point() + 
  facet_grid(method ~ delta) +
  geom_hline(aes(yintercept = -delta), col = "grey50") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
# when delta <= 0, you see some benefits from points flattening out at grey line towards 0 ME
# although there are some upward biases introduced 
#   e.g. when there's no QRP, PET/PETPEESE have upward bias
# In general I think this is a reasonable bias/variance tradeoff;
#   gains a little bias sometimes but generally cuts a lot of variance

summ %>% 
  filter(method %in% c("reMA", "PET.lm", "PEESE.lm", "PETPEESE.lm", 
                       "pcurve", "puniform", "3PSM")) %>% 
  filter(k == 100) %>% 
  ggplot(aes(x = ME, y = ME.pos, col = qrpEnv)) +
  geom_point() + 
  facet_grid(method ~ delta) +
  geom_hline(aes(yintercept = -delta), col = "grey50") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)


# what does posification do to bias?
summ.me.pos %>% 
  filter(censor == 0,
         qrpEnv == "none") %>% 
  arrange(tau, delta) %>% 
  View()
summ.me.pos %>% 
  gather(key = key, value = ME.pos, reMA:`3PSM`) %>% 
  filter(censor == 0, qrpEnv == "none", 
         key %in% c("3PSM", "pcurve", "puniform", "reMA", "TF")) %>% 
  ggplot(aes(x = key, y = ME.pos, col = key)) +
  geom_point() +
  facet_grid(delta ~ tau)
# Posification seems to cause some upward bias when delta = 0
# because the results are no longer symmetrical
# This is probably good for RMSE, though, right?

# 95% CI coverage ----
# No pub bias and no QRP
summ.ci %>% 
  filter(censor == 0,
         qrpEnv == "none") %>% 
  arrange(tau, delta) %>% 
  View()
# reMA seems to have slight overcoverage for tau = 0, undercoverage for tau > 0 
# TF has stronger undercoverage all around, exacerbated by increasing d and tau 
#   (often 90%ish coverage but can be as bad as 55%)
# PET has increasing undercoverage problem as tau and delta increase
# PEESE has undercoverage problem under tau, plus the downward bias thing
# PETPEESE has same undercoverage issues as PET
# rma versions provide better coverage than lm versions
# p-uniform shows good coverage until tau > 0 blows it up
# 3PSM very similar to reMA -- slight undercoverage for tau > 0, esp when k = 10

# 60% pub bias and no QRP
summ.ci %>% 
  filter(censor == "A",
         qrpEnv == "none") %>% 
  arrange(tau, delta) %>% 
  View()
# reMA completely misses it of course. best case is 96% coverage when delta = 0.8
# TF coverage is terrible unless delta >= 0.5. 60, 70, 80% coverage there, 
#  ~95% when delta = 0.8 and k = 10 and tau = 0
# PET has good coverage for d = 0 tau = 0 but it rapidly goes to hell away from that
# PEESE has decent (80%) coverage for delta >= 0.5
# PET PEESE has decent coverage (80-90%) but can be very bad when delta = 0.2
# p-uniform shows good coverage (~95%) so long as tau = 0
# 3PSM shows good coverage throughout, never worse than 83% (small k, big tau)

# 90% pub bias and no QRP
summ.ci %>% 
  filter(censor == "B",
         qrpEnv == "none") %>% 
  arrange(tau, delta) %>% 
  View()
# Obviously reMA and TF coverage are terrible unless d = 0.8
# PET: Coverage can be good when delta = 0. Kind of a mess otherwise, heavily dependent on bias
# PEESE: Coverage is uniformly bad, starts at 90% and works its way south quickly
# PETPEESE: Highly variable coverage rates, often bad (<70%)
# p-uniform coverage is spot-on until tau > 0
# TopN returning NAs, not sure why
# 3PSM column has a lot of NAs. 
#   I'm guessing it refuses to return a CI when there aren't many (any?) non-sig studies
#   Question is, should there be an na.rm = T somewhere up the pipeline?



# What does QRP do to coverage?




# lm metaregression vs rma metaregression ----
# In the absence of QRPs, it seems .rma method is preferable, yielding slightly greater
#   efficiency and 95% CI coverage
# Given QRPs, however, .rma performance degenerates more than .lm performance

# bias
# above line means rma better, below line means lm better (but reversed above y = 0)
# Differences in the two in bias seem minimal, but QRPs bias rma worse than lm
summ.me %>%
  #filter(qrpEnv == "none") %>%
  ggplot(aes(x = PET.lm, y = PET.rma)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = 0, col = "grey40") +
  geom_vline(xintercept = 0, col = "grey40") +
  #facet_wrap(~censor) +
  facet_grid(qrpEnv~censor) +
  ggtitle("Bias, PET")
summ.me %>%
  #filter(qrpEnv == "none") %>%
  ggplot(aes(x = PEESE.lm, y = PEESE.rma)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = 0, col = "grey40") +
  geom_vline(xintercept = 0, col = "grey40") +
  facet_grid(qrpEnv~censor) +
  ggtitle("Bias, PEESE")
summ.me %>%
  #filter(qrpEnv == "none") %>%
  ggplot(aes(x = PETPEESE.lm, y = PETPEESE.rma)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = 0, col = "grey40") +
  geom_vline(xintercept = 0, col = "grey40") +
  facet_grid(qrpEnv~censor) +
  ggtitle("Bias, PETPEESE")

# RMSE
# below line means rma better, above line means lm better
summ.rmse %>%
  ggplot(aes(x = PET.lm, y = PET.rma)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  facet_grid(qrpEnv~censor) +
  ggtitle("RMSE, PET")
summ.rmse %>%
  #filter(qrpEnv == "none") %>%
  ggplot(aes(x = PEESE.lm, y = PEESE.rma)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  facet_grid(qrpEnv~censor) +
  ggtitle("RMSE, PEESE")
summ.rmse %>%
  #filter(qrpEnv == "none") %>%
  ggplot(aes(x = PETPEESE.lm, y = PETPEESE.rma)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  facet_grid(qrpEnv~censor) +
  #facet_wrap(~censor) +
  ggtitle("RMSE, PETPEESE")

# Coverage probability
# above line means rma better, below line means lm better
# rma seems to show better coverage, except QRPs can wreck it
summ.ci %>%
  #filter(qrpEnv == "none") %>%
  ggplot(aes(x = PET.lm, y = PET.rma)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = .95) +
  facet_grid(qrpEnv~censor) +
  ggtitle("Coverage probability, PET")
summ.ci %>%
  #filter(qrpEnv == "none") %>%
  ggplot(aes(x = PEESE.lm, y = PEESE.rma)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = .95) +
  facet_grid(qrpEnv~censor) +
  ggtitle("Coverage probability, PEESE")
summ.ci %>%
  #filter(qrpEnv == "none") %>%
  ggplot(aes(x = PETPEESE.lm, y = PETPEESE.rma)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = .95) +
  facet_grid(qrpEnv~censor) +
  ggtitle("Coverage probability, PETPEESE")
}
