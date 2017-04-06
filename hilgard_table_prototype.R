# Thinking about tables...
source("start.R")
library(tidyr)

load("summ.Rdata")

summ2 <- summ %>% 
  ungroup() %>% 
  select(-condition, -k.label, -delta.label, -qrp.label,
         -selProp.label, -tau.label, -meanEst) %>% 
  filter(!(method %in% c("pcurve.evidence", "pcurve.lack", "pcurve.hack")))

# maybe comparison against RE is useful, e.g. method - RE difference in ME, RMSE, etc?

# maybe restriction to one combination of factors at a time?
# consider use of spread() to spread ME across multiple methods.

summ.me <- summ2 %>% 
  select(k:method, ME) %>% 
  spread(key = method, value = ME) %>% 
  mutate_each(funs(round(., 3)), reMA:`3PSM`)
summ.rmse <- summ2 %>% 
  select(k:method, RMSE) %>% 
  spread(key = method, value = RMSE) %>% 
  mutate_each(funs(round(., 3)), reMA:`3PSM`)

# How should I filter these? How should I arrange these?
# Could start by filtering for some delta, selProp, tau
# given biasing influence of QRP, maybe makes more sense to arrange by QRP
# then by k

# delta = .5, selProp = 0, tau = .2 ----
summ.me %>% 
  filter(delta == .5, 
         selProp == 0,
         tau == .2) %>% 
  View
# RE has slight upward bias under QRP
# TF has very slight downward bias (-.02) that gets bigger under QRP (-.06)
# PET has modest downward bias (-.05) but QRP badly exacerbates (-.20ish)
# PEESE has very slight downward bias (-.03) but QRP exacerbates (-.09, -.1)
# PETPEESE inherits PET's bias
# p-curve and p-uniform, as ever, tau upward bias, QRP downward bias
# TopN is unbiased
# 3PSM is unbiased, but QRPs can inflict downward bias (-.1 -- -.18)

summ.rmse %>% 
  filter(delta == .5, 
         selProp == 0,
         tau == .2) %>% 
  View
# naturally nothing can be RE in this scenario
# TF is only slightly worse
# PET is substantially worse due to its bias
# PEESE is rather more variable than RE and TF (2-3x variance)
# p-curve/uniform suffer for their bias (from tau), 2-3x variance
# TopN is less efficient than p-curve
# 3psm starts out worse than TF, passes it at k => 60,
#   but QRPs will inflict bad bias


# delta = .5, selProp = .6, tau = .2. ----
# What's influence of k, QRP? 
summ.me %>% 
  filter(delta == .5, 
         selProp == .6,
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
         selProp == .6,
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

# delta = 0, tau = .2, selProp = .6 ----
# Is tau = .2 appropriate for delta = 0? Maybe not
summ.me %>% 
  filter(delta == 0, 
         selProp == .6,
         tau == .2) %>% 
  View
# TF cuts bias slightly
# PET cuts it further but is still upward biased, which is surprising
# PEESE cuts it a bit more than TF, has compensating downward bias from QRPS
# p-curve has bias due to tau, compensating downward bias from QRP
# ditto p-uniform
# TopN cuts bias a little better than TAF but not so well as PET, PEESE, p
# 3PSM looks great but has downward bias from QRPs

summ.rmse %>% 
  filter(delta == 0, 
         selProp == .6,
         tau == .2) %>% 
  View
# TF provides slight benefit to RMSE relative to RE
# PET is an improvement at k => 30, better than TF unless QRP = high
# PEESE is better than TF for k=> 30, and downward bias from QRP helps
# PETPEESE is an improvement from TF for k => 30; downward QRP bias helps
# p-curve is an improvement by k = 30 
#   but depends on competing bias of tau and QRP
# p-uniform seems slightly more efficient than p-curve somehow
# TopN is worse than TF at k < 60, only slightly better thereafter
# 3PSM looks quite good but underestimation can be an issue under QRPs


# delta = 0, tau = 0, selProp = .6 ----
# e.g., the effect is definitely zero
summ.me %>% 
  filter(delta == 0, 
         selProp == .6,
         tau == 0) %>% 
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
         selProp == .6,
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


# Bigger table, better arrangement
# I think we should set aside QRPs for the main results,
# just mention that they generally inflict downward bias on many estimators

# accuracy of techniques when no bias or QRP
summ.me %>% 
  filter(tau != .4,
         selProp == 0, qrpEnv == "none") %>% # other arguments to come
  arrange(delta, tau, k) %>% 
  View()
summ.mse %>% 
  filter(tau != .4,
         selProp == 0, qrpEnv == "none") %>% # other arguments to come
  arrange(delta, tau, k) %>% 
  View()

# accuracy of techniques when selProp = 60%, no QRP
summ.me %>% 
  filter(tau != .4,
         selProp == 0.6, qrpEnv == "none") %>% # other arguments to come
  arrange(delta, tau, k) %>% 
  View()
summ.rmse %>% 
  filter(tau != .4,
         selProp == 0.6, qrpEnv == "none") %>% # other arguments to come
  arrange(delta, tau, k) %>% 
  View()

n1 <- 50; n2 <- 50
df <- n1+n2-2
d <- seq(0, 1, .01)
vard.1 <- (n1 + n2)/(n1 * n2) + (d^2 / (2 *df)) * (n1 + n2) / df
vard.2 <- 2/n1 + (d*2)/(2*(n1-3.94))
vard.3 <- (n1 + n2)/(n1 * n2)
plot(d, vard.2, type = 'l')
lines(d, vard.1, col = 'darkred')
abline(h = vard.3, col = "blue")
