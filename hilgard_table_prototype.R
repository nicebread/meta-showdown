# Thinking about tables...
source("start.R")

load("summ.Rdata")

summ2 <- summ %>% 
  ungroup() %>% 
  select(-condition, -k.label, -delta.label, -qrp.label,
         -selProp.label, -tau.label, -meanEst) %>% 
  filter(!(method %in% c("pcurve.evidence", "pcurve.lack")))

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

summ.rmse %>% 
  filter(delta == .5, 
         selProp == .6,
         tau == .2) %>% 
  View

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
