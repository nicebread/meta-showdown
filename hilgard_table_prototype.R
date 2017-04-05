# Thinking about tables...

load("summ.Rdata")

summ2 <- summ %>% 
  ungroup() %>% 
  select(-condition, -k.label, -delta.label, -qrp.label,
         -selProp.label, -tau.label, -meanEst) %>% 
  filter(!(method %in% c("pcurve.evidence", "pcurve.lack")))

# maybe comparison against RE is useful, e.g. method - RE difference in ME, RMSE, etc?

# maybe restriction to one combination of factors at a time?
# consider use of spread() to spread ME across multiple methods.