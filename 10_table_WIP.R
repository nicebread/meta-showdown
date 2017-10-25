# Do I want to focus on k = 10 and k = 100
# or is k = 100 too easy and would k = 60 be more informative

library(dplyr)
library(tidyr)
load("dataFiles/summ.RData")



# k = 100, is it always perfect power for delta = 0.5?
filter(summ2, delta == .5, k == 100, tau %in% c(0, 0.2)) %>% 
  ggplot(aes(x = H0.reject.rate, fill = method)) + 
  geom_histogram() # looks like it's only an issue for 3psm really

filter(summ2, delta == .5, k == 100, tau %in% c(0, 0.2)) %>% 
  pull(H0.reject.rate) %>% 
  summary() # min: 68%, median: 100%, mean: 99%

# is k = 60 more interesting?
filter(summ2, delta == .5, k == 60, tau %in% c(0, 0.2)) %>% 
  ggplot(aes(x = H0.reject.rate, fill = method)) + 
  geom_histogram() # now an issue for 3psm and PP

filter(summ2, delta == .5, k == 60, tau %in% c(0, 0.2)) %>% 
  pull(H0.reject.rate) %>% 
  summary() # min: 56%, median: 100%, mean: 97%

# qrpEnv is none med high
# censor is none med high
master <- summ2 %>% 
  filter(delta %in% c(0, 0.5),
         tau %in% c(0, 0.2),
         k %in% c(10, 60)) %>% 
  ungroup() %>% 
  dplyr::select(delta, tau, k, method, qrpEnv, censor, ME, RMSE, H0.reject.rate, coverage)

output.ME <- master %>% 
  dplyr::select(delta:censor, ME) %>% 
  unite(method, method, k) %>% 
  spread(key = method, value = ME) %>% 
  arrange(censor, qrpEnv, tau, delta)

output.RMSE <- master %>% 
  dplyr::select(delta:censor, RMSE) %>% 
  unite(method, method, k) %>% 
  spread(key = method, value = RMSE) %>% 
  arrange(censor, qrpEnv, tau, delta)

output.pow <- master %>% 
  dplyr::select(delta:censor, H0.reject.rate) %>% 
  unite(method, method, k) %>% 
  spread(key = method, value = H0.reject.rate) %>% 
  arrange(censor, qrpEnv, tau, delta)

output.coverage <- master %>% 
  dplyr::select(delta:censor, coverage) %>% 
  unite(method, method, k) %>% 
  spread(key = method, value = coverage) %>% 
  arrange(censor, qrpEnv, tau, delta)

write.csv(output.ME, "tables/ME_table.csv", row.names = F)
write.csv(output.RMSE, "tables/RMSE_table.csv", row.names = F)
write.csv(output.pow, "tables/pow_table.csv", row.names = F)
write.csv(output.coverage, "tables/coverage_table.csv", row.names = F)