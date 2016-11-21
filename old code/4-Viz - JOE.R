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