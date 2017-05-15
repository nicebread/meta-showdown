# Under what conditions does 3PSM fail to return 95% CI?
# When 3PSM fails to return 95% CI, does it return a point estimate?
# This is all rather cycle-intensive
source("start.R")
library(tidyr)

# Typical 3PSM behavior
set.seed(5)
dat <- dataMA(k = 12, delta = 0.41, tau = 0.01, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0, qrpEnv = "none")
dat <- data.frame(dat)
mm <- estimate.onestep.selection.heterogeneous(z.obs=dat$t, n1=dat$n1, n2=dat$n2, 
                                         alpha=0.05/2, 
                                         theta.init=c(expected.d = 0.3, max.tau= 0.5, p.report = 0.99))
mm
TPSM.est(t=dat$t, n1=dat$n1, n2=dat$n2)

# Bias in tau and delta under heavy pub bias and small k ----
set.seed(42069)
simLength <-  250 

output <- NULL
for (i in 1:simLength) {
  # Make data
  trial <- dataMA(k = 10, delta = 0, tau = 0.2,
                  empN = T, selProp = .9, qrpEnv = "none") %>% 
    as.data.frame()
  
  # extract estimates
  rma_out <- with(trial, RMA.est(d = d, v = v, long = T))
  tpsm_out <- with(trial, TPSM.est(t = t, n1 = n1, n2 = n2, long = T))
  # Tag with run number
  rma_out$trial <- i
  tpsm_out$trial <- i
  
  # Append to output
  output <- bind_rows(output, rma_out, tpsm_out)
}

# Summarize effect size information
out.wide.delta <- filter(output, term == "b0", variable == "estimate", method != "TF") %>% 
  select(-variable) %>% 
  spread(key = method, value = value)
summary(out.wide.delta)

# Summarize tau information
out.wide.tau <- filter(output, term %in% c("max.tau", "tau2"), variable == "estimate", method != "TF") %>% 
  select(-variable) %>% 
  mutate(value = ifelse(term == "tau2", sqrt(value), value),
         term = "max.tau") %>% 
  select(-term) %>% 
  spread(key = method, value = value)
summary(out.wide.tau)

output %>% 
  filter(term == "max.tau", variable == "estimate", method == "3PSM") %>% 
  with(., hist(value, main = "3PSM underestimates tau = 0.2, k = 10"))
abline(v = .2, col = 'red')
abline(v = .12, col = 'blue')

# Perhaps delta is overestimated when tau is underestimated
output %>% 
  filter(method == "3PSM", 
         term %in% c("b0", "max.tau"),
         variable == "estimate") %>% 
  spread(term, value) %>% 
  ggplot(aes(x = max.tau, y = b0)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = .2) +
  ggtitle("Inverse relationship between\ntau-hat and delta-hat")

# Is it easier with larger tau?
set.seed(42069)
simLength <-  250 

output <- NULL
for (i in 1:simLength) {
  # Make data
  trial <- dataMA(k = 10, delta = 0, tau = .4,
                  empN = T, selProp = .9, qrpEnv = "none") %>% 
    as.data.frame()
  
  # extract estimates
  rma_out <- with(trial, RMA.est(d = d, v = v, long = T))
  tpsm_out <- with(trial, TPSM.est(t = t, n1 = n1, n2 = n2, long = T))
  # Tag with run number
  rma_out$trial <- i
  tpsm_out$trial <- i
  
  # Append to output
  output <- bind_rows(output, rma_out, tpsm_out)
}

# summary of delta-hat
out.wide.delta <- filter(output, term == "b0", variable == "estimate", method != "TF") %>% 
  select(-variable) %>% 
  spread(key = method, value = value)
summary(out.wide.delta)

# summary of tau-hat
out.wide.tau <- filter(output, term %in% c("max.tau", "tau2"), variable == "estimate", method != "TF") %>% 
  select(-variable) %>% 
  mutate(value = ifelse(term == "tau2", sqrt(value), value),
         term = "max.tau") %>% 
  select(-term) %>% 
  spread(key = method, value = value)
summary(out.wide.tau)

# histogram on tau
output %>% 
  filter(term == "max.tau", variable == "estimate") %>% 
  with(., hist(value, main = "3PSM underestimates tau = 0.4, k = 10"))
abline(v = .4, col = 'red')
abline(v = .35, col = 'blue')

# Delta is overestimated when tau is underestimated
output %>% 
  filter(method == "3PSM", 
         term %in% c("b0", "max.tau"),
         variable == "estimate") %>% 
  spread(term, value) %>% 
  ggplot(aes(x = max.tau, y = b0)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = .4) +
  ggtitle("Inverse relationship between\ntau-hat and delta-hat")

# Is it easier with larger k?
set.seed(42069)
simLength <-  250 

output <- NULL
for (i in 1:simLength) {
  # Make data
  trial <- dataMA(k = 100, delta = 0, tau = .2,
                  empN = T, selProp = .9, qrpEnv = "none") %>% 
    as.data.frame()
  
  # extract estimates
  rma_out <- with(trial, RMA.est(d = d, v = v, long = T))
  tpsm_out <- with(trial, TPSM.est(t = t, n1 = n1, n2 = n2, long = T))
  # Tag with run number
  rma_out$trial <- i
  tpsm_out$trial <- i
  
  # Append to output
  output <- bind_rows(output, rma_out, tpsm_out)
}

# summary of delta
out.wide.delta <- filter(output, term == "b0", variable == "estimate", method != "TF") %>% 
  select(-variable) %>% 
  spread(key = method, value = value)
summary(out.wide.delta)

# summary of tau
out.wide.tau <- filter(output, term %in% c("max.tau", "tau2"), variable == "estimate", method != "TF") %>% 
  select(-variable) %>% 
  mutate(value = ifelse(term == "tau2", sqrt(value), value)) %>% 
  select(-term) %>% 
  spread(key = method, value = value)
summary(out.wide.tau)

# histogram on tau
output %>% 
  filter(term == "max.tau", variable == "estimate") %>% 
  with(., hist(value, main = "3PSM underestimates tau = 0.2, k = 100"))
abline(v = .2, col = 'red')
abline(v = .16, col = 'blue')

# Delta is overestimated when tau is underestimated
output %>% 
  filter(method == "3PSM", 
         term %in% c("b0", "max.tau"),
         variable == "estimate") %>% 
  spread(term, value) %>% 
  ggplot(aes(x = max.tau, y = b0)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = .2) +
  ggtitle("Inverse relationship between\ntau-hat and delta-hat")
