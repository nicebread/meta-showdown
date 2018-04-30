# Do I want to focus on k = 10 and k = 100
# or is k = 100 too easy and would k = 60 be more informative

#library(dplyr)
library(tidyverse)
load("dataFiles/summ.RData")

# Remember, the ugly thing about this is that puniform only has power rates for H0.reject.pos.rate
#    and p-curve only has power rates for H0.reject.pos.rate, listed under method "pcurve.evidence"

summ2 <- summ %>% 
  filter(method %in% c("reMA", "TF", "PETPEESE.lm", "pcurve", "pcurve.evidence", 
                       "puniform", "3PSM", "WAAP-WLS")) %>% 
  # Rename pcurve.evidence to pcurve
  mutate(method = ifelse(method == "pcurve.evidence", "pcurve", method)) %>% 
  # Rename "reMA" to "RE" & make hyphenated names pretty
  mutate(method = factor(method, 
                         levels=c("reMA", "TF", "PETPEESE.lm", "pcurve", "puniform", "3PSM", "WAAP-WLS"), 
                         labels=c("RE", "TF", "PET-PEESE", "p-curve", "p-uniform", "3PSM", "WAAP-WLS")))

# pcurve and puniform only provide posified power, so we have to grab those & call it power instead
#     if we want to put it in the table
# summ3 <- summ %>% 
#   filter(method %in% c("pcurve.evidence", "puniform")) %>% 
#   # rename pcurve.evidence to pcurve
#   mutate(method = ifelse(method == "pcurve.evidence", "p-curve", "p-uniform"))
  
# Plot them with method and k in columns for easier reading as I put it into text
# qrpEnv is none med high
# censor is none med high
master <- summ2 %>% 
  filter(delta %in% c(0, 0.5),
         tau %in% c(0, 0.2),
         k %in% c(10, 60)) %>% 
  ungroup() %>% 
  dplyr::select(delta, tau, k, method, qrpEnv, censor, ME, RMSE, 
                H0.reject.rate, coverage, H0.reject.pos.rate)

# TODO: turn p-curve/p-uniform's posified power into power and put it back into master

# master2 <- summ3 %>% 
#   filter(delta %in% c(0, 0.5),
#          tau %in% c(0, 0.2),
#          k %in% c(10, 60)) %>% 
#   ungroup() %>% 
#   dplyr::select(delta, tau, k, method, qrpEnv, censor, H0.reject.pos.rate)

# Todo: combine master and master2 in an elegant way for clean reading
# bind_rows(master, master2) %>% 
#   filter(method %in% c("pcurve", "p-curve")) %>% View("bound")

output.ME <- master %>% 
  dplyr::select(delta:censor, ME) %>% 
  filter(!is.na(ME)) %>% 
  unite(method, method, k) %>% 
  spread(key = method, value = ME) %>% 
  arrange(censor, qrpEnv, tau, delta)

output.RMSE <- master %>% 
  dplyr::select(delta:censor, RMSE) %>% 
  filter(!is.na(RMSE)) %>% 
  unite(method, method, k) %>% 
  spread(key = method, value = RMSE) %>% 
  arrange(censor, qrpEnv, tau, delta)

output.pow <- master %>% 
  dplyr::select(delta:censor, H0.reject.rate) %>% 
  filter(!is.na(H0.reject.rate)) %>% 
  unite(method, method, k) %>% 
  spread(key = method, value = H0.reject.rate) %>% 
  arrange(censor, qrpEnv, tau, delta)

output.coverage <- master %>% 
  dplyr::select(delta:censor, coverage) %>% 
  filter(!is.na(coverage)) %>% 
  unite(method, method, k) %>% 
  spread(key = method, value = coverage) %>% 
  arrange(censor, qrpEnv, tau, delta)

write.csv(output.ME, "tables/ME_table.csv", row.names = F)
write.csv(output.RMSE, "tables/RMSE_table.csv", row.names = F)
write.csv(output.pow, "tables/pow_table.csv", row.names = F)
write.csv(output.coverage, "tables/coverage_table.csv", row.names = F)

# Make smaller, easier tables for starting results section
output.ME %>%  
  filter(qrpEnv == "none",
         censor %in% c("none", "high")) %>% 
  write.csv("tables/ME_table_small.csv", row.names = F)

output.RMSE %>%  
  filter(qrpEnv == "none",
         censor %in% c("none", "high")) %>% 
  write.csv("tables/RMSE_table_small.csv", row.names = F)

output.pow %>%  
  filter(qrpEnv == "none",
         censor %in% c("none", "high")) %>% 
  write.csv("tables/pow_table_small.csv", row.names = F)

output.coverage %>% 
  filter(qrpEnv == "none",
         censor %in% c("none", "high")) %>% 
  write.csv("tables/cov_table_small.csv", row.names = F)


# sort by RMSE
# TODO: How do I sort within these groups?
output.RMSE %>% 
  gather(key, value, `3PSM_10`:`WAAP-WLS_60`) %>% 
  separate(key, into = c("method", "k"), sep ="_") %>% 
  filter(qrpEnv == "none",
         censor == "high") %>% 
  select(delta, tau, k, censor, method, value) %>% 
  arrange(delta, tau, k, censor, value) %>% 
  View()

output.coverage %>% 
  gather(key, value, `3PSM_10`:`WAAP-WLS_60`) %>% 
  separate(key, into = c("method", "k"), sep ="_") %>% 
  filter(qrpEnv == "none",
         censor == "high") %>% 
  dplyr::select(delta, tau, k, censor, method, value) %>% 
  arrange(delta, tau, k, censor, value) %>% 
  View()

# Plot them with method in columns, just one outcome, for supplementary tables
# Power
master %>% 
  dplyr::select(k, delta, qrpEnv, censor, tau, method, H0.reject.rate, H0.reject.pos.rate) %>% 
  # kludge p-curve into place
  mutate(H0.reject.rate = ifelse(is.na(H0.reject.rate), H0.reject.pos.rate, H0.reject.rate)) %>% 
  # clean up the mess, dropping H0.reject.pos.rate and removing the missing values
  dplyr::select(-H0.reject.pos.rate) %>% 
  filter(!is.na(H0.reject.rate)) %>% 
  spread(key = method, value = H0.reject.rate) %>% View()


# Try to plot some stuff to figure out effects of QRPs.
MEplot <- function(dat, est) {
  filter(dat, method == est) %>% 
    ggplot(aes(x = interaction(tau, delta), y = ME, color = qrpEnv)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(-.3, .5)) +
    facet_grid(k~censor) +
    ggtitle(est)
}
# how much bias can QRPs alone cause?
filter(master, method == "RE", censor == "med", delta == 0, tau == 0)

MEplot(master, "RE") # QRP generally increases ME when h0 true; decreases when h1 true are minimal
MEplot(master, "TF") # QRP generally increases ME when h0 true, but less than for RE
MEplot(master, "WAAP-WLS") # QRP increase ME when h0 true; slight decrease when h1 true
MEplot(master, "PET-PEESE") # QRP sharply decreases ME across conditions, often strong negative bias
MEplot(master, "p-curve") # QRP decreases ME across conditions, sometimes negative bias
MEplot(master, "p-uniform") # QRP decreases ME across conditions, sometimes negative bias
MEplot(master, "3PSM") # QRP decreases ME across conditions, sometimes negative bias

# max diff from QRP
master %>% 
  filter(method != "p-curve_H0") %>% 
  group_by(method, delta, tau, k, censor) %>% 
  summarize(maxMEdiff = max(ME) - min(ME)) %>% 
  ggplot(aes(x = maxMEdiff, fill = as.factor(delta))) +
  geom_histogram() + 
  facet_wrap(~method)
# max diff from censoring
master %>% 
  filter(method != "p-curve_H0") %>% 
  group_by(method, delta, tau, k, qrpEnv) %>% 
  summarize(maxMEdiff = max(ME) - min(ME)) %>% 
  ggplot(aes(x = maxMEdiff, fill = as.factor(delta))) +
  geom_histogram() + 
  facet_wrap(~method)

RMSEplot <- function(dat, est) {
  filter(dat, method == est) %>% 
    ggplot(aes(x = interaction(tau, delta), y = RMSE, color = qrpEnv)) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 0.575)) +
    facet_grid(k~censor) +
    ggtitle(est)
}
RMSEplot(master, "RE") # QRP generally increases RMSE slightly, but more influential under pub bias
RMSEplot(master, "TF") # same
RMSEplot(master, "WAAP-WLS") # same
RMSEplot(master, "PET-PEESE") # QRP generally increases RMSE, can decrease it if bias is strong
RMSEplot(master, "p-curve") # QRP increases RMSE when null is false, homogeneity; decreases o.w.
RMSEplot(master, "p-uniform") # same
RMSEplot(master, "3PSM")

# max diff from QRP
master %>% 
  filter(method != "p-curve_H0") %>% 
  group_by(method, delta, tau, k, censor) %>% 
  summarize(maxRMSEdiff = max(RMSE) - min(RMSE)) %>% 
  ggplot(aes(x = maxRMSEdiff, fill = as.factor(delta))) +
  geom_histogram() + 
  facet_wrap(~method)
# max diff from censoring
master %>% 
  filter(method != "p-curve_H0") %>% 
  group_by(method, delta, tau, k, qrpEnv) %>% 
  summarize(maxRMSEdiff = max(RMSE) - min(RMSE)) %>% 
  ggplot(aes(x = maxRMSEdiff, fill = as.factor(delta))) +
  geom_histogram() + 
  facet_wrap(~method)

powplot <- function(dat, est) {
  filter(dat, method == est) %>% 
    ggplot(aes(x = interaction(tau, delta), y = H0.reject.rate, color = qrpEnv)) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 1)) +
    geom_hline(yintercept = c(.05, .80), lty = 2) +
    facet_grid(k~censor) +
    ggtitle(est)
}
powplot.pos <- function(dat, est) {
  filter(dat, method == est) %>% 
    ggplot(aes(x = interaction(tau, delta), y = H0.reject.pos.rate, color = qrpEnv)) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 1)) +
    geom_hline(yintercept = c(.05, .80), lty = 2) +
    facet_grid(k~censor) +
    ggtitle(est)
}
# increase in Type I error given true null and no pub bias
filter(master, method == "RE", censor == "none", delta == 0)

powplot(master, "RE") # increase in Type I is considerable but small compared to pub bias
powplot(master, "TF") # QRP still increases Type I given pub bias
powplot(master, "WAAP-WLS") # QRP has small effect on power, complex effect on pub bias
powplot(master, "PET-PEESE") # QRP increases Type I and Type II error both.  Wrong sign?
#powplot(master, "p-curve")
powplot.pos(master, "p-curve")
#powplot(master, "p-uniform")
powplot.pos(master, "p-uniform")
powplot(master, "3PSM")

powplot.pos(master, "PET-PEESE")

# trying to see how bad the drop is in points
filter(master, method %in% c("p-curve", "p-uniform"), 
       delta == 0.5,
       censor %in% c("med", "high"), 
       k == 10, 
       !is.na(H0.reject.pos.rate)) %>% 
  arrange(method, delta, censor) %>% 
  View()

filter(master, method == "3PSM",
       delta == 0.5,
       censor %in% c('med', 'high'),
       k == 10) %>% 
  arrange(method, delta, censor) %>% 
  View()

filter(master, method == "PET-PEESE",
       delta == 0.5,
       censor %in% c('none', 'med', 'high'),
       k == 10) %>% 
  arrange(method, delta, censor) %>% 
  View()


# is it wrong sign in PET-PEESE?
filter(master, method == "PET-PEESE") %>% 
  ggplot(aes(x = interaction(tau, delta), y = H0.reject.pos.rate, color = qrpEnv)) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = c(.05, .80), lty = 2) +
  facet_grid(k~censor)

ciplot <- function(dat, est) {
  filter(dat, method == est) %>% 
    ggplot(aes(x = interaction(tau, delta), y = coverage, color = qrpEnv)) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 1)) +
    geom_hline(yintercept = .95, lty = 2) +
    facet_grid(k~censor) +
    ggtitle(est)
}
ciplot(master, "RE") # generally speaking, a loss of coverage
ciplot(master, "TF") # complex
ciplot(master, "WAAP-WLS") # complex
ciplot(master, "PET-PEESE") # some complexity; generally a loss of coverage
ciplot(master, "p-uniform") # better coverage when h0 true or tau = 0.2
ciplot(master, "3PSM") # better coverage, sometimes, when there's pub bias
