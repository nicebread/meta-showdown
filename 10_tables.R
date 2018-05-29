# Load summary data & tidyverse package
load(file="./dataFiles/summ.RData")
library(tidyverse)

# Remember, the ugly thing about this is that puniform only has power rates for H0.reject.pos.rate
#    and p-curve only has power rates for H0.reject.pos.rate, listed under method "pcurve.evidence"

# Filter for the methods we want to talk about
summ <- summ %>% 
  ungroup() %>% 
  filter(method %in% c("reMA", "TF", "PET", "PEESE", "PETPEESE", "WAAP-WLS", 
                       "pcurve", "pcurve.evidence", "puniform", "3PSM", "4PSM"))

# Coerce pcurve.evidence to pcurve & h0.reject.pos.rate to h0.reject.hybrid.rate ----
# Get "pcurve.evidence" hyptests, relabel as "pcurve"
summ.pcurve.evi <- summ %>% 
  filter(method == "pcurve.evidence") %>% 
  mutate(method = "pcurve",
         # use reject.pos.rate as reject.hybrid.rate
         H0.reject.hybrid.rate = H0.reject.pos.rate) %>% 
  dplyr::select(condition:method, 
                H0.reject.pos.rate, H0.reject.hybrid.rate,
                n.p.values)

# Get "pcurve" estimation
summ.pcurve.est <- summ %>% 
  filter(method == "pcurve") %>% 
  dplyr::select(condition:method, ME, RMSE, ME.pos, RMSE.pos)

# Join them -- they will form one row
summ.pcurve <- full_join(summ.pcurve.est, summ.pcurve.evi)

# append to full dataset
summ2 <- filter(summ, !(method %in% c("pcurve", "pcurve.evidence", "pcurve.lack", "pcurve.hack"))) %>% 
  # make all other estimators' reject.hybrid.rate <- reject.rate
  mutate(H0.reject.hybrid.rate = H0.reject.rate) %>% 
  bind_rows(summ.pcurve)

# coerce puniform's H0.reject.pos.rate to H0.reject.hybrid.rate
summ2[summ2$method == "puniform", "H0.reject.hybrid.rate"] <- summ2[summ2$method == "puniform", "H0.reject.pos.rate"]

# Confirm that things have merged appropriately
summ2 %>% 
  filter(method %in% c("pcurve")) %>% 
  dplyr::select(method, ME, H0.reject.rate, H0.reject.pos.rate, H0.reject.hybrid.rate)
summ2 %>% 
  filter(method %in% c("puniform")) %>% 
  dplyr::select(method, ME, H0.reject.rate, H0.reject.pos.rate, H0.reject.hybrid.rate)
summ2 %>% 
  filter(method %in% c("reMA")) %>% 
  dplyr::select(method, ME, H0.reject.rate, H0.reject.pos.rate, H0.reject.hybrid.rate)

# Make results ----
output.ME <- summ2 %>% 
  # Restrict to identifiers and ME
  dplyr::select(k:method, ME, -ends_with(".label")) %>% 
  # Put k in separate columns
  unite(method, method, k) %>% 
  spread(key = method, value = ME) %>% 
  # Arrange by simulation settings
  arrange(censor, qrpEnv, tau, delta)

output.RMSE <- summ2 %>% 
  # Restrict to identifiers and RMSE
  dplyr::select(k:method, RMSE, -ends_with(".label")) %>% 
  # Put k in separate columns
  unite(method, method, k) %>% 
  spread(key = method, value = RMSE) %>% 
  # Arrange by simulation settings
  arrange(censor, qrpEnv, tau, delta)

output.pow <- summ2 %>% 
  # Restrict to identifiers and H0.reject.hybrid.rate
  dplyr::select(k:method, H0.reject.hybrid.rate, -ends_with(".label")) %>% 
  # Put k in separate columns
  unite(method, method, k) %>% 
  spread(key = method, value = H0.reject.hybrid.rate) %>% 
  # Arrange by simulation settings
  arrange(censor, qrpEnv, tau, delta)

output.coverage <- summ2 %>% 
  # Restrict to identifiers and coverage
  dplyr::select(k:method, coverage, -ends_with(".label")) %>% 
  # Put k in separate columns
  unite(method, method, k) %>% 
  spread(key = method, value = coverage) %>% 
  # Arrange by simulation settings
  arrange(censor, qrpEnv, tau, delta)

write.csv(output.ME, "tables/ME_table.csv", row.names = F)
write.csv(output.RMSE, "tables/RMSE_table.csv", row.names = F)
write.csv(output.pow, "tables/pow_table.csv", row.names = F)
write.csv(output.coverage, "tables/coverage_table.csv", row.names = F)

# Make smaller, easier tables for writing results section
output.ME %>%  
  filter(qrpEnv == "none",
         censor %in% c("none", "high"),
         delta %in% c(0, 0.5),
         tau %in% c(0, 0.2)) %>% 
  dplyr::select(-ends_with("_30"), -ends_with("_100")) %>% 
  write.csv("tables/ME_table_small.csv", row.names = F)

output.RMSE %>%  
  filter(qrpEnv == "none",
         censor %in% c("none", "high"),
         delta %in% c(0, 0.5),
         tau %in% c(0, 0.2)) %>% 
  dplyr::select(-ends_with("_30"), -ends_with("_100")) %>% 
  write.csv("tables/RMSE_table_small.csv", row.names = F)

output.pow %>%  
  filter(qrpEnv == "none",
         censor %in% c("none", "high"),
         delta %in% c(0, 0.5),
         tau %in% c(0, 0.2)) %>% 
  dplyr::select(-ends_with("_30"), -ends_with("_100")) %>% 
  write.csv("tables/pow_table_small.csv", row.names = F)

output.coverage %>% 
  filter(qrpEnv == "none",
         censor %in% c("none", "high"),
         delta %in% c(0, 0.5),
         tau %in% c(0, 0.2)) %>% 
  dplyr::select(-ends_with("_30"), -ends_with("_100")) %>% 
  write.csv("tables/cov_table_small.csv", row.names = F)

# Viewing manually ----

# make a function for filtering and arranging
forYourEyes <- function(x) {
  x %>% 
    # drop k = 30 and k = 100
    dplyr::select(-ends_with("_30"), -ends_with("_100")) %>% 
    # separate method and k
    gather(key, value, `3PSM_10`:`WAAP-WLS_60`) %>% 
    separate(key, into = c("method", "k"), sep ="_") %>% 
    # filter for no QRP, zero or high pub bias, delta 0 or 0.5, tau 0 or 0.2
    # also drop methods PET PEESE and 4PSM
    filter(qrpEnv == "none",
           censor %in% c("none", "high"),
           !(method %in% c("PET", "PEESE", "4PSM")),
           delta %in% c(0, 0.5),
           tau %in% c(0, 0.2)) %>% 
    # within each scenario (delta, tau, k, censor), arrange methods in order of performance
    select(delta, tau, k, censor, method, value) %>% 
    arrange(delta, tau, k, censor, value) %>% 
    # round to 3 decimals for reading's sake
    mutate(value = round(value, 3))
}

pretty.pow <- forYourEyes(output.pow)
pretty.ME <- forYourEyes(output.ME)
pretty.RMSE <- forYourEyes(output.RMSE)
pretty.coverage <- forYourEyes(output.coverage)
# View(pretty.pow)
# View(pretty.ME)
# View(pretty.RMSE)
# View(pretty.coverage)

pretty.RMSE %>% 
  filter(censor == "none") %>% 
  group_by(delta, k, method) %>% 
  summarize(d = mad(value, constant = 1))

# Examine absolute changes caused by heterogeneity
makeTauDiff <- function(x) {
  x %>% 
    group_by(delta, k, method) %>% 
    summarize(d = value[tau == .2] - value[tau == 0])  
}

# without pub bias
pretty.pow %>% 
  filter(censor == "none") %>% 
  makeTauDiff()
pretty.ME %>% 
  filter(censor == "none") %>% 
  makeTauDiff()
pretty.RMSE %>% 
  filter(censor == "none") %>% 
  makeTauDiff()
pretty.coverage %>% 
  filter(censor == "none") %>% 
  makeTauDiff()
# with pub bias
pretty.pow %>% 
  filter(censor == "high") %>% 
  makeTauDiff()
pretty.ME %>% 
  filter(censor == "high") %>% 
  makeTauDiff()
pretty.RMSE %>% 
  filter(censor == "high") %>% 
  makeTauDiff()
pretty.coverage %>% 
  filter(censor == "high") %>% 
  makeTauDiff()


# Inspect influencec of QRPs ----
makeQRPDiff <- function(x) {
  x %>% 
    # drop k = 30 and k = 100
    dplyr::select(-ends_with("_30"), -ends_with("_100")) %>% 
    # separate method and k
    gather(key, value, `3PSM_10`:`WAAP-WLS_60`) %>% 
    separate(key, into = c("method", "k"), sep ="_") %>% 
    # filter for no QRP, zero or high pub bias, delta 0 or 0.5, tau 0 or 0.2
    # also drop methods PET PEESE and 4PSM
    filter(#qrpEnv == "none",
           censor %in% c("none", "high"),
           !(method %in% c("PET", "PEESE", "4PSM")),
           delta %in% c(0, 0.5),
           tau %in% c(0, 0.2)) %>% 
    # within each scenario (delta, tau, k, censor), arrange methods in order of performance
    select(delta, tau, k, censor, qrpEnv, method, value) %>% 
    arrange(delta, tau, k, censor, qrpEnv, value) %>% 
    # round to 3 decimals for reading's sake
    mutate(value = round(value, 3)) %>% 
    group_by(delta, k, tau, censor, method) %>%
    summarize(low.med = value[qrpEnv == "med"] - value[qrpEnv == "none"],
              low.hi = value[qrpEnv == "high"] - value[qrpEnv == "none"],
              med.hi = value[qrpEnv == "high"] - value[qrpEnv == "med"])
}

output.pow %>% 
  makeQRPDiff
output.ME %>% 
  makeQRPDiff
output.RMSE %>% 
  makeQRPDiff
output.coverage %>% 
  makeQRPDiff



#############################
# Stuff gets messy after here
#############################

# Examine effects of QRPs ----
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
filter(summ2, method == "reMA", censor == "med", delta == 0, tau == 0)

MEplot(summ2, "reMA") # QRP generally increases ME when h0 true; decreases when h1 true are minimal
MEplot(summ2, "TF") # QRP generally increases ME when h0 true, but less than for RE
MEplot(summ2, "WAAP-WLS") # QRP increase ME when h0 true; slight decrease when h1 true
MEplot(summ2, "PETPEESE") # QRP sharply decreases ME across conditions, often strong negative bias
MEplot(summ2, "pcurve") # QRP decreases ME across conditions, sometimes negative bias
MEplot(summ2, "puniform") # QRP decreases ME across conditions, sometimes negative bias
MEplot(summ2, "3PSM") # QRP decreases ME across conditions, sometimes negative bias
MEplot(summ2, "4PSM")

# max diff from QRP
summ2 %>% 
  group_by(method, delta, tau, k, censor) %>% # group by all factors except QRP
  summarize(maxMEdiff = max(ME) - min(ME)) %>% # get difference between max bias and min bias
  ggplot(aes(x = maxMEdiff, fill = as.factor(delta))) +
  geom_histogram() + 
  scale_x_continuous(limits = c(0, 0.4)) + # scale b/c p-curve goes nuts somewhere
  facet_wrap(~method)
# max diff from censoring
summ2 %>% 
  group_by(method, delta, tau, k, qrpEnv) %>% # group by all factors except censor
  summarize(maxMEdiff = max(ME) - min(ME)) %>% # get difference between max bias and min bias
  ggplot(aes(x = maxMEdiff, fill = as.factor(delta))) +
  geom_histogram() + 
  scale_x_continuous(limits = c(0, 0.4)) + 
  facet_wrap(~method)

RMSEplot <- function(dat, est) {
  filter(dat, method == est) %>% 
    ggplot(aes(x = interaction(tau, delta), y = RMSE, color = qrpEnv)) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 0.575)) +
    facet_grid(k~censor) +
    ggtitle(est)
}
RMSEplot(summ2, "reMA") # QRP generally increases RMSE slightly, but more influential under pub bias
RMSEplot(summ2, "TF") # same
RMSEplot(summ2, "WAAP-WLS") # same
RMSEplot(summ2, "PETPEESE") # QRP generally increases RMSE, can decrease it if bias is strong
RMSEplot(summ2, "pcurve") # QRP increases RMSE when null is false, homogeneity; decreases o.w.
RMSEplot(summ2, "puniform") # same
RMSEplot(summ2, "3PSM")
RMSEplot(summ2, "4PSM")

# max diff from QRP
summ2 %>% 
  group_by(method, delta, tau, k, censor) %>% 
  summarize(maxRMSEdiff = max(RMSE) - min(RMSE)) %>% 
  ggplot(aes(x = maxRMSEdiff, fill = as.factor(delta))) +
  geom_histogram() + 
  facet_wrap(~method)
# max diff from censoring
summ2 %>% 
  group_by(method, delta, tau, k, qrpEnv) %>% 
  summarize(maxRMSEdiff = max(RMSE) - min(RMSE)) %>% 
  ggplot(aes(x = maxRMSEdiff, fill = as.factor(delta))) +
  geom_histogram() + 
  facet_wrap(~method)

powplot <- function(dat, est) {
  filter(dat, method == est) %>% 
    ggplot(aes(x = interaction(tau, delta), y = H0.reject.hybrid.rate, color = qrpEnv)) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 1)) +
    geom_hline(yintercept = c(.05, .80), lty = 2) +
    facet_grid(k~censor) +
    ggtitle(est)
}
# increase in Type I error given true null and no pub bias
filter(summ2, method == "RE", censor == "none", delta == 0)

powplot(summ2, "reMA") # increase in Type I is considerable but small compared to pub bias
powplot(summ2, "TF") # QRP still increases Type I given pub bias
powplot(summ2, "WAAP-WLS") # QRP has small effect on power, complex effect on pub bias
powplot(summ2, "PETPEESE") # QRP increases Type I and Type II error both.  Wrong sign?
powplot(summ2, "pcurve")
powplot(summ2, "puniform")
powplot(summ2, "3PSM")
powplot(summ2, "4PSM")

# trying to see how bad the drop is in points
filter(summ2, method %in% c("pcurve", "puniform"), 
       delta == 0.5,
       censor %in% c("med", "high"), 
       k == 10) %>% 
  arrange(method, delta, censor) %>% 
  View()

filter(summ2, method == "3PSM",
       delta == 0.5,
       censor %in% c('med', 'high'),
       k == 10) %>% 
  arrange(method, delta, censor) %>% 
  View()

filter(summ2, method == "PET-PEESE",
       delta == 0.5,
       censor %in% c('none', 'med', 'high'),
       k == 10) %>% 
  arrange(method, delta, censor) %>% 
  View()


# is it wrong sign in PET-PEESE?
filter(summ2, method == "PET-PEESE") %>% 
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
ciplot(summ2, "reMA") # generally speaking, a loss of coverage
ciplot(summ2, "TF") # complex
ciplot(summ2, "WAAP-WLS") # complex
ciplot(summ2, "PETPEESE") # some complexity; generally a loss of coverage
ciplot(summ2, "puniform") # better coverage when h0 true or tau = 0.2
ciplot(summ2, "3PSM") # better coverage, sometimes, when there's pub bias

# Write to table for supplement ----
# Write them with method in columns, just one outcome, for supplementary tables
# Power
summ2 %>% 
  dplyr::select(k, delta, qrpEnv, censor, tau, method, H0.reject.rate, H0.reject.pos.rate) %>% 
  # kludge p-curve into place
  mutate(H0.reject.rate = ifelse(is.na(H0.reject.rate), H0.reject.pos.rate, H0.reject.rate)) %>% 
  # clean up the mess, dropping H0.reject.pos.rate and removing the missing values
  dplyr::select(-H0.reject.pos.rate) %>% 
  filter(!is.na(H0.reject.rate)) %>% 
  spread(key = method, value = H0.reject.rate) %>% View()

# No pub bias ----
# ME.pos
summ2 %>%   
  filter(qrpEnv == "none", censor == "none") %>% 
  dplyr::select(k, delta, tau, qrpEnv, censor, method, ME.pos) %>% 
  mutate(ME.pos = round(ME.pos, 3)) %>% 
  spread(key = method, value = ME.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "none") %>% 
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
  filter(qrpEnv == "none", censor == "none") %>% 
  select(k, delta, tau, qrpEnv, censor, method, RMSE.pos) %>% 
  mutate(RMSE.pos = round(RMSE.pos, 3)) %>% 
  spread(key = method, value = RMSE.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "none") %>% 
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
  filter(qrpEnv == "none", censor == "none") %>% 
  select(k, delta, tau, qrpEnv, censor, method, H0.reject.pos.rate) %>% 
  spread(key = method, value = H0.reject.pos.rate) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "none") %>% 
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
  filter(qrpEnv == "none", censor == "none") %>% 
  dplyr::select(k, delta, tau, qrpEnv, censor, method, coverage) %>% 
  spread(key = method, value = coverage) %>% 
  arrange(tau, delta, k) %>% 
  View()

# Some pub bias ----
# ME.pos
summ2 %>%   
  filter(qrpEnv == "none", censor == "medium") %>% 
  select(k, delta, tau, qrpEnv, censor, method, ME.pos) %>% 
  mutate(ME.pos = round(ME.pos, 3)) %>% 
  spread(key = method, value = ME.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

# effects of tau
summ2 %>%   
  filter(qrpEnv == "none", censor == "medium") %>% 
  select(k, delta, tau, qrpEnv, censor, method, ME.pos) %>% 
  mutate(ME.pos = round(ME.pos, 3)) %>% 
  spread(key = method, value = ME.pos) %>% 
  arrange(delta, k, tau) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "medium") %>% 
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
  filter(qrpEnv == "none", censor == "medium") %>% 
  select(k, delta, tau, qrpEnv, censor, method, RMSE.pos) %>% 
  mutate(RMSE.pos = round(RMSE.pos, 3)) %>% 
  spread(key = method, value = RMSE.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "medium") %>% 
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
  filter(qrpEnv == "none", censor == "medium") %>% 
  select(k, delta, tau, qrpEnv, censor, method, H0.reject.pos.rate) %>% 
  spread(key = method, value = H0.reject.pos.rate) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "medium") %>% 
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
  filter(qrpEnv == "none", censor == "medium") %>% 
  select(k, delta, tau, qrpEnv, censor, method, coverage) %>% 
  spread(key = method, value = coverage) %>% 
  arrange(tau, delta, k) %>% 
  View()

# Strong pub bias ----
# ME.pos
summ2 %>%   
  filter(qrpEnv == "none", censor == "strong") %>% 
  select(k, delta, tau, qrpEnv, censor, method, ME.pos) %>% 
  mutate(ME.pos = round(ME.pos, 3)) %>% 
  spread(key = method, value = ME.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "strong") %>% 
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
  filter(qrpEnv == "none", censor == "strong") %>% 
  select(k, delta, tau, qrpEnv, censor, method, RMSE.pos) %>% 
  mutate(RMSE.pos = round(RMSE.pos, 3)) %>% 
  spread(key = method, value = RMSE.pos) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "strong") %>% 
  select(k, delta, tau, qrpEnv, censor, method, RMSE.pos) %>% 
  mutate(RMSE.pos = round(RMSE.pos, 3)) %>% 
  spread(key = method, value = RMSE.pos) %>% 
  arrange(reMA - `3PSM`) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "strong") %>% 
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
  filter(qrpEnv == "none", censor == "strong") %>% 
  select(k, delta, tau, qrpEnv, censor, method, H0.reject.pos.rate) %>% 
  spread(key = method, value = H0.reject.pos.rate) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "strong") %>% 
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
  filter(qrpEnv == "none", censor == "strong") %>% 
  select(k, delta, tau, qrpEnv, censor, method, coverage) %>% 
  spread(key = method, value = coverage) %>% 
  arrange(tau, delta, k) %>% 
  View()

# OK, 3PSM coverage isn't great. but aren't the alternatives even worse?
summ2 %>%   
  filter(qrpEnv == "none", censor == "strong") %>% 
  select(k, delta, tau, qrpEnv, censor, method, coverage) %>% 
  spread(key = method, value = coverage) %>% 
  arrange(tau, delta, k) %>% 
  View()

summ2 %>%   
  filter(qrpEnv == "none", censor == "strong") %>% 
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
  filter(qrpEnv == "none", censor == "strong") %>% 
  select(k:method, coverage) %>% 
  spread(key = method, value = coverage) %>%
  gather(key = method, value = coverage, PEESE.lm:TF) %>% 
  mutate(superior3PSM = `3PSM` > coverage & `3PSM` < .97) # some overcoverage of up to 96%
with(superiority, table(superior3PSM, method))

# effect of QRPs on false positives
summ2 %>%   
  filter(delta == 0, censor == "strong") %>% 
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
  filter(censor == "none",
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
  filter(censor == "none",
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
  filter(censor == "medium",  
         qrpEnv == "none") %>% 
  arrange(delta, k, tau) %>% 
  View

# PET and tau
summ.me %>% 
  filter(censor == "medium",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PET.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# PEESE and tau
summ.me %>% 
  filter(censor == "medium",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PEESE.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# PETPEESE and tau
summ.me %>% 
  filter(censor == "medium",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PETPEESE.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# RMSE
summ.rmse %>% 
  filter(censor == "medium",  
         qrpEnv == "none") %>% 
  arrange(delta, tau, k) %>% 
  View

# metaregression vs naive
summ.rmse %>% 
  filter(censor == "medium",  
         qrpEnv == "none") %>% 
  gather(key = method, value = rmse, PETPEESE.lm, reMA) %>% 
  ggplot(aes(x = k, y = rmse, col = method)) +
  geom_jitter(height = 0, width = 2, size = 3) +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# loss of efficiency from TopN vs naive
summ.rmse %>% 
  filter(censor == "medium",  
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

# censor == "strong", no QRP ---
summ.me %>% 
  filter(censor == "strong",  
         qrpEnv == "none") %>% 
  arrange(delta, k, tau) %>% 
  View

# PET and tau
summ.me %>% 
  filter(censor == "strong",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PET.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# PEESE and tau
summ.me %>% 
  filter(censor == "strong",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PEESE.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# PETPEESE and tau
summ.me %>% 
  filter(censor == "strong",  
         qrpEnv == "none") %>% 
  ggplot(aes(x = k, y = PETPEESE.lm)) +
  geom_point() +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# And the rest
summ.me %>% 
  filter(censor == "strong",  
         qrpEnv == "none") %>% 
  gather(key = method, value = bias, reMA, topN.fixed, TF, puniform, `3PSM`, PETPEESE.lm) %>% 
  #mutate(method = relevel(method, "reMA")) %>% 
  ggplot(aes(x = k, y = bias, col = method)) +
  geom_jitter(width = 3, height = 0, size = 3) +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# RMSE
summ.rmse %>% 
  filter(censor == "strong",  
         qrpEnv == "none") %>% 
  arrange(delta, tau, k) %>% 
  View

# metaregression vs naive
summ.rmse %>% 
  filter(censor == "strong",  
         qrpEnv == "none") %>% 
  gather(key = method, value = rmse, PETPEESE.lm, reMA) %>% 
  ggplot(aes(x = k, y = rmse, col = method)) +
  geom_jitter(height = 0, width = 2, size = 3) +
  facet_grid(delta~tau) +
  geom_hline(yintercept = 0)

# loss of efficiency from TopN vs naive
summ.rmse %>% 
  filter(censor == "strong",  
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
         censor == "medium",
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
         censor == "medium",
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
         censor == "medium",
         tau == .2) %>% 
  View

summ.rmse %>% 
  filter(delta == .5, 
         censor == "medium",
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
         censor == "medium",
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
  filter(censor == "none",
         qrpEnv == "none") %>% 
  arrange(tau, delta) %>% 
  View()
summ.me.pos %>% 
  gather(key = key, value = ME.pos, reMA:`3PSM`) %>% 
  filter(censor == "none", qrpEnv == "none", 
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
  filter(censor == "none",
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
  filter(censor == "medium",
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
  filter(censor == "strong",
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
