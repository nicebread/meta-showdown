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
  dplyr::select(condition:method, ME, RMSE, ME.pos, RMSE.pos, n.validEstimates)

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
  makeTauDiff() #%>% View()
pretty.ME %>% 
  filter(censor == "none") %>% 
  makeTauDiff() #%>% View()
pretty.RMSE %>% 
  filter(censor == "none") %>% 
  makeTauDiff() #%>% View()
pretty.coverage %>% 
  filter(censor == "none") %>% 
  makeTauDiff() %>% 
  filter(!is.na(d)) #%>% View()
# with pub bias
pretty.pow %>% 
  filter(censor == "high") %>% 
  makeTauDiff() #%>% View()
pretty.ME %>% 
  filter(censor == "high") %>% 
  makeTauDiff() #%>% View()
pretty.RMSE %>% 
  filter(censor == "high") %>% 
  makeTauDiff() #%>% View()
pretty.coverage %>% 
  filter(censor == "high") %>% 
  makeTauDiff() %>% 
  filter(!is.na(d)) #%>% View()


# Inspect influence of QRPs ----
summ2 %>% 
  dplyr::select(k, delta, tau, qrpEnv, censor, method, H0.reject.hybrid.rate, ME, RMSE, coverage) %>% 
  filter(delta %in% c(0, 0.5),
         tau %in% c(0, 0.2),
         k %in% c(10, 60),
         !(method %in% c("4PSM", "PET", "PEESE"))) %>%
  mutate_at(funs(round(., 3)), .vars = vars(H0.reject.hybrid.rate, ME, RMSE, coverage)) %>% 
  View()



# adjustments
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
           #censor %in% c("none", "high"),
           !(method %in% c("PET", "PEESE", "4PSM")),
           delta %in% c(0, 0.5),
           tau %in% c(0, 0.2)) %>% 
    # within each scenario (delta, tau, k, censor), arrange methods in order of performance
    select(delta, tau, k, censor, qrpEnv, method, value) %>% 
    arrange(delta, tau, k, censor, qrpEnv, value) %>% 
    # round to 3 decimals for reading's sake
    mutate(value = round(value, 3)) %>% 
    group_by(delta, k, tau, censor, method) %>%
    summarize(none = value[qrpEnv == "none"],
              low.med = value[qrpEnv == "med"] - value[qrpEnv == "none"],
              low.hi = value[qrpEnv == "high"] - value[qrpEnv == "none"],
              med.hi = value[qrpEnv == "high"] - value[qrpEnv == "med"])
}

output.pow %>% 
  makeQRPDiff #%>% View()
output.ME %>% 
  makeQRPDiff #%>% View()
output.RMSE %>% 
  makeQRPDiff #%>% View()
output.coverage %>% 
  makeQRPDiff %>% 
  filter(!is.na(low.med)) #%>% View()

# Examine effects of QRPs ----
MEplot <- function(dat, est) {
  filter(dat, method == est) %>% 
    ggplot(aes(x = interaction(delta, tau), y = ME, color = qrpEnv)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0) +
    #scale_y_continuous(limits = c(-.3, .5)) +
    facet_grid(k~censor) +
    ggtitle(est)
}
# how much bias can QRPs alone cause?
filter(summ2, method == "reMA", censor == "med", delta == 0, tau == 0)

MEplot(summ2, "reMA") # QRP generally increases ME when h0 true; decreases when h1 true are minimal
MEplot(summ2, "TF") # QRP generally increases ME when h0 true, but less than for RE; slight decrease under h1
MEplot(summ2, "WAAP-WLS") # QRP increase ME when h0 true; slight decrease when h1 true
MEplot(summ2, "PETPEESE") # QRP sharply decreases ME across conditions, often strong negative bias
# vvv This looks weird. why? 
MEplot(summ2, "pcurve") # QRP decreases ME across conditions, sometimes negative bias
# See pop-up of qrpEnv=="med", delta == 0.5, tau == 0, censor == "none", k == 30
# See also pop-up of qrpEnv == "high", delta == 0, tau == 0, censor == "med", k == 100
# Why is "n.validEstimates" NA?
MEplot(summ2, "puniform") # Ok this looks like it but less weird... # QRP decreases ME across conditions, sometimes negative bias
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
#filter(summ2, method == "reMA", censor == "none", delta == 0)

powplot(summ2, "reMA") # increase in Type I is considerable but small compared to pub bias
powplot(summ2, "TF") # QRP still increases Type I given pub bias
powplot(summ2, "WAAP-WLS") # QRP has small effect on power, complex effect on pub bias
powplot(summ2, "PETPEESE") # QRP increases Type I and Type II error both.  Wrong sign?
powplot(summ2, "pcurve")
powplot(summ2, "puniform")
powplot(summ2, "3PSM")
powplot(summ2, "4PSM")

# trying to see how bad the drop is in points
# TODO: Add dplyr::select so this is legible
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
# TODO: How would this plot tell me that?
filter(summ2, method == "PETPEESE") %>% 
  ggplot(aes(x = interaction(tau, delta), y = H0.reject.hybrid.rate, color = qrpEnv)) +
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



#############################
# Stuff gets messy after here
#############################

# Write to table for supplement ----
# ALL THIS APPEARS TO BE DEPRECATED
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

