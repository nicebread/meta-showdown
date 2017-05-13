# Under what conditions does 3PSM fail to return 95% CI?
# When 3PSM fails to return 95% CI, does it return a point estimate?
# This is all rather cycle-intensive
source("start.R")

load(file="res.wide.red.RData")
# How many 3PSM estimates have no CI?
t3 <- res.wide.red %>% filter(method=="3PSM") %>% group_by(selProp, delta, k, tau, qrpEnv) %>% summarise(
	n.CI = sum(!is.na(b0_conf.high)),
	n.p_value = sum(!is.na(b0_p.value))
)
summary(t3$n.CI)
print(t3, n=432)
t3[t3$n.CI < 500, ]


# How many non-sig. studies does 3PSM need to return reasonable results?

# propagate kSig from p-curve rows to all rows (we need it in the 3PSM row)
res.wide.red <- res.wide.red %>% group_by(id) %>% mutate(kSig = min(kSig_estimate, na.rm=TRUE))
res.wide.red$kNonSig <- res.wide.red$k - res.wide.red$kSig

t3 <- res.wide.red %>% filter(method=="3PSM", k == 10, kNonSig <= 5, !is.na(p.value)) %>% group_by(kNonSig, selProp, delta, tau, qrpEnv) %>% summarise(
	ME 			= mean(b0_estimate - delta, na.rm=TRUE),
	RMSE		= sqrt(mean((b0_estimate - delta)^2, na.rm=TRUE)),
	nStudies = n()
)

table(t3$kNonSig)

ggplot(t3, aes(x=kNonSig, y=ME, shape=qrpEnv, color=factor(delta), fill=factor(delta))) + 
  geom_point(size = 0.7) +	
  coord_flip(ylim=c(-.6, 0.6)) +
  facet_grid(tau~selProp~delta, labeller = label_bquote(rows = tau == .(tau)))+
  scale_y_continuous(breaks = c(-.25,.0,.25)) + 
  scale_shape_manual(values=c(21,22,24)) + 
  ylab("Mean error (ME)") +
  xlab(expression(italic("# nonSig studies")))



# What does 3PSM do when 100% are significant?
set.seed(42069)
trial1 <- dataMA(k = 10, delta = 0, tau = 0,
                 empN = T, selProp = 1, qrpEnv = "none") %>% 
  as.data.frame()

with(trial1, RMA.est(d = d, v = v, long = T))
with(trial1, TPSM.est(t = t, n1 = n1, n2 = n2, long = T))
estimate.onestep.selection.heterogeneous(z.obs=trial1$t, n1=trial1$n1, n2=trial1$n2, 
                                         alpha=0.05/2, 
                                         theta.init=c(expected.d = 0.3, max.tau= 0.5, p.report = 0.99))
# returns point estimates but no variance/covariance matrix, not sure what output $ll is

set.seed(777)
trial2 <- dataMA(k = 10, delta = 0, tau = 0,
                 empN = T, selProp = 1, qrpEnv = "none") %>% 
  as.data.frame()

with(trial2, RMA.est(d = d, v = v, long = T))
with(trial2, TPSM.est(t = t, n1 = n1, n2 = n2, long = T))
estimate.onestep.selection.heterogeneous(z.obs=trial2$t, n1=trial2$n1, n2=trial2$n2, 
                                         alpha=0.05/2, 
                                         theta.init=c(expected.d = 0.3, max.tau= 0.5, p.report = 0.99))

# is this a case in which it might be biased upwards?
simLength <-  250

output <- NULL
for (i in 1:simLength) {
  # Make data
  trial <- dataMA(k = 10, delta = 0, tau = 0,
                  empN = T, selProp = 1, qrpEnv = "none") %>% 
    as.data.frame()
  
  # extract estimates
  rma_out <- with(trial, RMA.est(d = d, v = v, long = T)) %>% 
    filter(method == "reMA", term == "b0", variable == "estimate")
  tpsm_out <- with(trial, TPSM.est(t = t, n1 = n1, n2 = n2, long = T)) %>% 
    filter(method == "3PSM", term == "b0", variable == "estimate")
  # Tag with run number
  rma_out$trial <- i
  tpsm_out$trial <- i
  
  # Append to output
  output <- bind_rows(output, rma_out, tpsm_out)
}

out.wide <- spread(output, key = method, value = value)

ggplot(out.wide, aes(x = reMA, y = `3PSM`)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0)

ggplot(output, aes(x = method, y = value)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  ggtitle("delta = 0, selProp = 100%, tau = 0, k = 10, no QRPs")

# What about with QRPs? Still biased upwards? ----
simLength <-  100 # very time-consuming

output <- NULL
for (i in 1:simLength) {
  # Make data
  trial <- dataMA(k = 10, delta = 0, tau = 0,
                  maxN = 500, 
                  empN = T, selProp = 1, qrpEnv = "high") %>% 
    as.data.frame()
  
  # extract estimates
  rma_out <- with(trial, RMA.est(d = d, v = v, long = T)) %>% 
    filter(method == "reMA", term == "b0", variable == "estimate")
  tpsm_out <- with(trial, TPSM.est(t = t, n1 = n1, n2 = n2, long = T)) %>% 
    filter(method == "3PSM", term == "b0", variable == "estimate")
  # Tag with run number
  rma_out$trial <- i
  tpsm_out$trial <- i
  
  # Append to output
  output <- bind_rows(output, rma_out, tpsm_out)
}

out.wide <- spread(output, key = method, value = value)

ggplot(out.wide, aes(x = reMA, y = `3PSM`)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0)

ggplot(output, aes(x = method, y = value)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  ggtitle("delta = 0, selProp = 100%, tau = 0, k = 10, high QRPs")


# Does increased k fix this when selProp = 1? No. ----
simLength <-  250

output <- NULL
for (i in 1:simLength) {
  # Make data
  trial <- dataMA(k = 60, delta = 0, tau = 0,
                  empN = T, selProp = 1, qrpEnv = "none") %>% 
    as.data.frame()
  
  # extract estimates
  rma_out <- with(trial, RMA.est(d = d, v = v, long = T)) %>% 
    filter(method == "reMA", term == "b0", variable == "estimate")
  tpsm_out <- with(trial, TPSM.est(t = t, n1 = n1, n2 = n2, long = T)) %>% 
    filter(method == "3PSM", term == "b0", variable == "estimate")
  # Tag with run number
  rma_out$trial <- i
  tpsm_out$trial <- i
  
  # Append to output
  output <- bind_rows(output, rma_out, tpsm_out)
}

out.wide <- spread(output, key = method, value = value)

ggplot(out.wide, aes(x = reMA, y = `3PSM`)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0)

ggplot(output, aes(x = method, y = value)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  ggtitle("delta = 0, selProp = 100%, tau = 0, k = 10, no QRPs")
