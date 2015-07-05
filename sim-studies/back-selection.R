# Select a number of published studies and sample statistics according
  # to effect size, n, and bias.
library(truncdist)
library(dplyr)

gen_k_studies = function(k, percent_sig, avg_n, 
                         min_n = 40, max_n = Inf, d_true=0,
                         tails = 2) {
  n_per_cell = 
    rtrunc(k, "lnorm", a = min_n, b = max_n, 
           meanlog = log(avg_n), sdlog = .5) %>% 
    round(0)  
  df = n_per_cell * 2 - 2
  
  # Run studies
  # minimum significant t-value
  if (tails == 1) {
    tcrit = qt(.95, df)
  }  else if (tails == 2) tcrit = qt(.975, df)
  # make a vector 'lower_bound' that is equal to
  # -Inf if the study is not necessarily significant,
  # tcrit if the study must be significant.
  sig = rbinom(k, size = 1, prob = percent_sig) + 1 # 1 if false, 2 if true
  lower_bound = c(-Inf, 1)[sig]
  lower_bound = lower_bound * tcrit
  # Ditto 'upperbound'. Nonsig studies not allowed to have sig result.
  upper_bound = c(1, Inf)[sig]
  upper_bound = upper_bound * tcrit
  # sample a t-value coerced to be significant or nonsignificant.
    #rtrunc() barks but seems to work fine
  t = rtrunc(k, "t", a = lower_bound, b = upper_bound,
             df = df,
             ncp = sqrt(n_per_cell/2)*d_true) # correct equation for ncp?
  d=2*t/sqrt(2*n_per_cell)
  var_d = ( (2*n_per_cell / (n_per_cell^2)) + d^2 / 2*df ) * (2*n_per_cell / df)
  se_d = sqrt(var_d)
  p=2*pt(t, df=df, lower.tail = F)
  # output
  out = data.frame("Study" = 1:k, "nobs" = n_per_cell, 
                   "dobs" = d, "sedobs" = se_d, "vardobs" = var_d,
                   "t" = t, "p" = p)
}

# Demonstration ----
# Generate 20 significant results on a null effect
biased_meta = gen_k_studies(k = 20, percent_sig = 1, avg_n = 30, d_true = 0)
plot(biased_meta$dobs, biased_meta$nobs, xlim = c(-.3, .7))
subset(biased_meta$p, biased_meta$p < .05) %>% 
  hist(breaks=seq(0, .05, by=.01))
# Generate 30 studies on a null effect, of which 75% are forced to be significant
biased_meta = gen_k_studies(k = 30, percent_sig = .75, avg_n = 30, d_true = 0)
plot(biased_meta$dobs, biased_meta$nobs, xlim = c(-.3, .7))
subset(biased_meta$p, biased_meta$p < .05) %>% 
  hist(breaks=seq(0, .05, by=.01))
# Generate 40 studies on a small effect with heavy pub bias
biased_meta = gen_k_studies(k = 40, percent_sig = .80, avg_n = 30, d_true = .3)
plot(biased_meta$dobs, biased_meta$nobs, xlim = c(-.3, .9))
subset(biased_meta$p, biased_meta$p < .05) %>% 
  hist(breaks=seq(0, .05, by=.01))
