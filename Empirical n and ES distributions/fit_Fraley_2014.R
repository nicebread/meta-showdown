### functions for truncated distributions -->should be in package truncdist!


#### get appropriate ES from Richard et al (2003) paper. convert r to d ####

r <- rtrunc(1, spec="beta", a=.08, b=.14, 1.34, 5.03 )
dtrue <- 2*r/sqrt(1-r^2) # this will select effect sizes that hit the social psych "sweet spot." it runs from 20th to 40th percentiles. This is roughly from the mode up a bit.


#### Get sample sizes. ####
# I pulled the raw data from Fraley & Vazire's n-pact factor paper, then selected the social psych journals (JESP, the first 2 sections of JPSP, and SPPS)
# these were total N per study. To be generous, I assumed that they were all 2 group designs, then used this function to randomly sample a per-group n from that distribution

rtrunc(n=1, spec="nbinom", a=10, b=Inf, size=2.3, mu=48)

