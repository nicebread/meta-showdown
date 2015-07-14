library(ggplot2)

dat <- read.csv("npact dat.csv")
summary(dat)

social <- subset(dat,  Journal == "JESP"  | Journal == "JPSP" & JPSP_Section < 3 )
summary(social)

social$nper <- social$reis_n/2


ns <- social$nper

ggplot(social, aes(x= nper)) +
  theme_bw() +
  geom_histogram(aes(y=..density..), color="darkgrey", fill="grey60", alpha=.2) + 
  geom_density() +
  xlim(c(0, 300)) +
  stat_function(fun = dnbinom, size=1, color='hotpink', args=list(size=2.3, mu=48)) ## this gives us the parameters to pick N


