#Set k to the number of effect sizes.
k <- length(y)

#FE mean-only model.
w <- 1/v
FEmean <- sum(w*y)/sum(w)

Qwithinmeanonly <- function(FEmean) {
   resid <- y-FEmean
   w <- 1/v
   return(sum(w*resid^2)) 
 }

Qw <- Qwithinmeanonly(FEmean)
dfQw <- k-1
pQw <- 1-pchisq(Qw,dfQw)

#I-squared.
I2 <- 100*((Qw-(k-1))/Qw)
