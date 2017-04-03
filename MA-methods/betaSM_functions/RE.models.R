#Negative likelihood functions of the random-effects models.

#Unadjusted mean-only model.
neglikeunadjustedRE <- function(unadjustedREpars, y, v) {
   vc2 <- unadjustedREpars[1]
   beta02 <- unadjustedREpars[2]
   thisone <- 1/2*log(v+vc2) + 1/2*((y-beta02)^2/(v+vc2))
   return(sum(thisone)) 
 }

#Adjusted mean-only beta density weight-function model.
denfxn <- function(yy,vv,a2,b2,vc2,beta02) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,beta02,sqrt(vv+vc2))
   return(part1*part2*part3) 
}

# adjustedREpars are to be optimized
# y, v, p are fixed parameter vectors
neglikeadjustedRE <- function(adjustedREpars, y, v, p) {
   a2 <- adjustedREpars[1]
   b2 <- adjustedREpars[2]
   vc2 <- adjustedREpars[3]
   beta02 <- adjustedREpars[4]

   num <- sum( (a2-1)*log(p) + 
             (b2-1)*log(1-p) - 
             1/2*log(v+vc2) - 1/2*((y-beta02)^2/(v+vc2)) )

   total <- rep(0,length(y))
	 
   for(i in 1:length(y)) {
      vv = v[i]
      yy = y[i]
      answer <- integrate(denfxn,-Inf,Inf,vv=vv,a2=a2,b2=b2,vc2=vc2,beta02=beta02)
      bottom <- .00001*denfxn(-Inf,vv,a2,b2,vc2,beta02)
      top <- .00001*denfxn(Inf,vv,a2,b2,vc2,beta02)
      total[i] <- log( answer$value + bottom + top ) }
   den <- sum(total)

	 return(-num+den) 
}


