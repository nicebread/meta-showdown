#Note: fxnA is the same as denfxn.
fxnA <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   return(part1*part2*part3) }

fxnda <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- log(pp)
   return(part1*part2*part3*part4) }

fxndb <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- log(1-pp)
   return(part1*part2*part3*part4) }

fxndvc <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- ((yy-mu2)^2)/(2*(vv+vc2)^2)
   return(part1*part2*part3*part4) }

fxndmu <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- (yy-mu2)/(vv+vc2)
   return(part1*part2*part3*part4) }

fxnda2 <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- log(pp)^2
   return(part1*part2*part3*part4) }

fxndb2 <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- log(1-pp)^2
   return(part1*part2*part3*part4) }

fxndvc2 <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- ( ((yy-mu2)^2)/(2*(vv+vc2)^2) )^2
   part5 <- ((yy-mu2)^2)/((vv+vc2)^3)
   return(part1*part2*part3*part4 - part1*part2*part3*part5) }

fxndmu2 <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- ( (yy-mu2)/(vv+vc2) )^2
   part5 <- 1/(vv+vc2)
   return(part1*part2*part3*part4 - part1*part2*part3*part5) }

fxndadb <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- log(pp)*log(1-pp)
   return(part1*part2*part3*part4) }

fxndadvc <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- log(pp)*(((yy-mu2)^2)/(2*(vv+vc2)^2))
   return(part1*part2*part3*part4) }

fxndadmu <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- log(pp)*((yy-mu2)/(vv+vc2))
   return(part1*part2*part3*part4) }

fxndbdvc <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- log(1-pp)*(((yy-mu2)^2)/(2*(vv+vc2)^2))
   return(part1*part2*part3*part4) }

fxndbdmu <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- log(1-pp)*((yy-mu2)/(vv+vc2))
   return(part1*part2*part3*part4) }

fxndvcdmu <- function(yy,vv,a2,b2,vc2,mu2) {
   pp <- 1-pnorm(yy/sqrt(vv))
   pp <- ifelse(pp < .00001,.00001,pp)
   pp <- ifelse(pp > .99999,.99999,pp)
   part1 <- pp^(a2-1)
   part2 <- (1-pp)^(b2-1)
   part3 <- dnorm(yy,mu2,sqrt(vv+vc2))
   part4 <- (((yy-mu2)^2)/(2*(vv+vc2)^2))*((yy-mu2)/(vv+vc2))
   part5 <- (yy-mu2)/((vv+vc2)^2)
   return(part1*part2*part3*part4 - part1*part2*part3*part5) }