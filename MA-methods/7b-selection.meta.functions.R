## ======================================================================
## Code from supplementary material of McShane, B. B., Bo ckenholt, U., & Hansen, K. T. (2016). Adjusting for Publication Bias in Meta-Analysis: An Evaluation of Selection Methods and Some Cautionary Notes. Perspectives on Psychological Science, 11(5), 730–749. http://doi.org/10.1177/1745691616662243
## ======================================================================


# These functions all assume one-sided selection (i.e., direction matters).
# Without loss of generality, they also assume right-sided selection (i.e., positive is "correct").
# alpha denotes stated nominal one-sided Type I error.
#
# It is trivial to generalize these functions to accomodate two-sided selection.
# 
# The selection model specification is particularly simple (i.e., a step function with a single step at alpha).
# Thus, this is equivalent to:
#		No selection: w=1
#		Hedges 1984 (one-sided, extreme selection): w=0
#		Iyengar 1988 (one-sided): w estimated
# Clearly far more complicated functions are possible and have been used.


gsi <- c(-0.5, 2.0)
cat("Grid search interval for estimated homogeneous one-step models when all or no studies are significant is hard-coded:", gsi, "\n")


# Master homogeneous negative log likelihood function:
onestep.homogeneous.nll <- function(theta, t.obs, n1, n2, alpha=0.025){
	delta <- theta[1]						# True effect size
	w <- theta[2]							# Relative likelihood non-stat sig study is reported

	df <- n1 + n2 - 2
	t.cut <- qt(1-alpha,df=df)
	k <- sum(t.obs<t.cut)
	lambda <- delta * sqrt((n1*n2)/(n1+n2))
	
	ll <- ifelse(k==0, 0, k*log(w))			# Equivalent to defining 0*log(0)=0 as is common
	ll <- ll + sum(dt(t.obs, df=df, ncp=lambda, log=TRUE))
	ll <- ll - sum(log( w*pt(t.cut,df=df,ncp=lambda) + (1 - pt(t.cut,df=df,ncp=lambda)) ))
	-ll	
}

# Master heterogeneous negative log likelihood function:
# We use a z approximation to the likelihood because using normal mixture of non-central t distributions
# requires two numerical integrations per datapoint which is too computationally expensive but this 
# integration is analytic with the z approximation. Plus, a normal mixture of non-central t distributions
# is relatively normal.
onestep.heterogeneous.nll <- function(theta, z.obs, n1, n2, alpha=0.025){
	delta <- theta[1]						# True population average effect size
	tau <- theta[2]							# Heterogeneity
	w <- theta[3]							# Relative likelihood non-stat sig study is reported
	z.cut <- qnorm(1-alpha)
	d.obs <- z.obs / sqrt((n1*n2)/(n1+n2))
	d.cut <- z.cut / sqrt((n1*n2)/(n1+n2))
	k <- sum(z.obs<z.cut)

	s <- sqrt(tau^2 + 1/n1 + 1/n2)
	ll <- ifelse(k==0, 0, k*log(w))			# Equivalent to defining 0*log(0)=0 as is common
	ll <- ll + sum(dnorm(d.obs, delta, s, log=TRUE))
	ll <- ll - sum(log( w*pnorm(d.cut,delta,s) + (1 - pnorm(d.cut,delta,s)) ))
	-ll
}

# Homogeneous estimation functions:
estimate.no.selection.homogeneous <- function(t.obs,n1,n2,interval){
	tmpf <- function(delta,t.obs,n1,n2){ onestep.homogeneous.nll(c(delta,1), t.obs, n1, n2)  }
	tmpo <- optimize(tmpf, interval, t.obs=t.obs, n1=n1, n2=n2)
	tmph <- optimHess(tmpo$minimum, tmpf, t.obs=t.obs, n1=n1, n2=n2)
	return(c(tmpo$minimum, sqrt(1/tmph), -tmpo$objective))	
}
estimate.extreme.selection.homogeneous <- function(t.obs,n1,n2,alpha,interval){
	p.value <- 1 - pt(t.obs, df=n1+n2-2)
	sel <- t.obs > 0 & p.value < alpha
	if(sum(sel)==0){ return(c(NA,NA,NA)) }
	
	t.obs <- t.obs[sel]
	n1 <- n1[sel]
	n2 <- n2[sel]

	tmpf <- function(delta,t.obs,n1,n2,alpha){ onestep.homogeneous.nll(c(delta,0), t.obs, n1, n2, alpha) }
	tmpo <- optimize(tmpf, interval, t.obs=t.obs, n1=n1, n2=n2, alpha=alpha)
	tmph <- optimHess(tmpo$minimum, tmpf, t.obs=t.obs, n1=n1, n2=n2, alpha=alpha)
	return(c(tmpo$minimum, sqrt(1/tmph), -tmpo$objective))	
}
estimate.onestep.selection.homogeneous <- function(t.obs,n1,n2,alpha,theta.init){
	p.value <- 1 - pt(t.obs, df=n1+n2-2)
	sel <- t.obs > 0 & p.value < alpha

	if(sum(sel)==0 | sum(sel)==length(sel)){
		if(sum(sel)==length(sel)){ w <- 1/(length(sel)+2) }		# Not identified; use Wilson-like
		if(sum(sel)==0){ w <- 1 - 1/(length(sel)+2) }			# estimator.
		e <- estimate.known.onestep.selection.homogeneous(t.obs,n1,n2,w,alpha, gsi)
		return(list(est=c(e[1],w), est.var=matrix(c(e[2]^2,NA,NA,NA),2,2), ll=e[3]))
	}
	
	if(sum(sel) > 0 & sum(sel)<length(sel)){
		theta.init <- c(theta.init[1], log(theta.init[2]))
		tmpf <- function(theta,t.obs,n1,n2,alpha){ onestep.homogeneous.nll(c(theta[1],exp(theta[2])),t.obs,n1,n2,alpha) }
		tmpg <- function(theta,t.obs,n1,n2,alpha){ onestep.homogeneous.nll(c(theta[1],theta[2]),t.obs,n1,n2,alpha) }
		tmpo <- optim(theta.init, tmpf, t.obs=t.obs, n1=n1, n2=n2, alpha=alpha)
		theta.hat <- c(tmpo$par[1], exp(tmpo$par[2]))
		tmpv <- matrix(NA, 2, 2)
		suppressWarnings(try( tmpv <- solve(optimHess(theta.hat, tmpg, t.obs=t.obs, n1=n1, n2=n2, alpha=alpha)), silent=TRUE))
		return(list(est=theta.hat, est.var=tmpv, ll=-tmpo$value))
	}	
}
estimate.known.onestep.selection.homogeneous <- function(t.obs,n1,n2,w,alpha,interval){
	tmpf <- function(delta,w,t.obs,n1,n2,alpha){ onestep.homogeneous.nll(c(delta,w), t.obs, n1, n2, alpha) }
	tmpo <- optimize(tmpf, interval, w=w, t.obs=t.obs, n1=n1, n2=n2, alpha=alpha)
	tmph <- optimHess(tmpo$minimum, tmpf, w=w, t.obs=t.obs, n1=n1, n2=n2, alpha=alpha)
	return(c(tmpo$minimum, sqrt(1/tmph), -tmpo$objective))	
}

# Homogeneous estimation functions (using a z approximation; this is simply for comparison purposes):
estimate.z.no.selection.homogeneous <- function(z.obs,n1,n2,interval){
	tmpf <- function(delta,z.obs,n1,n2){ onestep.heterogeneous.nll(c(delta,0,1), z.obs, n1, n2)  }
	tmpo <- optimize(tmpf, interval, z.obs=z.obs, n1=n1, n2=n2)
	tmph <- optimHess(tmpo$minimum, tmpf, z.obs=z.obs, n1=n1, n2=n2)
	return(c(tmpo$minimum, sqrt(1/tmph), -tmpo$objective))	
}
estimate.z.extreme.selection.homogeneous <- function(z.obs,n1,n2,alpha,interval){
	p.value <- 1 - pnorm(z.obs)
	sel <- z.obs > 0 & p.value < alpha
	if(sum(sel)==0){ return(c(NA,NA,NA)) }
	
	z.obs <- z.obs[sel]
	n1 <- n1[sel]
	n2 <- n2[sel]

	tmpf <- function(delta,z.obs,n1,n2,alpha){ onestep.heterogeneous.nll(c(delta,0,0), z.obs, n1, n2, alpha) }
	tmpo <- optimize(tmpf, interval, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)
	tmph <- optimHess(tmpo$minimum, tmpf, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)
	return(c(tmpo$minimum, sqrt(1/tmph), -tmpo$objective))	
}
estimate.z.onestep.selection.homogeneous <- function(z.obs,n1,n2,alpha,theta.init){
	p.value <- 1 - pnorm(z.obs)
	sel <- z.obs > 0 & p.value < alpha
	
	if(sum(sel)==0 | sum(sel)==length(sel)){
		if(sum(sel)==length(sel)){ w <- 1/(length(sel)+2) }		# Not identified; use Wilson-like
		if(sum(sel)==0){ w <- 1 - 1/(length(sel)+2) }			# estimator.
		e <- estimate.z.known.onestep.selection.homogeneous(z.obs,n1,n2,w,alpha, gsi)
		return(list(est=c(e[1],w), est.var=matrix(c(e[2]^2,NA,NA,NA),2,2), ll=e[3]))
	}

	if(sum(sel) > 0 & sum(sel)<length(sel)){
		theta.init <- c(theta.init[1], log(theta.init[2]))
		tmpf <- function(theta,z.obs,n1,n2,alpha){ onestep.heterogeneous.nll(c(theta[1],0,exp(theta[2])),z.obs,n1,n2,alpha) }
		tmpg <- function(theta,z.obs,n1,n2,alpha){ onestep.heterogeneous.nll(c(theta[1],0,theta[2]),z.obs,n1,n2,alpha) }
		tmpo <- optim(theta.init, tmpf, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)
		theta.hat <- c(tmpo$par[1], exp(tmpo$par[2]))
		tmpv <- matrix(NA, 2, 2)
		suppressWarnings(try( tmpv <- solve(optimHess(theta.hat, tmpg, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)), silent=TRUE))
		return(list(est=theta.hat, est.var=tmpv, ll=-tmpo$value))
	}	
}
estimate.z.known.onestep.selection.homogeneous <- function(z.obs,n1,n2,w,alpha,interval){
	tmpf <- function(delta,w,z.obs,n1,n2,alpha){ onestep.heterogeneous.nll(c(delta,0,w), z.obs, n1, n2, alpha) }
	tmpo <- optimize(tmpf, interval, w=w, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)
	tmph <- optimHess(tmpo$minimum, tmpf, w=w, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)
	return(c(tmpo$minimum, sqrt(1/tmph), -tmpo$objective))	
}

# Heterogeneous estimation functions:
estimate.no.selection.heterogeneous <- function(z.obs,n1,n2,theta.init){
	theta.init <- c(theta.init[1], log(theta.init[2]), 0)
	tmpf <- function(theta,z.obs,n1,n2,alpha){ onestep.heterogeneous.nll(c(theta[1],exp(theta[2]),1),z.obs,n1,n2,alpha) }
	tmpg <- function(theta,z.obs,n1,n2,alpha){ onestep.heterogeneous.nll(c(theta[1],theta[2],1),z.obs,n1,n2,alpha) }
	tmpo <- optim(theta.init, tmpf, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)
	theta.hat <- c(tmpo$par[1], exp(tmpo$par[2]))
	tmpv <- matrix(NA, 2, 2)
	suppressWarnings(try( tmpv <- solve(optimHess(theta.hat, tmpg, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)), silent=TRUE))
	return(list(est=theta.hat, est.var=tmpv, ll=-tmpo$value))
}
estimate.extreme.selection.heterogeneous <- function(z.obs,n1,n2,alpha,theta.init){
	# cat("Caution: Theoretically unstable. Do not use.\n")		# I provide the code for the user.
	#p.value <- 1 - pnorm(z.obs)
	#sel <- z.obs > 0 & p.value < alpha
	#if(sum(sel)==0){ return(list(NA,NA,NA)) }
	
	#z.obs <- z.obs[sel]
	#n1 <- n1[sel]
	#n2 <- n2[sel]

	#theta.init <- c(theta.init[1], log(theta.init[2]))
	#tmpf <- function(theta,z.obs,n1,n2,alpha){ onestep.heterogeneous.nll(c(theta[1],exp(theta[2]),0),z.obs,n1,n2,alpha) }
	#tmpg <- function(theta,z.obs,n1,n2,alpha){ onestep.heterogeneous.nll(c(theta[1],theta[2],0),z.obs,n1,n2,alpha) }
	#tmpo <- optim(theta.init, tmpf, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)
	#theta.hat <- c(tmpo$par[1], exp(tmpo$par[2]))
	#tmpv <- matrix(NA, 2, 2)
	#suppressWarnings(try( tmpv <- solve(optimHess(theta.hat, tmpg, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)), silent=TRUE))
	#return(list(est=theta.hat, est.var=tmpv, ll=-tmpo$value))
}
estimate.onestep.selection.heterogeneous <- function(z.obs,n1,n2,alpha,theta.init){
	p.value <- 1 - pnorm(z.obs)
	sel <- z.obs > 0 & p.value < alpha

	if(sum(sel)==0 | sum(sel)==length(sel)){
		if(sum(sel)==length(sel)){ w <- 1/(length(sel)+2) }		# Not identified; use Wilson-like
		if(sum(sel)==0){ w <- 1 - 1/(length(sel)+2) }			# estimator.
		e <- estimate.known.onestep.selection.heterogeneous(z.obs,n1,n2,w,alpha, theta.init[1:2])
		tmp <- e[[2]]
		tmp <- cbind(tmp, c(NA,NA))
		tmp <- rbind(tmp, c(NA,NA,NA))
		return(list(est=c(e[[1]],w), est.var=tmp, ll=e[[3]]))
	}

	if(sum(sel) > 0 & sum(sel)<length(sel)){
		theta.init <- c(theta.init[1], log(theta.init[2:3]))
		tmpf <- function(theta,z.obs,n1,n2,alpha){ onestep.heterogeneous.nll(c(theta[1],exp(theta[2:3])),z.obs,n1,n2,alpha) }
		tmpg <- function(theta,z.obs,n1,n2,alpha){ onestep.heterogeneous.nll(c(theta[1],theta[2:3]),z.obs,n1,n2,alpha) }
		tmpo <- optim(theta.init, tmpf, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)
		theta.hat <- c(tmpo$par[1], exp(tmpo$par[2:3]))
		tmpv <- matrix(NA, 3, 3)
		suppressWarnings(try( tmpv <- solve(optimHess(theta.hat, tmpg, z.obs=z.obs, n1=n1, n2=n2, alpha=alpha)), silent=TRUE))
		return(list(est=theta.hat, est.var=tmpv, ll=-tmpo$value))
	}	
}
estimate.known.onestep.selection.heterogeneous <- function(z.obs,n1,n2,w,alpha,theta.init){
	theta.init <- c(theta.init[1], log(theta.init[2]))
	tmpf <- function(theta,z.obs,n1,n2,w,alpha){ onestep.heterogeneous.nll(c(theta[1],exp(theta[2]),w),z.obs,n1,n2,alpha) }
	tmpg <- function(theta,z.obs,n1,n2,w,alpha){ onestep.heterogeneous.nll(c(theta[1],theta[2],w),z.obs,n1,n2,alpha) }
	tmpo <- optim(theta.init, tmpf, z.obs=z.obs, n1=n1, n2=n2, w=w, alpha=alpha)
	theta.hat <- c(tmpo$par[1], exp(tmpo$par[2]))
	tmpv <- matrix(NA, 2, 2)
	suppressWarnings(try( tmpv <- solve(optimHess(theta.hat, tmpg, z.obs=z.obs, n1=n1, n2=n2, w=w, alpha=alpha)), silent=TRUE))
	return(list(est=theta.hat, est.var=tmpv, ll=-tmpo$value))
}






