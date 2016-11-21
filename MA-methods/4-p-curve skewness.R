# ---------------------------------------------------------------------
# These p-curve functions are partially copied, partially adapted from Uri Simonsohn's (uws@wharton.upenn.edu) original p-curve functions
# http://p-curve.com/Supplement/Rcode_other/R%20Code%20behind%20p-curve%20app%203.0%20-%20distributable.R


clamp <- function(x, MIN=.00001, MAX=.99999) {x[x<MIN] <- MIN; x[x>MAX] <- MAX; x}

# ---------------------------------------------------------------------
# p-curve-app 3.0 functions

# functions that find noncentrality parameter for t,f,chi distributions that gives 33% power for those d.f.

#t-test
ncp33t <- function(df, power=1/3, p.crit=.05) {      
      xc = qt(p=1-p.crit/2, df=df)
      #Find noncentrality parameter (ncp) that leads 33% power to obtain xc
	  f = function(delta, pr, x, df) pt(x, df = df, ncp = delta) - (1-power)
	  out = uniroot(f, c(0, 37.62), x = xc, df = df)  
	  return(out$root) 
}


ncp33z <- function(power=1/3, p.crit=.05) {      
      xc = qnorm(p=1-p.crit/2)
      #Find noncentrality parameter (ncp) that leads 33% power to obtain xc
	  f = function(delta, pr, x) pnorm(x, mean = delta) - (1-power)
	  out = uniroot(f, c(0, 37.62), x = xc)
	  return(out$root) 
}


#F-test
ncp33f <- function(df1, df2, power=1/3, p.crit=.05) {      
	xc=qf(p=1-p.crit,df1=df1,df2=df2)
	f = function(delta, pr, x, df1,df2) pf(x, df1 = df1, df2=df2, ncp = delta) - (1-power)
	out = uniroot(f, c(0, 37.62), x = xc, df1=df1, df2=df2)  
	return(out$root)       
}

#chi-square
ncp33chi <- function(df, power=1/3, p.crit=.05) {      
	xc=qchisq(p=1-p.crit, df=df)
	#Find noncentrality parameter (ncp) that leads 33% power to obtain xc
	f = function(delta, pr, x, df) pchisq(x, df = df, ncp = delta) - (1-power)
	out = uniroot(f, c(0, 37.62), x = xc, df = df)  
	return(out$root)
}



get_pp_values <- function(type, statistic, df, df2, p.crit=.05, power=1/3) {

    # convert r to t values
	type <- as.character(type)
	statistic[tolower(type)=="r"] <- statistic[tolower(type)=="r"] / sqrt( (1 - statistic[tolower(type)=="r"]^2) / df[tolower(type)=="r"])
	type[tolower(type)=="r"] <- "t"
	
	statistic <- abs(statistic)

	res <- data.frame()
	ncp <- data.frame()
	for (i in 1:length(type)) {
		switch(tolower(type[i]), 
			"t" = {
				p <- 2*(1-pt(abs(statistic[i]),df=df[i]))
				ppr <- p*(1/p.crit)	# pp-value for right-skew 
				ppl <- 1-ppr		# pp-value for left-skew
	  	        ncp33 <- ncp33t(df[i], power=power, p.crit=p.crit)
	  	        pp33 <- (pt(statistic[i],  df=df[i], ncp=ncp33)-(1-power))*(1/power)
				},
			"f" = {
				p <- 1-pf(abs(statistic[i]), df1=df[i], df2=df2[i])
				ppr <- p*(1/p.crit)	# pp-value for right-skew 
				ppl <- 1-ppr		# pp-value for left-skew
	  	        ncp33 <- ncp33f(df1=df[i], df2=df2[i], power=power, p.crit=p.crit)
	  	        pp33 <- (pf(statistic[i], df1=df[i], df2=df2[i],  ncp=ncp33)-(1-power))*(1/power)
				},
			"z" = {
				p <- 2*(1-pnorm(abs(statistic[i])))
				ppr <- p*(1/p.crit)	# pp-value for right-skew 
				ppl <- 1-ppr		# pp-value for left-skew
				
				ncp33 <- ncp33z(power=power, p.crit=p.crit)
	  	        pp33 <- (pnorm(statistic[i], mean=ncp33, sd=1)-(1-power))*(1/power)
				},	
			"p" = {
				p <- statistic[i]
				z <- qnorm(p/2, lower.tail=FALSE)
				ppr <- p*(1/p.crit)	# pp-value for right-skew 
				ppl <- 1-ppr		# pp-value for left-skew
			
				ncp33 <- ncp33z(power=power, p.crit=p.crit)
	  	        pp33 <- (pnorm(z, mean=ncp33, sd=1)-(1-power))*(1/power)
				},		
			"chi2" = {
				p <- 1-pchisq(abs(statistic[i]), df=df[i])
				ppr <- p*(1/p.crit)	# pp-value for right-skew 
				ppl <- 1-ppr		# pp-value for left-skew
	  	        ncp33 <- ncp33chi(df[i], power=power, p.crit=p.crit)
	  	        pp33 <- (pchisq(statistic[i],  df=df[i], ncp=ncp33)-(1-power))*(1/power)
			},
			{
				# default
				warning(paste0("Test statistic ", type[i], " not suported by p-curve."))
			}
		)
		res <- rbind(res, data.frame(p=p, ppr=ppr, ppl=ppl, pp33=pp33))
		ncp <- rbind(ncp, data.frame(type=type[i], df=df[i], df2=df2[i], ncp=ncp33))
	}
		
	if (nrow(res) > 0) {
		# clamp to extreme values	
		res$ppr <- clamp(res$ppr, MIN=.00001, MAX=.99999)
		res$ppl <- clamp(res$ppl, MIN=.00001, MAX=.99999)
		res$pp33 <- clamp(res$pp33, MIN=.00001, MAX=.99999)
		
		# remove non-significant values
		res[res$p > p.crit, ] <- NA
	
		return(list(res=res, ncp=ncp))
	} else {
		return(NULL)
	}
}





# ---------------------------------------------------------------------
# New p-curve computation (p-curve app 3.0, http://www.p-curve.com/app3/)
p_curve_3 <- function(pps) {

	pps <- na.omit(pps)

	# STOUFFER: Overall tests aggregating pp-values
	ktot <- sum(!is.na(pps$ppr))
	Z_ppr <- sum(qnorm(pps$ppr))/sqrt(ktot)          # right skew
	Z_ppl <- sum(qnorm(pps$ppl))/sqrt(ktot)          # left skew
	Z_pp33<- sum(qnorm(pps$pp33))/sqrt(ktot)         # 33%
	
	p_ppr <- pnorm(Z_ppr)
	p_ppl <- pnorm(Z_ppl)
	p_pp33<- pnorm(Z_pp33)

	return(list(
		Z_evidence = Z_ppr, 
		p_evidence = p_ppr, 
		Z_hack = Z_ppl, 
		p_hack = p_ppl, 
		Z_lack = Z_pp33, 
		p_lack = p_pp33,
		inconclusive = ifelse(p_ppr>.05 & p_ppl>.05 & p_pp33>.05, TRUE, FALSE)))
}


# ---------------------------------------------------------------------
# Old p-curve computation (p-curve app 2.0, http://www.p-curve.com/app2/)
p_curve_2 <- function(pps) {

	pps <- na.omit(pps)
	
	df <- 2*sum(nrow(pps))

	chi2_evidence <- -2*sum(log(pps$ppr), na.rm=TRUE)
	p_evidence <- pchisq(chi2_evidence, df=df, lower.tail=FALSE)

	chi2_hack <- -2*sum(log(pps$ppl), na.rm=TRUE)
	p_hack <- pchisq(chi2_hack, df=df, lower.tail=FALSE)

	chi2_lack <- -2*sum(log(pps$pp33), na.rm=TRUE)
	p_lack <- pchisq(chi2_lack, df=df, lower.tail=FALSE)

	return(list(
		chi2_evidence = chi2_evidence, 
		p_evidence = p_evidence, 
		chi2_hack = chi2_hack, 
		p_hack = p_hack, 
		chi2_lack = chi2_lack, 
		p_lack = p_lack,
		df = df, 
		inconclusive = ifelse(p_evidence>.05 & p_hack>.05 & p_lack>.05, TRUE, FALSE)))
}



theoretical_power_curve <- function(power=1/3, p.max=.05, normalize=TRUE) {
	# compute arbitrary test statistics for requested power
	library(pwr)
	d <- 0.2
	n <- pwr.t.test(d=0.2, power=power)$n*2
	
	crit <- seq(0.01, p.max, by=.01)
	pdens <- c()
	for (cr in crit) {
		pdens <- c(pdens, pwr.t.test(d=0.2, power=NULL, n=n/2, sig.level=cr)$power)
	}
	p.dens <- diff(c(0, pdens))
	if (normalize == TRUE) p.dens <- p.dens/sum(p.dens)
		
	names(p.dens) <- as.character(crit)
	return(p.dens)
}


pc_skew <- function(t.value, df, long=TRUE) {
	
	# only select directionally consistent effects
	df <- df[t.value > 0]
	t.value <- t.value[t.value > 0]
	
	if (length(t.value) >= 1) {
	
		pp <- get_pp_values(type=rep("t", length(t.value)), statistic=t.value, df=df, df2=NA)
	
		PC_skew <- p_curve_3(pp$res)

		PC_skew.long <- melt(data.frame(PC_skew[1:6]), id.var=NULL)
	
		res <- data.frame(
			method=c("pcurve.evidence", "pcurve.hack", "pcurve.lack"),
			term = "skewtest",
			statistic = PC_skew.long$value[c(1, 3, 5)],
			p.value = PC_skew.long$value[c(2, 4, 6)]
		)
		
		res <- plyr::rbind.fill(res, data.frame(
			method=c("pcurve.evidence", "pcurve.hack", "pcurve.lack"),
			term="kSig",
			estimate=sum(!is.na(pp$res$p))
		))
	} else {
		res <- data.frame(
			method=c("pcurve.evidence", "pcurve.hack", "pcurve.lack"),
			term="kSig",
			estimate=0
		)
	}
			
    returnRes(res, long)
}

