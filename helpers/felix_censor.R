clamp <- function(x) {if (x > 1) x=1; if (x < 0) x = 0; return(x)}

#' @param posSign_NS_baseRate What's the probability that a p > .10 in the right direction enters the literature?
#' @param negSign_NS_baseRate What's the probability that a p > .05 in the wrong direction enters the literature?
#' @param direction +1: Expected direction, -1: wrong direction
censor <- function(pObs, direction, posSign_NS_baseRate = 0.3, negSign_NS_baseRate = 0.05){
	
  if(direction > 0 & pObs < .05){       #right direction, sig
    pubProb = 1
	#}else if(direction > 0 & pObs < .05){ #right direction, standard sig
    #pubProb = .98
  }else if(direction > 0 & pObs >= .05 & pObs <= .1){ #right direction, trending
    pubProb = 1-((1-posSign_NS_baseRate)/.05)*(pObs-.05)
  }else if (direction > 0 & pObs > .1){	# right direction; non-significant (p > .1)
    pubProb =posSign_NS_baseRate
  }else if(direction <= 0 & pObs < .01){	# wrong direction, highly sig.
    pubProb= 1
  }else if(direction <= 0 & pObs >= .01 & pObs < .05){ # wrong direction, standard sig.
    pubProb= 1-((1-negSign_NS_baseRate)/.04)*(pObs-.01)
  }else if(direction <= 0 & pObs >= .05){	# wrong direction, non-sig.
    pubProb=negSign_NS_baseRate
  }
  return(pubProb)
}


plotCensorFunction <- function(posSign_NS_baseRate = 0.3, negSign_NS_baseRate = 0.05) {
	pubProb <- data.frame()
	for (p in seq(0, 1, by=0.001)) {
		pubProb <- rbind(pubProb, data.frame(p=p, d=-1, facet="Wrong sign", pubProb=censor(p, -1, posSign_NS_baseRate, negSign_NS_baseRate)))
		pubProb <- rbind(pubProb, data.frame(p=p, d=+1, facet="Expected sign", pubProb=censor(p, +1, posSign_NS_baseRate, negSign_NS_baseRate)))
	}

	ann <- data.frame(
		facet = c("Wrong sign", "Expected sign"),
		label = c(negSign_NS_baseRate, posSign_NS_baseRate)
	)

	library(ggplot2)
	ggplot(pubProb, aes(x=p, y=pubProb)) + 
		geom_vline(xintercept=c(.01, .05, .1), linetype="dotted", color="grey20") + geom_line() + 
		facet_wrap(~facet) + theme_bw() + coord_cartesian(ylim=c(0, 1)) + 
		geom_text(data=ann, aes(label=label, y=label), x=0.5, vjust=-0.3) + ylab("Probability of publication") + xlab("p-value")
}

plotCensorFunction(0.30, 0.10)

plotCensorFunction(0.05, 0.01)


# ---------------------------------------------------------------------
# 

censor2 <- function(pObs, direction, posSign_NS_baseRate = 0.3, negSign_NS_baseRate1 = 0.05, negSign_NS_baseRate2 = 0.10){
	
  if(direction > 0 & pObs < .05){       #right direction, sig
    pubProb = 1
	#}else if(direction > 0 & pObs < .05){ #right direction, standard sig
    #pubProb = .98
  } else if(direction > 0 & pObs >= .05 & pObs <= .1){ #right direction, trending
    pubProb = 1-((1-posSign_NS_baseRate)/.05)*(pObs-.05)
  } else if (direction > 0 & pObs > .1){	# right direction; non-significant (p > .1)
    pubProb = posSign_NS_baseRate
  } else if(direction <= 0 & pObs < .01){	# wrong direction, highly sig.
    pubProb = 1
  } else if(direction <= 0 & pObs >= .01 & pObs < .05){ # storng decay to negSign_NS_baseRate1
    pubProb= 1-((1-negSign_NS_baseRate1)/.04)*(pObs-.01)
  } else if(direction <= 0 & pObs >= .05){	# increase to negSign_NS_baseRate2
    pubProb= negSign_NS_baseRate1 + pObs*(negSign_NS_baseRate2 - negSign_NS_baseRate1)
  }
		
  return(pubProb)
}

plotCensorFunction2 <- function(posSign_NS_baseRate = 0.3, negSign_NS_baseRate1 = 0.05, negSign_NS_baseRate2 = 0.15) {
	pubProb <- data.frame()
	for (p in seq(0, 1, by=0.001)) {
		pubProb <- rbind(pubProb, data.frame(p=p, d=-1, facet="Wrong sign", pubProb=censor2(p, -1, posSign_NS_baseRate, negSign_NS_baseRate1, negSign_NS_baseRate2)))
		pubProb <- rbind(pubProb, data.frame(p=p, d=+1, facet="Expected sign", pubProb=censor2(p, +1, posSign_NS_baseRate, negSign_NS_baseRate1, negSign_NS_baseRate2)))
	}

	ann <- data.frame(
		facet = c("Wrong sign", "Wrong sign", "Expected sign"),
		xPos = c(.12, .95, .5),
		y = c(negSign_NS_baseRate1, negSign_NS_baseRate2, posSign_NS_baseRate),
		label = c(negSign_NS_baseRate1, negSign_NS_baseRate2, posSign_NS_baseRate)
	)

	library(ggplot2)
	ggplot(pubProb, aes(x=p, y=pubProb)) + 
		geom_vline(xintercept=c(.01, .05, .1), linetype="dotted", color="grey20") + geom_line() + 
		facet_wrap(~facet) + theme_bw() + coord_cartesian(ylim=c(0, 1)) + ylab("Probability of publication") + xlab("p-value") +
		geom_text(data=ann, aes(label=label, x=xPos, y=y), vjust=-0.3)
}

plotCensorFunction2(0.25, 0.03, 0.10)
plotCensorFunction2(0.03, 0.005, 0.01)



# ---------------------------------------------------------------------
# 


pH1 <- 1				# rate of true hypotheses
alpha <- .05		# alpha
Pow <- .35			# average power
selProp <- .05	# probability that a nonsig. result enters the literature

percSig <- (Pow*pH1) + (alpha*(1-pH1))
publishedStudies <- percSig + (1-percSig)*selProp
sigRatio <- percSig / publishedStudies


# solve for selProp:
selProp <- (((pH1-1)*percSig - pH1 + 1) * alpha - pH1*Pow*percSig + pH1*Pow) / ((pH1-1)*percSig*alpha + (1 - pH1*Pow)*percSig)
selProp


pH1 <- 0.3				# rate of true hypotheses
alpha <- .05		# alpha
Pow <- .5			# average power
percSig <- .90

selProp <- (((pH1-1)*percSig - pH1 + 1) * alpha - pH1*Pow*percSig + pH1*Pow) / ((pH1-1)*percSig*alpha + (1 - pH1*Pow)*percSig)
selProp


# ---------------------------------------------------------------------
#

X1 <- simMA(500, 0.2, .1, "med", censorFunc="0", verbose=TRUE)
X2 <- simMA(500, 0.2, .1, "med", censorFunc="A", verbose=TRUE)
X3 <- simMA(500, 0.2, .1, "med", censorFunc="B", verbose=TRUE)

mean(X1$d)
mean(X2$d)
mean(X3$d)

prop.table(table(X1$p < .05))
prop.table(table(X2$p < .05))
prop.table(table(X3$p < .05))
