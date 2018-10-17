# helper functions for censor function

# clamp x to values between 0 and 1
clamp <- function(x) {if (x > 1) x=1; if (x < 0) x = 0; return(x)}


#' @param p observed p value
#' @param p_range Range of observed p-values where the easing takes place
#' @param from_prob Probability of publication (starting position)
#' @param to_prob Probability of publication (end position)
easeOutExpo <- function (p, p_range, from_prob, to_prob) {
	p_start <- p_range[1]
	p_range_length <- p_range[2] - p_range[1]
	(to_prob-from_prob) * (-2^(-10 * (p-p_range[1])/p_range_length) + 1) + from_prob;
}

easeInExpo <- function (p, p_range, from_prob, to_prob) {
	p_start <- p_range[1]
	p_range_length <- p_range[2] - p_range[1]
	(to_prob-from_prob) * 2^(10 * (((p-p_range[1])/p_range_length) - 1)) + from_prob;
}


#' @param pObs two-tailed p-value
#' @param posSign_NS_baseRate What's the probability that a p > .10 in the right direction enters the literature?
#' @param negSign_NS_baseRate What's the probability that a p > .01 in the wrong direction enters the literature? (Left anchor at p = .01)
#' @param counterSig_rate What's the probability that a p < .001 in the wrong direction enters the literature?
#' @param direction +1: Expected direction, -1: wrong direction

censor <- function(pObs, direction, posSign_NS_baseRate = 0.3, negSign_NS_baseRate = 0.05, counterSig_rate = 0.50){
	
	# ---------------------------------------------------------------------
	# Correct direction of effect
	
  if (direction > 0 & pObs < .05){       #right direction, sig
    pubProb = 1
  } else if(direction > 0 & pObs >= .05 & pObs < .1){ #right direction, trending
    pubProb = easeOutExpo(p=pObs, p_range=c(.05, .1), from_prob=1, to_prob=posSign_NS_baseRate)
  } else if (direction > 0 & pObs >= .1){	# right direction; non-significant (p > .1)
    pubProb =posSign_NS_baseRate
				
	# ---------------------------------------------------------------------
	# Wrong direction of effect	
  } else if(direction <= 0 & pObs < .001){	# wrong direction, highly sig.
    pubProb= counterSig_rate
  } else if(direction <= 0 & pObs >= .001 & pObs < .01){ # wrong direction, standard sig.
    pubProb= easeOutExpo(p=pObs, p_range=c(.001, .01), from_prob=counterSig_rate, to_prob=negSign_NS_baseRate)
  } else if(direction <= 0 & pObs >= .01){	# wrong direction, non-sig.
    pubProb=negSign_NS_baseRate
  }
  return(pubProb)
}


# in this (equivalent) variant of the function, you can provide a one-tailed p-value
# --> then it's not necessary to provide the direction of the effect
censor.1t.0 <- function(pObs, posSign_NS_baseRate = 0.3, negSign_NS_baseRate = 0.05, counterSig_rate = 0.50){
	
	# ---------------------------------------------------------------------
	# Correct direction of effect
	
  if (pObs < .05/2) {       #right direction, sig
    pubProb = 1
  } else if (pObs >= .05/2 & pObs < .1/2) { #right direction, trending
    pubProb = easeOutExpo(p=pObs, p_range=c(.05/2, .1/2), from_prob=1, to_prob=posSign_NS_baseRate)
  } else if (pObs >= .1/2 & pObs < .5) {	# right direction; non-significant (p > .1)
    pubProb = posSign_NS_baseRate
				
	# ---------------------------------------------------------------------
	# Wrong direction of effect	
	
	} else if (pObs >= .5 & pObs < 1-(.01/2)){	# wrong direction, non-sig. at 1%
	  pubProb = negSign_NS_baseRate
	} else if (pObs >= 1-(.01/2) & pObs < 1-(.001/2)){ # wrong direction, two-sided p between .01 and .001
    pubProb = easeInExpo(p=pObs, p_range=c(1-(.01/2), 1-(.001/2)), from_prob=negSign_NS_baseRate, to_prob=counterSig_rate)
  } else if (pObs >= 1-(.001/2)){	# wrong direction, highly sig.
    pubProb = counterSig_rate
  }
	
  return(pubProb)
}
censor.1t <- Vectorize(censor.1t.0)

# helper: convert 1-tailed p-value to 2-tailed
p.1.to.2 <- function(p.1tailed) {
	1-abs(0.5-p.1tailed)*2
}

# helper: get direction of a 1-tailed p-value
getDir <- function(p.1tailed) {
	ifelse(p.1tailed < .5, 1, -1)
}

# Sanity check: do both censor functions return the same value?
# curve(censor.1t(pObs=x, posSign_NS_baseRate = 0.20, negSign_NS_baseRate = 0.05, counterSig_rate = 0.50), from=0, to=1, n=10000)
#
# for (p.1t in seq(0, 1, length.out=1000)) {
#   points(x=p.1t, y=censor(pObs=p.1.to.2(p.1t), direction=getDir(p.1t)), col="red", pch=21, cex=.3)
# }


# some predefined settings: medium publication bias
censorMedium0 <- function(pObs, direction) {
	censor(pObs, direction, posSign_NS_baseRate = 0.20, negSign_NS_baseRate = 0.05, counterSig_rate = 0.50)
}
censorMedium <- Vectorize(censorMedium0)

# some predefined settings: strong publication bias
censorHigh0 <- function(pObs, direction) {
	censor(pObs, direction, posSign_NS_baseRate = 0.05, negSign_NS_baseRate = 0.00, counterSig_rate = 0.20)
}
censorHigh <- Vectorize(censorHigh0)