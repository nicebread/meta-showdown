# helper functions for censor function

# clamp x to values between 0 and 1
clamp <- function(x) {if (x > 1) x=1; if (x < 0) x = 0; return(x)}


#' @param p observed p value
#' @param p_range Range of observed p-values where the easing takes place
#' @param from_prob Probability of publication
#' @param p observed p value
easeOutExpo <- function (p, p_range, from_prob, to_prob) {
	p_start <- p_range[1]
	p_range_length <- diff(range(p_range))
		(to_prob-from_prob) * (-2^(-10 * (p-p_range[1])/p_range_length) + 1) + from_prob;
}

#ps <- seq(.05, .1, by=.001)
#plot(ps, easeOutExpo(p = ps, p_range=range(ps), from_prob = .7, to_prob = .3))


#' @param posSign_NS_baseRate What's the probability that a p > .10 in the right direction enters the literature?
#' @param negSign_NS_baseRate1 What's the probability that a p > .01 in the wrong direction enters the literature? (Left anchor at p = .01)
#' @param negSign_NS_baseRate2 What's the probability that a p > .01 in the wrong direction enters the literature? (Right anchor at p = 1)
#' @param direction +1: Expected direction, -1: wrong direction

censor <- function(pObs, direction, posSign_NS_baseRate = 0.3, negSign_NS_baseRate = 0.05, counterSig_rate = 0.50){
	
	# ---------------------------------------------------------------------
	# Correct direction fo effect
	
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
