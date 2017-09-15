clamp <- function(x) {if (x > 1) x=1; if (x < 0) x = 0; return(x)}

#' @param posSign_NS_baseRate What's the probability that a p > .10 in the right direction enters the literature?
#' @param negSign_NS_baseRate1 What's the probability that a p > .01 in the wrong direction enters the literature? (Left anchor at p = .01)
#' @param negSign_NS_baseRate2 What's the probability that a p > .01 in the wrong direction enters the literature? (Right anchor at p = 1)
#' @param direction +1: Expected direction, -1: wrong direction

censor <- function(pObs, direction, posSign_NS_baseRate = 0.3, negSign_NS_baseRate = 0.05){
	
	# ---------------------------------------------------------------------
	# Correct direction fo effect
	
  if (direction > 0 & pObs < .05){       #right direction, sig
    pubProb = 1
  } else if(direction > 0 & pObs >= .05 & pObs < .1){ #right direction, trending
    pubProb = 1-((1-posSign_NS_baseRate)/.05)*(pObs-.05)
  } else if (direction > 0 & pObs >= .1){	# right direction; non-significant (p > .1)
    pubProb =posSign_NS_baseRate
				
	# ---------------------------------------------------------------------
	# Wrong direction of effect	
  } else if(direction <= 0 & pObs < .01){	# wrong direction, highly sig.
    pubProb= 1
  } else if(direction <= 0 & pObs >= .01 & pObs < .05){ # wrong direction, standard sig.
    pubProb= 1-((1-negSign_NS_baseRate)/.04)*(pObs-.01)
  } else if(direction <= 0 & pObs >= .05){	# wrong direction, non-sig.
    pubProb=negSign_NS_baseRate
  }
  return(pubProb)
}
