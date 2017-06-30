clamp <- function(x) {if (x > 1) x=1; if (x < 0) x = 0; return(x)}

censor = function(dObs,pObs,f){
  if(dObs > 0 & pObs < .05){       #right direction, sig
    pubProb = 1
	#}else if(dObs > 0 & pObs < .05){ #right direction, standard sig
    #pubProb = .98
  }else if(dObs > 0 & pObs >= .05 & pObs <= .1){ #right direction, trending
    pubProb = 1-((1-0.3*f)/.05)*(pObs-.05)
  }else if (dObs > 0 & pObs > .1){	# right direction; non-significant (p > .1)
    pubProb =.3*f
  }else if(dObs <= 0 & pObs < .01){	# wrong direction, highly sig.
    pubProb= 1
  }else if(dObs <= 0 & pObs >= .01 & pObs < .05){ # wrong direction, standard sig.
    pubProb= 1-((1-0.1*f)/.04)*(pObs-.01)
  }else if(dObs <= 0 & pObs >= .05){	# wrong direction, non-sig.
    pubProb=.1*f
  }
  return(pubProb)
}

pubProb <- data.frame()
for (p in seq(0, 1, by=0.001)) {
	pubProb <- rbind(pubProb, data.frame(p=p, f=1, d=-1, pubProb=censor(-1, p, 1)))
	pubProb <- rbind(pubProb, data.frame(p=p, f=.5, d=-1, pubProb=censor(-1, p, .5)))
	pubProb <- rbind(pubProb, data.frame(p=p, f=1, d=+1, pubProb=censor(+1, p, 1)))
	pubProb <- rbind(pubProb, data.frame(p=p, f=.5, d=+1, pubProb=censor(+1, p, .5)))
}

library(ggplot2)
ggplot(pubProb, aes(x=p, y=pubProb, color=factor(f), group=factor(f))) + 
geom_vline(xintercept=c(.01, .05, .1), linetype="dotted", color="grey20")+ geom_line() + facet_wrap(~d)
