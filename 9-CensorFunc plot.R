theme_metashowdown <- theme(
  title = element_text(size=14),
  axis.title = element_text(size=12),
  axis.text = element_text(size=9),
  strip.text = element_text(size=16),
  panel.spacing =unit(.5, "lines"),
  panel.background = element_rect(fill="white"),
  panel.border = element_rect(color="grey70",fill=NA, size = 1), #element_blank(),
  panel.grid.minor= element_blank(),
  panel.grid.major= element_blank(),#element_line(color="grey90"),
  strip.background = element_rect(colour="white", fill="white"), #"grey93"
  axis.ticks = element_line(color="lightgrey"),
  legend.position = "none"#c("bottom"),
)


plotCensorFunction <- function(posSign_NS_baseRate = 0.3, negSign_NS_baseRate = 0.10, counterSig_rate = 0.50, log.x = TRUE) {
	pubProb <- data.frame()
	for (p in seq(0.001, 1, by=0.001)) {
		pubProb <- rbind(pubProb, data.frame(p=p, d=-1, facet="Wrong sign", pubProb=censor(p, -1, posSign_NS_baseRate, negSign_NS_baseRate, counterSig_rate)))
		pubProb <- rbind(pubProb, data.frame(p=p, d=+1, facet="Expected sign", pubProb=censor(p, +1, posSign_NS_baseRate, negSign_NS_baseRate, counterSig_rate)))
	}

	ann <- data.frame(
		facet = c("Wrong sign", "Expected sign"),
		label = c(negSign_NS_baseRate, posSign_NS_baseRate)
	)

	library(ggplot2)
	p1 <- ggplot(pubProb, aes(x=p, y=pubProb)) + 
		geom_vline(xintercept=c(.01, .05, .1), linetype="dotted", color="grey20") + geom_line() + 
		facet_wrap(~facet) + coord_cartesian(ylim=c(0, 1)) +
		geom_text(data=ann, aes(label=label, y=label), x=0.5, vjust=-0.3) + ylab("Pr(publication)") + xlab("p-value") + 
		theme_metashowdown
		
		if (log.x == TRUE) {
			 p1 <- p1 + scale_x_log10(breaks=c(.01, .05, .1, .5, 1)) + xlab("p-value (logarithmic scale)")
		}
		
		p1
}

#plotCensorFunction(0.30, 0.10, 0.5)

c1 <- plotCensorFunction(1, 1, 1) + ggtitle("(A) No publication bias") + xlab("") + ylab("")
c2 <- plotCensorFunction(0.20, 0.05, 0.50) + ggtitle("(B) Medium publication bias") + xlab("")
c3 <- plotCensorFunction(0.05, 0, 0.20) + ggtitle("(C) Strong publication bias") + ylab("")

# ---------------------------------------------------------------------
# Save PDF

library(gridExtra)

pdf("Plots/CensorFunction.pdf", width=7, height=8)
grid.arrange(c1, c2, c3, ncol=1)
dev.off()



## ======================================================================
## Alternative version: Use one-tailed p-value
## ======================================================================


plotCensorFunction.1t <- function(posSign_NS_baseRate = 0.3, negSign_NS_baseRate = 0.10, counterSig_rate = 0.50, log.x = TRUE) {
	
	# helper function: convert 1-tailed p-value into 2-tailed
	p.1.to.2 <- function(p.1tailed) {1-abs(0.5-p.1tailed)*2}
	getDir <- function(p.1tailed) {ifelse(p.1tailed < .5, 1, -1)}
	p.1.to2log <- function(p.1tailed) {log(p.1.to.2(p.1tailed))*getDir(p.1tailed)}
	
	pubProb <- data.frame()
	for (p.1t in seq(0.0001, 1-0.0001, by=0.0001)) {
		pubProb <- rbind(pubProb, data_frame(
			p.1t=p.1t,
			p.2t.log = p.1.to2log(p.1t),
			pubProb=censor.1t(p.1t, posSign_NS_baseRate, negSign_NS_baseRate, counterSig_rate)
		))
	}

	ann <- data.frame(
		facet = c("Wrong sign", "Expected sign"),
		label = c(negSign_NS_baseRate, posSign_NS_baseRate)
	)

	library(ggplot2)
	library(grid)
	
	xbreaks <- p.1.to2log(c(0.0001, .025, .05, .5, .995, .9995, 1-0.0001))
	
	p1 <-
	 ggplot(pubProb, aes(x=p.2t.log, y=pubProb)) + 
		annotate(geom="segment", x=xbreaks, xend=xbreaks, y=0, yend=1, linetype="dotted", color="grey20") + 
		annotate(geom="segment", x=p.1.to2log(0.5), xend=p.1.to2log(0.5), y=0, yend=1.1, linetype="dotted", color="grey20") + 
		geom_line() + 
		coord_cartesian(ylim=c(0, 1.1)) +
		scale_x_continuous(breaks=xbreaks, labels=c("0", ".025", ".05", ".5", ".995", ".9995", "1")) +
		scale_y_continuous(breaks=c(0, .25, .5, .75, 1)) + 
		ylab("Pr(publication)") + xlab("One-tailed p-value (log scale on both sides of .5)") + 
		annotate(geom="segment", x=p.1.to2log(0.48), y=1.1, xend=p.1.to2log(0.05), yend=1.1, arrow=arrow(length = unit(0.05, "inches"))) +
		annotate(geom="segment", x=p.1.to2log(0.52), y=1.1, xend=p.1.to2log(0.95), yend=1.1, arrow=arrow(length = unit(0.05, "inches"))) +
		annotate(geom="text", label = "Expected sign", x=p.1.to2log(0.05), y=1.1, hjust = 1.1, size=4) +
		annotate(geom="text", label = "Wrong sign", x=p.1.to2log(0.95), y=1.1, hjust = -.1, size=4) +
		theme_metashowdown

		p1
}

#plotCensorFunction(0.30, 0.10, 0.5)

c1.1t <- plotCensorFunction.1t(1, 1, 1) + ggtitle("(A) No publication bias") + xlab("") + ylab("")
c2.1t <- plotCensorFunction.1t(0.20, 0.05, 0.50) + ggtitle("(B) Medium publication bias") + xlab("")
c3.1t <- plotCensorFunction.1t(0.05, 0, 0.20) + ggtitle("(C) Strong publication bias") + ylab("")

# ---------------------------------------------------------------------
# Save PDF

library(gridExtra)

pdf("Plots/CensorFunction1t.pdf", width=7, height=8)
grid.arrange(c1.1t, c2.1t, c3.1t, ncol=1)
dev.off()