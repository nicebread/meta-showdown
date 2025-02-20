## ======================================================================
## This is the code for Figure 2 (Estimation Plot)
## ======================================================================


library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

load("dataFiles/summ.RData")

# ---------------------------------------------------------------------
# Plot settings

#YLIM <- c(-0.09, 1.2)
YLIM <- c(-0.52, 1.2)

theme_metashowdown <- theme(
  title = element_text(size=18),
  axis.title = element_text(size=18),
  axis.text = element_text(size=12),
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


#summ2 <- summ %>% filter(method %in% c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve", "puniform", "1PSM", "3PSM", "4PSM", "WAAP-WLS")) %>% 
#  mutate(method = factor(method, levels=c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve", "puniform", "1PSM", "3PSM", "4PSM", "WAAP-WLS"), labels=c("RE", "TF", "PET", "PEESE", "PET-PEESE", "p-curve", "p-uniform", "1PSM", "3PSM", "4PSM", "WAAP-WLS")))

# reduced set for revision
	summ2 <- summ %>% filter(method %in% c("reMA", "TF", "PETPEESE", "pcurve", "puniform", "3PSM", "WAAP-WLS")) %>% 
	  mutate(method = factor(method, levels=c("reMA", "TF", "WAAP-WLS", "pcurve", "puniform", "PETPEESE", "3PSM"), labels=c("RE", "TF", "WAAP-WLS", "p-curve", "p-uniform", "PET-PEESE", "3PSM")))



# prepare extra data.frame for the number of successful computation out of 1000 simulations
summ2$just <- ifelse(summ2$delta==0, 1.8, -0.8)
summ2$symbolCol <- ifelse(summ2$delta==0, "0", "1")
summ2$n.validEstimates.label <- as.character(summ2$n.validEstimates)
summ2$n.validEstimates.label[summ2$n.validEstimates.label=="1000"] <- ""

summ2$n.validEstimates.symbol <- cut(summ2$n.validEstimates, breaks=c(-1, 250, 500, 750, 1000), labels=c("! ", "#", "* ", ""))

#summ2$nPos <- summ2$perc2.5.pos
#summ2$nPos[summ2$delta > 0] <- summ2$perc97.5.pos[summ2$delta > 0]
summ2$nPos <- summ2$perc2.5
summ2$nPos[summ2$delta > 0] <- summ2$perc97.5[summ2$delta > 0]

#dat = summ2 %>% filter(censor==0, delta %in% DELTAS)

buildFacet <- function(dat, title) {
  PLOT <- dat %>%
   # ggplot(aes(x=factor(k), y=meanEst.pos, ymin=perc2.5.pos, ymax=perc97.5.pos, shape=qrp.label, color=factor(delta), fill=factor(delta))) + 
	 ggplot(aes(x=factor(k), y=meanEst, ymin=perc2.5, ymax=perc97.5, shape=qrp.label, color=factor(delta), fill=factor(delta))) + 
    geom_hline(yintercept=DELTAS[1], color="skyblue") + 
    geom_hline(yintercept=DELTAS[2], color="black") + 
    geom_pointrange(position=position_dodge(width=.7), size = 0.4) +	
    coord_flip(ylim=YLIM) +
		geom_text(aes(x=factor(k), y=nPos, label=n.validEstimates.symbol, hjust=just, group=qrp.label, color=factor(delta)), position=position_dodge(width=0.7), size=3, vjust=0.9) +
    
    #facet_grid(tau.label~method,labeller = label_bquote(cols = alpha ^ .(vs),rows =  tau = .(tau))) + 
    facet_grid(tau~method,labeller = label_bquote(rows = tau == .(tau))) + 
    
    theme_metashowdown +
    scale_y_continuous(breaks = c(-.5,.0,.5,1)) + 
    scale_shape_manual(values=c(21,22,24)) + 
    scale_color_manual(values=c("steelblue3", "black", "steelblue3", "black")) +
    scale_fill_manual(values=c("skyblue", "black")) +
    ylab("Estimated effect size") +
    xlab(expression(italic("k"))) +
    ggtitle(title)
  return(PLOT)
}



# ---------------------------------------------------------------------
# Build legend


buildLegend <- function(DELTAS) {
	values <- c("skyblue", "black")
	names(values) <- DELTAS

	#Extract Legend 
	g_legend<-function(a.gplot){ 
	  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
	  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
	  legend <- tmp$grobs[[leg]] 
	  return(legend)
	} 

	legOnlyPlot <- summ2 %>% filter(censor=="none", delta %in% DELTAS) %>%
	  ggplot(aes(x=factor(k), y=meanEst, shape=factor(qrpEnv),color=factor(delta),fill=factor(delta))) + 
	  geom_point() +
	  coord_flip(ylim=YLIM) +
	  facet_grid(tau.label~method) +
	  theme(
	    panel.background = element_rect(fill="white"),
	    legend.position = c("bottom"),
	    legend.key = element_rect(fill='white'),
			legend.title = element_text(size=14, face="bold"),
			legend.text = element_text(size=12)
	  ) + 
	  scale_shape_manual(values=c("none"=21,"med"=22,"high"=24),guide = guide_legend(title = "QRP Env.", override.aes = list(size=6))) +
	  scale_color_manual(values=values, guide = guide_legend(title = bquote(delta), override.aes = list(size=6))) +
	  scale_fill_manual(values=values, guide = guide_legend(title = bquote(delta)))


	return(g_legend(legOnlyPlot))
}



# ---------------------------------------------------------------------
# Save PDF for main text: delta = 0 vs. 0.5

DELTAS <- c(0, 0.5)
plotA <- buildFacet(dat = summ2 %>% filter(censor=="none", delta %in% DELTAS), bquote("(A) no publication bias"))
plotB <- buildFacet(dat = summ2 %>% filter(censor=="med", delta %in% DELTAS), bquote("(B) medium publication bias"))
plotC <- buildFacet(dat = summ2 %>% filter(censor=="high", delta %in% DELTAS), bquote("(C) strong publication bias"))
legend <- buildLegend(DELTAS)

pdf("Plots/Estimation_H1_05.pdf", width=15, height=22)
grid.arrange(plotA, plotB, plotC, legend, nrow=19, layout_matrix = cbind(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4)))
dev.off()


# ---------------------------------------------------------------------
# Save PDFs for supplement: delta = 0 vs. 0.2, and 0 vs. 0.8

DELTAS <- c(0, 0.2)
plotA.2 <- buildFacet(dat = summ2 %>% filter(censor=="none", delta %in% DELTAS), bquote("(A) no publication bias"))
plotB.2 <- buildFacet(dat = summ2 %>% filter(censor=="med", delta %in% DELTAS), bquote("(B) medium publication bias"))
plotC.2 <- buildFacet(dat = summ2 %>% filter(censor=="high", delta %in% DELTAS), bquote("(C) strong publication bias"))
legend <- buildLegend(DELTAS)

pdf("Plots/Estimation_H1_02.pdf", width=15, height=22)
grid.arrange(plotA.2, plotB.2, plotC.2, legend, nrow=19, layout_matrix = cbind(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4)))
dev.off()


DELTAS <- c(0, 0.8)
plotA.8 <- buildFacet(dat = summ2 %>% filter(censor=="none", delta %in% DELTAS), bquote("(A) no publication bias"))
plotB.8 <- buildFacet(dat = summ2 %>% filter(censor=="med", delta %in% DELTAS), bquote("(B) medium publication bias"))
plotC.8 <- buildFacet(dat = summ2 %>% filter(censor=="high", delta %in% DELTAS), bquote("(C) strong publication bias"))
legend <- buildLegend(DELTAS)

pdf("Plots/Estimation_H1_08.pdf", width=15, height=22)
grid.arrange(plotA.8, plotB.8, plotC.8, legend, nrow=19, layout_matrix = cbind(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4)))
dev.off()