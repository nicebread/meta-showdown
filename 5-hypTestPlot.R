## ======================================================================
## This is the code for Figure 3 (Hypothesis Test Plot)
## ======================================================================

library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

#setwd("C:/Users/evan.c.carter/Documents/Meta-analysis showdown")
load(file="dataFiles/summ.RData")

# ---------------------------------------------------------------------
# SETTINGS

prepareDataSet <- function(H1) {
	# select only some methods for displaying
	hyp.sel <- summ %>% 
		filter(method %in% c("reMA", "TF", "PETPEESE", "pcurve.evidence", "puniform", "3PSM", "WAAP-WLS")) %>% 
	  mutate(method = factor(method, levels=c("reMA", "TF", "WAAP-WLS", "pcurve.evidence", "puniform", "PETPEESE", "3PSM"), labels=c("RE", "TF", "WAAP-WLS", "p-curve", "p-uniform", "PET-PEESE", "3PSM"))) %>% ungroup()

	
	# prepare extra data.frame for the number of successful computation out of 1000 simulations
	hyp.sel$n.p.values.symbol <- cut(hyp.sel$n.p.values, breaks=c(-1, 250, 500, 750, 1000), labels=c("! ", "#", "* ", ""))	

	# join H1 and H0 data to wide format data frame
	hyp.H0 <- hyp.sel %>% filter(delta == 0) %>% select(k, qrp.label, censor, tau.label, method, tau, n.p.values.symbol.H0 = n.p.values.symbol, TypeI = H0.reject.pos.rate)
	hyp.H1 <- hyp.sel %>% filter(delta == H1) %>% select(k, qrp.label, censor, tau.label, method, tau, n.p.values.symbol.H1 = n.p.values.symbol, Power = H0.reject.pos.rate)

	hyp.wide <- inner_join(hyp.H0, hyp.H1)

	hyp.wide$rejectionRatio <- hyp.wide$Power/hyp.wide$TypeI
	hyp.wide$errorSum <- (1-hyp.wide$Power) + hyp.wide$TypeI
	
	return(hyp.wide)
}


theme_metashowdown <- theme(
  title = element_text(size=18),
  axis.title = element_text(size=18),
  axis.text.y = element_text(size=18),
  axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1,size=15),
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


buildFacet <- function(dat, title) {
  PLOT <- dat %>%
    ggplot(aes(x=factor(k), shape=qrp.label)) + 
    geom_hline(yintercept=c(.05, .50, .80), linetype="dotted", color="skyblue") +
    geom_hline(yintercept=c(0, 1), linetype="solid", color="skyblue") +
    geom_point(aes(y=TypeI), position=position_dodge(width=.7), size = 3, color="steelblue3", fill="skyblue") +	
    geom_point(aes(y=Power), position=position_dodge(width=.7), size = 1.5, color="black", fill="black") +	
		geom_text(aes(x=factor(k), y=-0.05, label=n.p.values.symbol.H0, group=qrp.label), position=position_dodge(width=0.7), size=2.5, hjust=1, vjust=0.6, color="steelblue3") +
		geom_text(aes(x=factor(k), y=1.05, label=n.p.values.symbol.H1, group=qrp.label), position=position_dodge(width=0.7), size=2.5, hjust=0, vjust=0.6, color="black") +
    coord_flip() +
    facet_grid(tau~method,labeller = label_bquote(rows = tau == .(tau))) + 
    scale_y_continuous(labels=c("5%", "50%", "80%", "100%"), limits=c(-0.07, 1.07), breaks = c(.05, .5, .8, 1)) + 
    scale_shape_manual(values=c(21, 22, 24)) + 
    ylab("False positives / Statistical Power") +
    xlab(expression(italic(k))) + 
    theme_metashowdown +
    ggtitle(title)
  return(PLOT)	
}


# ---------------------------------------------------------------------
# Build the legend

legOnlyPlot <- data.frame(qrp.label=factor(c("none", "med", "high"), ordered=TRUE), rate=c("False positive rate", "Power", "Power")) %>% 
  ggplot(aes(x=1, y=1, shape=qrp.label, color=rate, fill=rate)) + 
  geom_point()+
  theme(
    panel.background = element_rect(fill="white"),
    legend.position = c("bottom"),
    legend.key = element_rect(fill = 'white'),
		legend.title = element_text(size=14, face="bold"),
		legend.text = element_text(size=12)
  ) + 
  scale_shape_manual(values=c("none"=21,"med"=22,"high"=24), guide = guide_legend(title = "QRP Env.", override.aes = list(size=6))) +
  scale_color_manual(values=c("False positive rate"="skyblue","Power"="black"), guide = guide_legend(title = "Rates", override.aes = list(size=6))) +
  scale_fill_manual(values=c("False positive rate"="skyblue","Power"="black"), guide = guide_legend(title = "Rates")) 
	 
#Extract Legend 
g_legend<-function(a.gplot) { 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
} 

legend <- g_legend(legOnlyPlot) 



# ---------------------------------------------------------------------
# Save PDF for main text: delta = 0 vs. 0.5

# compare delta==0 (for false positive rate) against delta==0.5 (for power)
hyp.wide.05 <- prepareDataSet(H1=0.5)
save(hyp.wide.05, file="dataFiles/hyp.wide.RData")
save(hyp.wide.05, file="Shiny/metaExplorer/hyp.wide.RData")

plotA.5 <- buildFacet(dat = hyp.wide.05 %>% filter(censor=="none", method != "4PSM"), bquote("(A) no publication bias"))
plotB.5 <- buildFacet(dat = hyp.wide.05 %>% filter(censor=="med", method != "4PSM"), bquote("(B) medium publication bias"))
plotC.5 <- buildFacet(dat = hyp.wide.05 %>% filter(censor=="high", method != "4PSM"), bquote("(C) strong publication bias"))


pdf("Plots/HypothesisTest_H1_05.pdf", width=15, height=22)
grid.arrange(plotA.5, plotB.5, plotC.5, legend, nrow=19, layout_matrix = cbind(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4)))
dev.off()


# ---------------------------------------------------------------------
# Save PDFs for supplement: delta = 0 vs. 0.2, and 0 vs. 0.8

hyp.wide.2 <- prepareDataSet(H1=0.2)

plotA.2 <- buildFacet(dat = hyp.wide.2 %>% filter(censor=="none", method != "4PSM"), bquote("(A) no publication bias"))
plotB.2 <- buildFacet(dat = hyp.wide.2 %>% filter(censor=="med", method != "4PSM"), bquote("(B) medium publication bias"))
plotC.2 <- buildFacet(dat = hyp.wide.2 %>% filter(censor=="high", method != "4PSM"), bquote("(C) strong publication bias"))


pdf("Plots/HypothesisTest_H1_02.pdf", width=15, height=22)
grid.arrange(plotA.2, plotB.2, plotC.2, legend, nrow=19, layout_matrix = cbind(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4)))
dev.off()


hyp.wide.8 <- prepareDataSet(H1=0.8)

plotA.8 <- buildFacet(dat = hyp.wide.8 %>% filter(censor=="none", method != "4PSM"), bquote("(A) no publication bias"))
plotB.8 <- buildFacet(dat = hyp.wide.8 %>% filter(censor=="med", method != "4PSM"), bquote("(B) medium publication bias"))
plotC.8 <- buildFacet(dat = hyp.wide.8 %>% filter(censor=="high", method != "4PSM"), bquote("(C) strong publication bias"))


pdf("Plots/HypothesisTest_H1_08.pdf", width=15, height=22)
grid.arrange(plotA.8, plotB.8, plotC.8, legend, nrow=19, layout_matrix = cbind(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4)))
dev.off()