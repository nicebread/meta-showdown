## ======================================================================
## This is the code for Figure X (Coverage Plot)
## ======================================================================

library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

load(file="summ.RData")

# ---------------------------------------------------------------------
# SETTINGS

# show coverage for delta==0 and delta==0.5
deltas <- c(0, 0.5)

# select only some methods for displaying
cov.sel <-  summ %>% 
  filter(method %in% c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "puniform", "3PSM")) %>% 
  mutate(method = factor(method, levels=c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "puniform", "3PSM"), labels=c("RE", "TF", "PET", "PEESE", "PET-PEESE", "p-uniform", "3PSM"))) %>% 
	ungroup() %>% 
	filter(delta %in% deltas)
	
	
# prepare extra data.frame for the number of successful computation out of 1000 simulations
cov.sel$n.p.values.symbol <- cut(cov.sel$n.p.values, breaks=c(-1, 250, 500, 750, 1000), labels=c("! ", "#", "* ", ""))	



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
    geom_hline(yintercept=c(0, .50, .95), linetype="dotted", color="skyblue") +
    geom_hline(yintercept=c(0, 1), linetype="solid", color="skyblue") +
    geom_point(aes(y=coverage.pos, color=factor(delta), fill=factor(delta)), position=position_dodge(width=.7), size = 3) +	
		geom_text(aes(x=factor(k), y=-0.05, label=n.p.values.symbol, group=qrp.label), position=position_dodge(width=0.7), size=2.5, hjust=1, vjust=0.6, color="steelblue3") +
    coord_flip() +
    facet_grid(tau~method,labeller = label_bquote(rows = tau == .(tau))) + 
    scale_y_continuous(labels=scales::percent, limits=c(-0.07, 1.07), breaks = c(.50, .95)) + 
    scale_shape_manual(values=c(21, 22, 24)) + 
		scale_color_manual(breaks=c("0", "0.5"), values=c("steelblue3", "black")) +
		scale_fill_manual(breaks=c("0", "0.5"), values=c("skyblue", "black")) +
    ylab("Coverage") +
    xlab(expression(italic(k))) + 
    theme_metashowdown +
    ggtitle(title)
  return(PLOT)	
}

plotA <- buildFacet(dat = cov.sel %>% filter(selProp==0), bquote("(A) Coverage at 0% publication bias"))
plotB <- buildFacet(dat = cov.sel %>% filter(selProp==0.6), bquote("(B) Coverage at 60% publication bias"))
plotC <- buildFacet(dat = cov.sel %>% filter(selProp==0.9), bquote("(C) Coverage at 90% publication bias"))


# ---------------------------------------------------------------------
# Build the legend

legOnlyPlot <- expand.grid(qrp.label=factor(c("none", "med", "high"), ordered=TRUE), deltas=deltas) %>% 
  ggplot(aes(x=1, y=1, shape=qrp.label, color=factor(deltas), fill=factor(deltas))) + 
  geom_point()+
  theme(
    panel.background = element_rect(fill="white"),
    legend.position = c("bottom"),
    legend.key = element_rect(fill = 'white'),
		legend.title = element_text(size=14, face="bold"),
		legend.text = element_text(size=12)
  ) + 
  scale_color_manual(values=c("steelblue3", "black"), guide = guide_legend(title = bquote(delta), override.aes = list(size=6))) +
  scale_fill_manual(values=c("skyblue", "black"), guide = guide_legend(title = bquote(delta))) +
  scale_shape_manual(values=c("none"=21,"med"=22,"high"=24),guide = guide_legend(title = "QRP Env.", override.aes = list(size=6)))
  
	 
#Extract Legend 
g_legend<-function(a.gplot) { 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
} 

legend <- g_legend(legOnlyPlot) 

# ---------------------------------------------------------------------
# Save PDF of plot

pdf("Plots/coverage.pdf", width=15, height=22)
grid.arrange(plotA, plotB, plotC, legend, nrow=19, layout_matrix = cbind(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4)))
dev.off()
