library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

#setwd("C:/Users/evan.c.carter/Documents/Meta-analysis showdown")
load("summ.RData")

# ---------------------------------------------------------------------
# Plot settings

YLIM <- c(-0.09, 1.2)
DELTAS <- c(0, 0.5)

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


summ2 <- summ %>% filter(method %in% c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve", "puniform", "3PSM")) %>% 
  mutate(method = factor(method, levels=c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve", "puniform", "3PSM"), labels=c("RE", "TF", "PET", "PEESE", "PET-PEESE", "p-curve", "p-uniform", "3PSM")))


# prepare extra data.frame for the number of successful computation out of 1000 simulations
summ2$nPos <- ifelse(summ2$delta==0, -0.05, 1.3)
summ2$just <- ifelse(summ2$delta==0, 1.8, -0.4)
summ2$n.validEstimates.label <- as.character(summ2$n.validEstimates)
summ2$n.validEstimates.label[summ2$n.validEstimates.label=="1000"] <- ""

summ2$n.validEstimates.symbol <- cut(summ2$n.validEstimates, breaks=c(-1, 250, 500, 750, 1000), labels=c("! ", "#", "* ", ""))

summ2$nPos2 <- summ2$perc2.5.pos
summ2$nPos2[summ2$delta > 0] <- summ2$perc97.5.pos[summ2$delta > 0]

#dat = summ2 %>% filter(selProp==0, delta %in% DELTAS)

buildFacet <- function(dat, title) {
  PLOT <- dat %>%
    ggplot(aes(x=factor(k), y=meanEst.pos, ymin=perc2.5.pos, ymax=perc97.5.pos, shape=qrp.label, color=factor(delta), fill=factor(delta))) + 
    geom_hline(yintercept=DELTAS[1], color="skyblue") + 
    geom_hline(yintercept=DELTAS[2], color="black") + 
    geom_pointrange(position=position_dodge(width=.7), size = 0.4) +	
    coord_flip(ylim=YLIM) +
		#geom_text(aes(x=factor(k), y=nPos, label=n.validEstimates.label, hjust=just, group=qrp.label), position=position_dodge(width=0.7), size=3, vjust=0.5) +
		geom_text(aes(x=factor(k), y=nPos2, label=n.validEstimates.symbol, hjust=just, group=qrp.label), position=position_dodge(width=0.7), size=3, vjust=0.9, color="steelblue3") +
    
    #facet_grid(tau.label~method,labeller = label_bquote(cols = alpha ^ .(vs),rows =  tau = .(tau))) + 
    facet_grid(tau~method,labeller = label_bquote(rows = tau == .(tau))) + 
    
    theme_metashowdown +
    scale_y_continuous(breaks = c(-.5,.0,.5,1)) + 
    scale_shape_manual(values=c(21,22,24)) + 
    scale_color_manual(values=c("steelblue3", "black")) +
    scale_fill_manual(values=c("skyblue", "black")) +
    ylab("Estimated effect size") +
    xlab(expression(italic("k"))) +
    ggtitle(title)
  return(PLOT)
}

#UPDATED TITLES
plotA <- buildFacet(dat = summ2 %>% filter(selProp==0, delta %in% DELTAS), bquote("(A) 0% publication bias"))
plotB <- buildFacet(dat = summ2 %>% filter(selProp==0.6, delta %in% DELTAS), bquote("(B) 60% publication bias"))
plotC <- buildFacet(dat = summ2 %>% filter(selProp==0.9, delta %in% DELTAS), bquote("(C) 90% publication bias"))

#OLDER VERSION
#plotA <- buildFacet(summ2 %>% filter(selProp==0, delta %in% DELTAS), bquote("(A) Estimate and 95% percentiles at"~delta~" = "~.(DELTAS[2])~", 0% publication bias"))
#plotB <- buildFacet(summ2 %>% filter(selProp==0.6, delta %in% DELTAS), bquote("(B) Estimate and 95% percentiles at"~delta~" = "~.(DELTAS[2])~", 60% publication bias"))
#plotC <- buildFacet(summ2 %>% filter(selProp==0.9, delta %in% DELTAS), bquote("(C)Estimate and 95% percentiles at"~delta~" = "~.(DELTAS[2])~", 90% publication bias"))


# ---------------------------------------------------------------------
# Build legend

values <- c("skyblue", "black")
names(values) <- DELTAS

#Extract Legend 
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legOnlyPlot = summ2 %>% filter(selProp==0.9, delta %in% DELTAS) %>%
  ggplot(aes(x=factor(k), y=meanEst.pos, ymin=perc2.5.pos,
             ymax=perc97.5.pos, shape=factor(qrpEnv),color=factor(delta),fill=factor(delta))) + 
  geom_pointrange(position=position_dodge(width=.7),size = 0.4) +
  coord_flip(ylim=YLIM) +
  facet_grid(tau.label~method) +
  theme(
    panel.background = element_rect(fill="white"),
    legend.position = c("bottom"),
    legend.key = element_rect(fil='white')
  ) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_shape_manual(values=c("none"=21,"med"=22,"high"=24),guide = guide_legend(title = "QRP Env.")) +
  scale_color_manual(values=values, guide = guide_legend(title = bquote(delta))) +
  scale_fill_manual(values=values, guide = guide_legend(title = bquote(delta)))


legend <- g_legend(legOnlyPlot) 


# ---------------------------------------------------------------------
# Save PDF

pdf("Plots/estimation.pdf", width=15, height=22)
grid.arrange(plotA, plotB, plotC, legend, nrow=19, layout_matrix = cbind(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4)))
dev.off()
