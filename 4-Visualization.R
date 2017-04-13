library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

#setwd("C:/Users/evan.c.carter/Documents/Meta-analysis showdown")
load("summ.RData")

# ---------------------------------------------------------------------
# Plot settings

theme_metashowdown <- theme(
    panel.spacing =unit(.5, "lines"),
    panel.background = element_rect(fill="white"),
    panel.border = element_rect(color="grey70",fill=NA, size = 1), #element_blank(),
    panel.grid.minor= element_blank(),
    panel.grid.major= element_blank(),#element_line(color="grey90"),
    strip.background = element_rect(colour="white", fill="white"), #"grey93"
    axis.ticks = element_line(color="lightgrey"),
    legend.position = "none"#c("bottom")
)


summ2 <- summ %>% filter(
  #!method %in% c("PET.lm", "PEESE.lm", "pcurve.evidence", "pcurve.lack")
  !method %in% c("pcurve.evidence", "pcurve.lack", "pcurve.hack", "topN.fixed", "PET.rma", "PEESE.rma", "PETPEESE.rma")
)


#change the text that appears in facet columns
lev2 = levels(summ2$method)
lev2[3] = "PET"
lev2[4] = "PEESE"
lev2[7] = "PET-PEESE"
levels(summ2$method) <- lev2

YLIM <- c(-0.1, 1.2)


buildFacet <- function(dat, title) {
	
		PLOT <- dat %>%
	  ggplot(aes(x=factor(k), y=meanEst.pos, ymin=perc2.5.pos, ymax=perc97.5.pos, shape=qrp.label, color=factor(delta), fill=factor(delta))) + 
	  geom_hline(yintercept=0, color="skyblue") + 
		geom_hline(yintercept=0.5, color="black") + 
		geom_pointrange(position=position_dodge(width=.7),size = 0.4) +	
	  coord_flip(ylim=YLIM) +
	  facet_grid(tau.label~method) + 
		theme_metashowdown +
	  scale_y_continuous(breaks = c(-.5,.0,.5,1)) + 
	  scale_shape_manual(values=c(21,22,24)) + 
	  scale_color_manual(values=c("0"="steelblue3","0.5"="black")) +
	  scale_fill_manual(values=c("0"="skyblue","0.5"="black")) +
	  labs(shape="k", colour="Delta") +
	  ylab("Estimated effect size") +
	  xlab("Meta-analytic sample size (k)") +
	  ggtitle(title)
		
		return(PLOT)
}


plotA <- buildFacet(summ2 %>% filter(selProp==0, delta==0 | delta==.5), "(A) Estimate and 95% bootstrap percentiles, 0% publication bias")
plotB <- buildFacet(summ2 %>% filter(selProp==0.6, delta==0 | delta==.5), "(B) Estimate and 95% bootstrap percentiles, 60% publication bias")
plotC <- buildFacet(summ2 %>% filter(selProp==0.9, delta==0 | delta==.5), "(C) Estimate and 95% bootstrap percentiles, 90% publication bias")


# ---------------------------------------------------------------------
# Build legend

#Extract Legend 
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legOnlyPlot = summ2 %>% filter(selProp==0.9,delta==0 | delta==.5) %>%
  ggplot(aes(x=factor(k), y=meanEst, ymin=perc2.5,
             ymax=perc97.5, shape=factor(qrpEnv),color=factor(delta),fill=factor(delta))) + 
  geom_pointrange(position=position_dodge(width=.7),size = 0.4) +
  coord_flip(ylim=YLIM) +
  facet_grid(tau.label~method) +
  theme(
    panel.background = element_rect(fill="white"),
    legend.position = c("bottom")
  ) + 
  scale_shape_manual(values=c(21,22,24)) +
  scale_shape_manual(values=c("none"=21,"med"=22,"high"=24),guide = guide_legend(title = "QRP Env.")) +
  scale_color_manual(values=c("0"="skyblue","0.5"="black"),guide = guide_legend(title = "Delta")) +
  scale_fill_manual(values=c("0"="skyblue","0.5"="black"),guide = guide_legend(title = "Delta"))


legend <- g_legend(legOnlyPlot) 


# ---------------------------------------------------------------------
# Save PDF

pdf("Plots/estimation.pdf", width=15, height=22)
grid.arrange(plotA, plotB, plotC, legend, nrow=19, layout_matrix = cbind(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4)))
dev.off()

