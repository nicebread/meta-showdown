library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

#setwd("C:/Users/evan.c.carter/Documents/Meta-analysis showdown")
load("summ.RData")

# ---------------------------------------------------------------------
# Show estimate

summ2 <- summ %>% filter(
  #!method %in% c("PET.lm", "PEESE.lm", "pcurve.evidence", "pcurve.lack")
  !method %in% c("pcurve.evidence", "pcurve.lack", "pcurve.hack", "topN.fixed")
)


#change the text that appears in facet columns
lev2 = levels(summ2$method)
lev2[3] = "PET"
lev2[4] = "PEESE"
lev2[7] = "PET-PEESE"
levels(summ2$method) <- lev2

#Extract Legend 
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 




plotA = summ2 %>% filter(selProp==0,delta==0 | delta==.5) %>%
  ggplot(aes(x=factor(k), y=meanEst, ymin=perc2.5,
             ymax=perc97.5,shape=qrp.label,color=factor(delta),fill=factor(delta))) + 
  geom_hline(aes(yintercept=delta)) + 
  #geom_hline(aes(yintercept=-.5)) + 
  geom_pointrange(position=position_dodge(width=.7),size = 0.4) +
  coord_flip(ylim=c(-0.6, 1.1)) +
  facet_grid(tau.label~method) + 
  #theme_bw() + 
  theme(
    panel.spacing =unit(.05, "lines"),
    panel.background = element_rect(fill="white"),
    panel.border = element_rect(color="grey90",fill=NA, size = 1), #element_blank(),
    panel.grid.minor= element_blank(),
    panel.grid.major= element_blank(),#element_line(color="grey90"),
    strip.background = element_rect(colour="white", fill="white"), #"grey93"
    axis.ticks = element_line(color="lightgrey"),
    legend.position = "none"#c("bottom")
  ) + 
  scale_y_continuous(limits=c(-1.3,1.3),breaks = c(-.5,.0,.5,1)) + 
  scale_shape_manual(values=c(21,22,24)) + 
  scale_color_manual(values=c("0"="grey60","0.5"="black")) +
  scale_fill_manual(values=c("0"="grey60","0.5"="black")) +
  labs(shape="k", colour="Delta") +
  ylab("Estimated effect size") +
  xlab("Meta-analytic sample size (k)") +
  ggtitle("(A) Estimate and 95% bootstrap percentiles, 0% publication bias")


plotB = summ2 %>% filter(selProp==0.6,delta==0 | delta==.5) %>%
  ggplot(aes(x=factor(k), y=meanEst, ymin=perc2.5,
             ymax=perc97.5,shape=qrp.label,color=factor(delta),fill=factor(delta))) + 
  geom_hline(aes(yintercept=delta)) + 
  #geom_hline(aes(yintercept=-.5)) + 
  geom_pointrange(position=position_dodge(width=.7),size = 0.4) +
  coord_flip(ylim=c(-0.6, 1.1)) +
  facet_grid(tau.label~method) + 
  #theme_bw() + 
  theme(
    panel.spacing =unit(.05, "lines"),
    panel.background = element_rect(fill="white"),
    panel.border = element_rect(color="grey90",fill=NA, size = 1), #element_blank(),
    panel.grid.minor= element_blank(),
    panel.grid.major= element_blank(),#element_line(color="grey90"),
    strip.background = element_rect(colour="white", fill="white"), #"grey93"
    axis.ticks = element_line(color="lightgrey"),
    legend.position = "none"#c("bottom")
  ) + 
  scale_y_continuous(limits=c(-1.3,1.3),breaks = c(-.5,.0,.5,1)) + 
  scale_shape_manual(values=c(21,22,24)) + 
  scale_color_manual(values=c("0"="grey60","0.5"="black")) +
  scale_fill_manual(values=c("0"="grey60","0.5"="black")) +
  labs(shape="k", colour="Delta") +
  ylab("Estimated effect size") +
  xlab("Meta-analytic sample size (k)") +
  ggtitle("(B) Estimate and 95% bootstrap percentiles, 60% publication bias")

plotC = summ2 %>% filter(selProp==0.9,delta==0 | delta==.5) %>%
  ggplot(aes(x=factor(k), y=meanEst, ymin=perc2.5,
             ymax=perc97.5,shape=qrp.label,color=factor(delta),fill=factor(delta))) + 
  geom_hline(aes(yintercept=delta)) + 
  #geom_hline(aes(yintercept=-.5)) + 
  geom_pointrange(position=position_dodge(width=.7),size = 0.4) +
  coord_flip(ylim=c(-0.6, 1.1)) +
  facet_grid(tau.label~method) + 
  #theme_bw() + 
  theme(
    panel.spacing =unit(.05, "lines"),
    panel.background = element_rect(fill="white"),
    panel.border = element_rect(color="grey90",fill=NA, size = 1), #element_blank(),
    panel.grid.minor= element_blank(),
    panel.grid.major= element_blank(),#element_line(color="grey90"),
    strip.background = element_rect(colour="white", fill="white"), #"grey93"
    axis.ticks = element_line(color="lightgrey"),
    legend.position = "none"#c("bottom")
  ) + 
  scale_y_continuous(limits=c(-1.3,1.3),breaks = c(-.5,.0,.5,1)) + 
  scale_shape_manual(values=c(21,22,24)) + 
  scale_color_manual(values=c("0"="grey60","0.5"="black")) +
  scale_fill_manual(values=c("0"="grey60","0.5"="black")) +
  labs(shape="k", colour="Delta") +
  ylab("Estimated effect size") +
  xlab("Meta-analytic sample size (k)") +
  ggtitle("(C) Estimate and 95% bootstrap percentiles, 90% publication bias")


legOnlyPlot = summ2 %>% filter(selProp==0.9,delta==0 | delta==.5) %>%
  ggplot(aes(x=factor(k), y=meanEst, ymin=perc2.5,
             ymax=perc97.5,shape=factor(qrpEnv),color=factor(delta),fill=factor(delta))) + 
  geom_pointrange(position=position_dodge(width=.7),size = 0.4) +
  coord_flip(ylim=c(-0.6, 1.1)) +
  facet_grid(tau.label~method) +
  theme(
    panel.background = element_rect(fill="white"),
    legend.position = c("bottom")
  )+ 
  scale_shape_manual(values=c(21,22,24)) +
  scale_shape_manual(values=c("none"=21,"med"=22,"high"=24),guide = guide_legend(title = "QRP Env.")) +
  scale_color_manual(values=c("0"="grey60","0.5"="black"),guide = guide_legend(title = "Delta")) +
  scale_fill_manual(values=c("0"="grey60","0.5"="black"),guide = guide_legend(title = "Delta"))


legend <- g_legend(legOnlyPlot) 


pdf("estimation.pdf",width=15,height=22)

grid.arrange(plotA,plotB,plotC,legend,nrow=19,layout_matrix = cbind(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4)))

dev.off()

