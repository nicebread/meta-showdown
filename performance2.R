ME.benchmark <- 0.1

library(ggplot2)
dat <- summ %>% filter(method=="reMA") %>% arrange(k.label, delta.label, qrp.label, censor.label, tau.label)

dat$loop1 <- factor(paste0(dat$k.label, ", ", dat$delta.label))
dat$loop2 <- factor(paste0(dat$qrp.label, ", ", dat$censor.label, ", ", dat$tau.label))

dat$performance <- factor(abs(dat$ME) < ME.benchmark, labels=c("good", "poor"))

ggplot(dat, aes(x=loop1, y=loop2, fill=performance)) + geom_tile() + theme(axis.text.x = element_text(angle = 90)) + xlab("") + ylab("") + scale_fill_manual(name="Performance", values = c("lightgreen", "red3"))