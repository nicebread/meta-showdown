

pH1 <- 1				# rate of true hypotheses
alpha <- .05		# alpha
Pow <- .35			# average power
selProp <- .05	# probability that a nonsig. result enters the literature

percSig <- (Pow*pH1) + (alpha*(1-pH1))
publishedStudies <- percSig + (1-percSig)*selProp
sigRatio <- percSig / publishedStudies


# solve for selProp:
selProp <- (((pH1-1)*percSig - pH1 + 1) * alpha - pH1*Pow*percSig + pH1*Pow) / ((pH1-1)*percSig*alpha + (1 - pH1*Pow)*percSig)
selProp


pH1 <- 0.3				# rate of true hypotheses
alpha <- .05		# alpha
Pow <- .5			# average power
percSig <- .90

selProp <- (((pH1-1)*percSig - pH1 + 1) * alpha - pH1*Pow*percSig + pH1*Pow) / ((pH1-1)*percSig*alpha + (1 - pH1*Pow)*percSig)
selProp


# ---------------------------------------------------------------------
#

X1 <- simMA(500, 0.2, .1, "med", censorFunc="0", verbose=TRUE)
X2 <- simMA(500, 0.2, .1, "med", censorFunc="A", verbose=TRUE)
X3 <- simMA(500, 0.2, .1, "med", censorFunc="B", verbose=TRUE)

mean(X1$d)
mean(X2$d)
mean(X3$d)

prop.table(table(X1$p < .05))
prop.table(table(X2$p < .05))
prop.table(table(X3$p < .05))
