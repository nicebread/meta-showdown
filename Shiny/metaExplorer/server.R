# for testing: some input values:
# input <- list(tau=0.2, k=10, delta=0.5, deltaFull=0.5, censor="med", qrpEnv = "none", dropNegatives=TRUE, PETPEESEmodel = "lm")

shinyServer(function(input, output, session) {


	## ======================================================================
	## Method Performance Check Plots
	## ======================================================================
	
	output$perfPlot <- renderUI({

		# abort rendering if one of the conditions has no values at all
		if (is.null(input$k_perf) | is.null(input$tau_perf) | is.null(input$censor_perf) | is.null(input$delta_H1_perf) | is.null(input$qrpEnv_perf) | is.na(input$ME_upper) | is.na(input$ME_lower)) {
		  validate(FALSE) # abort rendering right away
		}
		
		selectedMethod <- input$evaluatedMethod
		# if (input$PETPEESEmodel == "lm" & input$evaluatedMethod == "PETPEESE") selectedMethod <- "PETPEESE.lm"
		# if (input$PETPEESEmodel == "rma" & input$evaluatedMethod == "PETPEESE") selectedMethod <- "PETPEESE.rma"
		
		perf.dat <- summ2 %>% filter(
			method == selectedMethod,
			k %in% input$k_perf,
			tau %in% input$tau_perf,
			censor %in% input$censor_perf,
			qrpEnv %in% input$qrpEnv_perf,
			delta %in% c(0, input$delta_H1_perf)  # always select delta==0
		)
				
		perf.dat$loop1 <- factor(paste0(perf.dat$tau.label, ", ", perf.dat$k.label))
		perf.dat$loop2 <- factor(paste0(perf.dat$delta.label, ", ", perf.dat$censor.label, ", ", perf.dat$qrp.label))

		perfMeasureString <- ""
		
		if (is.na(as.numeric(input$ME_upper))) {
			perf.dat$ME_upper_check <- TRUE
		} else {
			perf.dat$ME_upper_check <- perf.dat$ME <= abs(as.numeric(input$ME_upper))
			perfMeasureString <- paste0(perfMeasureString, "positive bias <= ", as.numeric(input$ME_upper), "; ")
		} 
		
		if (is.na(as.numeric(input$ME_lower))) {
			perf.dat$ME_lower_check <- TRUE
		} else {
			perf.dat$ME_lower_check <- perf.dat$ME >= -abs(as.numeric(input$ME_lower))
			perfMeasureString <- paste0(perfMeasureString, "negative bias >= ", -abs(as.numeric(input$ME_lower)), "; ")
		} 
		
		
		if (is.na(as.numeric(input$RMSE_upperbound))) {
			perf.dat$RMSE_check <- TRUE
		} else {
			perf.dat$RMSE_check <- perf.dat$RMSE <= as.numeric(input$RMSE_upperbound)
			perfMeasureString <- paste0(perfMeasureString, "RMSE <= ", as.numeric(input$RMSE_upperbound), "; ")
		}
		
		if (is.na(as.numeric(input$MAD_upperbound))) {
			perf.dat$MAD_check <- TRUE
		} else {
			perf.dat$MAD_check <- perf.dat$MAD <= as.numeric(input$MAD_upperbound)
			perfMeasureString <- paste0(perfMeasureString, "MAD <= ", as.numeric(input$MAD_upperbound), "; ")
		}
		
		if (is.na(as.numeric(input$coverage_lowerbound))) {
			perf.dat$coverage_check <- TRUE
		} else {
			perf.dat$coverage_check <- perf.dat$coverage >= as.numeric(input$coverage_lowerbound)/100
			perfMeasureString <- paste0(perfMeasureString, "coverage >= ", as.numeric(input$coverage_lowerbound), "%", "; ")
		}
		
		if (is.na(as.numeric(input$FPR_upperbound))) {
			perf.dat$FPR_check <- TRUE
		} else {
			perf.dat$FPR_check <- perf.dat$TypeI <= as.numeric(input$FPR_upperbound)/100
			perfMeasureString <- paste0(perfMeasureString, "false positive rate <= ", as.numeric(input$FPR_upperbound), "%", "; ")
		}
		
		if (is.na(as.numeric(input$FNR_upperbound))) {
			perf.dat$FNR_check <- TRUE
		} else {
			perf.dat$FNR_check <- perf.dat$H0.reject.rate	>= 1-(as.numeric(input$FNR_upperbound)/100)
			perfMeasureString <- paste0(perfMeasureString, "false negative rate <= ", as.numeric(input$FNR_upperbound), "%", "; ")
		}
		
		perf.dat <- perf.dat %>% mutate(
			performance.H0 = factor(ME_upper_check & ME_lower_check & RMSE_check & MAD_check & coverage_check & FPR_check, levels=c(TRUE, FALSE), labels=c("good", "poor")),
			performance.H1 = factor(ME_upper_check & ME_lower_check & RMSE_check & MAD_check & coverage_check & FNR_check, levels=c(TRUE, FALSE), labels=c("good", "poor"))
		)
		
		title <- paste0("Method: ", input$evaluatedMethod, "\nCriterion for good performance: ", perfMeasureString)
		
		
		p.H1 <- ggplot(perf.dat %>% filter(delta > 0), aes(x=loop1, y=loop2, fill=performance.H1)) + geom_tile() + theme(axis.text.x = element_text(angle = 90)) + xlab("") + ylab("") + scale_fill_manual(name="Performance", values = c("good" = "lightblue", "poor"= "red3")) + ggtitle(title)
		
		p.H0 <- ggplot(perf.dat %>% filter(delta == 0), aes(x=loop1, y=loop2, fill=performance.H0)) + geom_tile() + theme(axis.text.x = element_text(angle = 90)) + xlab("") + ylab("") + scale_fill_manual(name="Performance", values = c("good" = "lightblue", "poor"= "red3")) + ggtitle(title)

		returnList <- list(
			h3("Under H0:"),
			renderPlot(p.H0, height = 600, units="px"),
			h3("Under H1s:"),
			renderPlot(p.H1, height = 600, units="px")
		)
		
		if (input$show_performance_table == TRUE) {
			perfTable.H0 <- perf.dat %>% filter(delta == 0) %>% select(-meanEst, -k.label, -delta.label, -qrp.label,	-censor.label, -tau.label, -stroke,	-fill,	-loop1,	-loop2, -H0.reject.pos.rate, -H0.reject.wrongSign.rate, -n.p.values, -n.validEstimates, -coverage.pos, -n.ci, -consisZero.rate, -FNR_check, -performance.H1, -contains(".pos"))
			perfTable.H1 <- perf.dat %>% filter(delta > 0) %>% select(-meanEst, -k.label, -delta.label, -qrp.label,	-censor.label, -tau.label, -stroke,	-fill,	-loop1,	-loop2, -H0.reject.pos.rate, -H0.reject.wrongSign.rate, -n.p.values, -n.validEstimates, -coverage.pos, -n.ci, -consisZero.rate, -FPR_check, -performance.H0, -contains(".pos"))
			
			perfTable.H0[, 8:15] <- round(perfTable.H0[, 8:15], 3)
			perfTable.H1[, 8:15] <- round(perfTable.H1[, 8:15], 3)
			returnList <- c(returnList, list(
				h3("Under H0 (table output):"),
				HTML(getTable(perfTable.H0)),
				h3("Under H1 (table output):"),
				HTML(getTable(perfTable.H1))
			))
		}
		
		return(returnList)
	})
		


	## ======================================================================
	## Error rate plots
	## ======================================================================

	ggDat.H0 <- reactive({
		RR.H0 %>% 
			filter(k == input$k, tau == input$tau, censor == input$censor, qrpEnv == input$qrpEnv) %>% 
			select(qrp.label, method, TypeI) %>% 
			selectModels()
	})
	
	
	ggDat.H1 <- reactive({
		RR.H1 %>% 
			filter(k == input$k, tau == input$tau, censor == input$censor, qrpEnv == input$qrpEnv, delta==input$delta) %>% 
			select(qrp.label, method, Power) %>% selectModels()
	})
	
	
	# renderer must be "canvas", otherwise NAs are displayed
	
	# ---------------------------------------------------------------------
	# Type I error plot
	
	verticalLineDataTypeI <- data.frame(value=c(.05, .05), method=rep(c("RE", "4PSM"), 1))	
	
	ggvis(ggDat.H0, y=~method, x=~TypeI, stroke:=H0.stroke, fill := H0.fill) %>% 
		layer_points()  %>% 		
		layer_paths(x= ~value, y= ~method, stroke := H0.fill, fill := H0.fill, data=verticalLineDataTypeI %>% group_by(factor(value))) %>%
		add_axis("x", properties=axis_props(
				title = list(fontSize = 16, text="False positive rate"),
				labels = list(fontSize = 13)
			), title_offset=40) %>% 
		scale_numeric("x", domain = c(0, 1), nice = FALSE) %>%	
		add_axis("y", properties=axis_props(
				title = list(fontSize = 16, text="Method"),
				labels = list(fontSize = 13)
			), title_offset=100) %>% 	
		scale_ordinal('y', domain=methodOrder) %>%	
		set_options(width=600, height=260, duration = 1000, renderer="canvas") %>%
		#add_legend(scales = "stroke", properties = legend_props(title = list(fontSize = 0), labels = list(fontSize = 0), symbols = list(size = 0))) %>% 
	  bind_shiny("ggvis_TypeI", "ggvis_ui_TypeI")
		
	# ---------------------------------------------------------------------
	# Power plot
	
	verticalLineDataPow <- data.frame(value=c(.50, .50, .8, .8, 1, 1), method=rep(c("RE", "4PSM"), 3))	
	
	ggvis(ggDat.H1, y=~method, x=~Power, stroke:=H1.stroke, fill := H1.fill) %>% 
		layer_points()  %>% 
		layer_paths(x= ~value, y= ~method, stroke := H1.fill, fill := H1.fill, data=verticalLineDataPow %>% group_by(factor(value))) %>%
		add_axis("x", properties=axis_props(
				title = list(fontSize = 16, text="Statistical Power"),
				labels = list(fontSize = 13)
			), title_offset=40) %>% 
		scale_numeric("x", domain = c(0, 1), nice = FALSE) %>%	
		add_axis("y", properties=axis_props(
				title = list(fontSize = 16, text="Method"),
				labels = list(fontSize = 13)
			), title_offset=100) %>% 	
		scale_ordinal('y', domain=methodOrder) %>% 			
		set_options(width=600, height=260, duration = 1000, renderer="canvas") %>%
	  bind_shiny("ggvis_Power", "ggvis_ui_Power")
		
		


	## ======================================================================
	## Estimation plot
	## ======================================================================
	
	
	YLIM <- reactive({
		if (input$dropNegatives == TRUE) {
			return(c(-0.1, 1.6))
		} else {
			return(c(-1.6, 1.6))
		}
	})
	
	ggSumm0 <- reactive({
		ggSumm0.res <- summLong %>% 
			filter(k == input$k, tau == input$tau, censor == input$censor, qrpEnv == input$qrpEnv, delta == 0) %>% 
			filter(variable == paste0("meanEst", ifelse(input$dropNegatives == TRUE, ".pos", ""))) %>% 
			selectModels()
		
		# cap values larger than YLIM[2] - otherwise the plot is screwed. A warning is displayed	
		ggSumm0.res$value[ggSumm0.res$value > isolate(YLIM()[2])] <- YLIM()[2]
		return(ggSumm0.res)
	})
	
	ggQ0 <- reactive({
		ggQ0.res <- summLong %>% 
			filter(k == input$k, tau == input$tau, censor == input$censor, qrpEnv == input$qrpEnv, delta == 0) %>% 
			filter(variable %in% c(paste0("perc2.5", ifelse(input$dropNegatives == TRUE, ".pos", "")), paste0("perc97.5", ifelse(input$dropNegatives == TRUE, ".pos", "")))) %>% 
			selectModels()
			
		# cap values larger than YLIM[2] - otherwise the plot is screwed. A warning is displayed	
		ggQ0.res$value[ggQ0.res$value > isolate(YLIM()[2])] <- YLIM()[2]
		return(ggQ0.res)
	})
	
	ggSumm1 <- reactive({
		ggSumm1.res <- summLong %>% 
			filter(k == input$k, tau == input$tau, censor == input$censor, qrpEnv == input$qrpEnv, delta == input$delta) %>% 
			filter(variable == paste0("meanEst", ifelse(input$dropNegatives == TRUE, ".pos", ""))) %>% 
			selectModels()
			
			# cap values larger than YLIM[2] - otherwise the plot is screwed. A warning is displayed	
			ggSumm1.res$value[ggSumm1.res$value > isolate(YLIM()[2])] <- YLIM()[2]
			return(ggSumm1.res)
	})
	
	ggQ1 <- reactive({
		ggQ1.res <- summLong %>% 
			filter(k == input$k, tau == input$tau, censor == input$censor, qrpEnv == input$qrpEnv, delta == input$delta)  %>% 
			filter(variable %in% c(paste0("perc2.5", ifelse(input$dropNegatives == TRUE, ".pos", "")), paste0("perc97.5", ifelse(input$dropNegatives == TRUE, ".pos", "")))) %>% 
			selectModels()
			
		# cap values larger than YLIM[2] - otherwise the plot is screwed. A warning is displayed	
		ggQ1.res$value[ggQ1.res$value > isolate(YLIM()[2])] <- YLIM()[2]
		return(ggQ1.res)
	})
	
	
	output$cap_alert <- renderUI({
		
		if (any(ggQ0()$value >= YLIM()[2]) | any(ggQ1()$value >= YLIM()[2])) {		
			return(list(
				alert.create(paste0("Note: Quantile bars have been capped at ", YLIM()[2]), style="warning")
				))
		}
	})
	
	
	# ggH1 <- reactive({
	# 	ggH1Label <- paste0("Bias-corrected estimate for delta = ", input$delta)
	#
	# 	summLongH1 <- summLong %>% filter(k == input$k, tau == input$tau, censor == input$censor, qrpEnv == input$qrpEnv, delta == input$delta, variable == paste0("meanEst", ifelse(input$dropNegatives == TRUE, ".pos", "")))
	#
	# 	Q1 <- summLong %>%
	# 	filter(k == input$k, tau == input$tau, censor == input$censor, qrpEnv == input$qrpEnv, delta == input$delta, variable %in% c(paste0("perc2.5", ifelse(input$dropNegatives == TRUE, ".pos", "")), paste0("perc97.5", ifelse(input$dropNegatives == TRUE, ".pos", ""))))
	#
	# 	verticalLineDataH1 <- data.frame(value=c(input$delta, input$delta), method=c("RE", "3PSM"))
	#
	# 	return(list(ggH1Label=ggH1Label, summLongH1=summLongH1, Q1=Q1, verticalLineDataH1=verticalLineDataH1))
	# })
	
	
	
	
	verticalLineDataH0 <- data.frame(value=c(0, 0), method=c("RE", "4PSM"))
	ggvis(ggSumm0, y=~method) %>% 
		layer_points(x=~value, y= ~factor(method), stroke := ~stroke, fill := ~fill) %>% 
		layer_paths(x= ~value, stroke := ~stroke, fill := ~fill, data=ggQ0 %>% group_by(method)) %>%
		layer_paths(x= ~value, y= ~method, stroke := H0.stroke, fill := H0.stroke, data=verticalLineDataH0) %>%
		add_axis("x", properties=axis_props(
				title = list(fontSize = 16, text="Bias-corrected estimate for delta = 0"),
				labels = list(fontSize = 13)
			), title_offset=40) %>%
		scale_numeric("x", domain = YLIM, nice = FALSE) %>%
		add_axis("y", properties=axis_props(
				title = list(fontSize = 16, text="Method"),
				labels = list(fontSize = 13)
			), title_offset=100) %>%
		scale_ordinal('y', domain=methodOrder) %>% 	
		set_options(width=600, height=260, duration = 1000, renderer="canvas") %>%
	  bind_shiny("ggvis_Estimation_H0", "ggvis_ui_Estimation_H0")
		
		
	# observe({
#
# 		# this pulls in reactive data, but destroys the transitions ...
# 		dat <- ggH1()
#
# 		ggvis(data=dat$summLongH1, y=~method) %>%
# 		layer_points(x=~value, y= ~factor(method), stroke := ~stroke, fill := ~fill) %>%
# 		layer_paths(x= ~value, stroke := ~stroke, fill := ~fill, data=dat$Q1 %>% group_by(method)) %>%
# 		layer_paths(x= ~value, y= ~method, stroke := H1.stroke, fill := H1.fill, data=dat$verticalLineDataH1) %>%
# 		add_axis("x", properties=axis_props(
# 				title = list(fontSize = 16, text=dat$ggH1Label),
# 				labels = list(fontSize = 13)
# 			), title_offset=40) %>%
# 		scale_numeric("x", domain = YLIM, nice = FALSE) %>%
# 		add_axis("y", properties=axis_props(
# 				title = list(fontSize = 16, text="Method"),
# 				labels = list(fontSize = 13)
# 			), title_offset=100) %>%
# 		scale_ordinal('y', domain=c("RE", "TF", "PET", "PEESE", "PET-PEESE", "p-curve", "p-uniform", "3PSM")) %>%
# 		set_options(width=600, height=260, duration = 1000, renderer="canvas") %>%
# 	  bind_shiny("ggvis_Estimation_H1", "ggvis_ui_Estimation_H1")
# 	})
	
		verticalLineDataH1 <- data.frame(value=c(.2, .2, .5, .5, .8, .8), method=rep(c("RE", "4PSM"), 3))
		observe({
			ggvis(data=ggSumm1, y=~method) %>% 
			layer_points(x=~value, y= ~factor(method), stroke := ~stroke, fill := ~fill) %>% 
			layer_paths(x= ~value, stroke := ~stroke, fill := ~fill, data=ggQ1 %>% group_by(method)) %>%
			layer_paths(x= ~value, y= ~method, stroke := H1.fill, fill := H1.fill, data=verticalLineDataH1 %>% group_by(factor(value))) %>%
			layer_paths(x= ~value, y= ~method, stroke := H0.fill, fill := H0.fill, data=verticalLineDataH0) %>%		
			add_axis("x", properties=axis_props(
					title = list(fontSize = 16, text=paste0("Bias-corrected estimate for delta = ", input$delta)),
					labels = list(fontSize = 13)
				), title_offset=40) %>%
			scale_numeric("x", domain = YLIM, nice = FALSE) %>%
			add_axis("y", properties=axis_props(
					title = list(fontSize = 16, text="Method"),
					labels = list(fontSize = 13)
				), title_offset=100) %>%
			scale_ordinal('y', domain=methodOrder) %>% 		
			set_options(width=600, height=260, duration = 1000, renderer="canvas") %>%
		  bind_shiny("ggvis_Estimation_H1", "ggvis_ui_Estimation_H1")
		})
	
	
## ======================================================================
## TABLES
## ======================================================================

hypTab <- reactive({
	RR.H1.specific <- RR.H1 %>% 
		filter(delta == input$delta)  %>% 
		filter(k == input$k, tau == input$tau, censor == input$censor, qrpEnv == input$qrpEnv) %>% 
		select(-delta, -censor) %>% 
		selectModels()

	RR.H0.specific <- RR.H0 %>% 
		filter(k == input$k, tau == input$tau, censor == input$censor, qrpEnv == input$qrpEnv) %>% 
		select(-delta, -censor) %>% 
		selectModels()
				
	RR.wide <- inner_join(RR.H0.specific, RR.H1.specific, by = c("k", "qrp.label", "qrpEnv", "censor.label", "tau", "method")) %>% 
		select(method, TypeI, Power)
	
	RR.wide[, 2:ncol(RR.wide)] <- round(RR.wide[, 2:ncol(RR.wide)], 3)
	
	RR.wide$rejectionRatio <- round(RR.wide$Power/RR.wide$TypeI, 1)
	RR.wide$rejectionRatio[is.infinite(RR.wide$rejectionRatio)] <- NA
	return(RR.wide)
})

output$hypTable <- renderUI({
	return(list(HTML(getTable(hypTab()))))
})



estTab <- reactive({
	summ0 <- summ2 %>% 	
		filter(k == input$k, tau == input$tau, censor == input$censor, qrpEnv == input$qrpEnv, delta %in% c(0, input$delta)) %>% 
		select(-k, -qrp.label, -censor.label, -tau.label, -consisZero.rate, -consisZero.rate.pos, -n.ci, -H0.reject.rate,  -H0.reject.pos.rate, -H0.reject.wrongSign.rate, -n.p.values) %>% 
		select(-censor, -qrpEnv, -stroke, -fill, -condition, -k.label, -delta.label, -tau, -MAD) %>% 
		selectModels()
	
	if (input$dropNegatives == TRUE) {
		summ0 <- summ0 %>% select(-meanEst, -perc2.5, -perc97.5, -ME, -RMSE, -coverage)
	} else {
		summ0 <- summ0 %>% select(-meanEst.pos, -perc2.5.pos, -perc97.5.pos, -ME.pos, -RMSE.pos, -coverage.pos)
	}
	
	summ0[, 3:ncol(summ0)] <- round(summ0[, 3:ncol(summ0)], 2)

	return(summ0)
})

output$estTable <- renderUI({
	return(list(
		h3("Under H0:"),
		HTML(getTable(estTab() %>% filter(delta==0) %>% select(-delta))),
		h3("Under H1:"),
		HTML(getTable(estTab() %>% filter(delta>0) %>% select(-delta)))
	))
})


## ======================================================================
## Funnel plots
## ======================================================================

# find the condition # that matches the selected tasu, censor, etc.
# load the raw data set with that condition
simDat <- reactive({

	cond <- which(conditions$k == input$k & conditions$tau == input$tau & conditions$censor == input$censor & conditions$qrpEnv == input$qrpEnv & conditions$delta == input$deltaFull)
	
	filename <- paste0("demoDat/simData_condition_", cond, ".RData")
	#filename <- paste0("demoDat/simData_condition_4.RData")
	load(filename)	
	
	return(sim %>% filter(unique==input$demoDatID))
})


output$funnelplot <- renderPlot({
	sim0 <- simDat()
	sim0$se <- sqrt(sim0$v)
	
	meta1 <- metagen(sim0$d, sim0$se)
	#meta::funnel(meta1, ref=0, comb.random=TRUE, contour.levels=.95, col.contour=c("skyblue"), xlab="Effect size", cex=.7, pch=19, xlim=c(-1.3, 1.7), col="grey30")
	
	plot(NA, xlim=c(-1.3, 1.7), ylim=c(max(sim0$se), 0), xlab="Effect size", ylab="Standard Error")
	
	# the contour triangle
	polygon(x=c(-1.96*max(sim0$se), 0, 1.96*max(sim0$se), -1.96*max(sim0$se)), y=c(max(sim0$se), 0, max(sim0$se), max(sim0$se)), col="skyblue", border=NA)
	
	# the RE triangle
	REest <- meta1$TE.random
	polygon(x=c(-1.96*max(sim0$se)+REest, REest, 1.96*max(sim0$se)+REest, -1.96*max(sim0$se)+REest), y=c(max(sim0$se), 0, max(sim0$se), max(sim0$se)), col=NA, border="grey20", lty="dotted")
	
	lines(x=c(REest, REest), y=c(max(sim0$se), 0), col="grey20", lty="dotted")
	
	# the study points
	points(sim0$d, sim0$se, cex=1.1, pch=21, col="black", bg="white")

	# compute PET-PEESE
	PET <- lm(d~se, weights=1/sim0$v, sim0)
	PEESE <- lm(d~v, weights=1/sim0$v, sim0)
	
  PET.est <- coef(PET)[1]
  PET.slope <- coef(PET)[2]
  PEESE.est <- coef(PEESE)[1]

	u <- par("usr")	# get range of plot coordinates

	# plot blue dot at RE-MA
	points(REest, u[3], cex=1.5, col="blue3", pch=20)
	
	# plot red dot at true ES
	points(input$deltaFull, u[3], cex=1.5, col="red3", pch=20)

	# plot PET-line
	range <- seq(0, u[3], length.out=100)

	if (input$show_PET == TRUE) {
		# predict values from model
		PET.p <- PET.est + PET.slope*range
		lines(PET.p, range, col="blue3", lwd=2)

		segments(coef(PET)[1], 0, coef(PET)[1], u[3], col="blue3", lty="dotted", lwd=2)
		points(coef(PET)[1], u[3], cex=1.5, col="blue3", pch=20)
	}

	# plot PEESE-line
	if (input$show_PEESE == TRUE) {
		PEESE.p <- PEESE.est + coef(PEESE)[2]*range^2
		lines(PEESE.p, range, col="blue3", lwd=2)

		segments(coef(PEESE)[1], 0, coef(PEESE)[1], u[3], col="blue3", lty="dotted", lwd=2)
		points(coef(PEESE)[1], u[3], cex=1.5, col="blue3", pch=20)
	}
})

output$funnelplotAnnotation <- htmlOutput({
	return(list(
		h2("TEST")
	))
})

})