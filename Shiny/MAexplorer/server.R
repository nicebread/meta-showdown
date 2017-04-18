library(shiny)
library(shinyBS) # Additional Bootstrap Controls
library(ggplot2)
library(dplyr)
library(ggvis)
library(reshape2)


load("summ.RData")
#load("res.hyp.RData")
load("RR.RData")

H0.stroke <- "black"
H0.fill <- "grey40"
H1.stroke <- "steelblue3"
H1.fill <- "skyblue"


RR$TypeI.excess <- cut(RR$TypeI, breaks=c(0, .05, .10, 1), labels=c("skyblue", "orange", "red"))
RR$shape <- as.character(factor(RR$qrp.label, labels=c("circle", "square", "triangle-up")))
RR.H1 <- RR %>% select(k, delta, qrp.label, selProp, selProp.label, tau.label, method, TypeI, TypeI.excess, Power, shape)
RR.H0 <- RR.H1 %>% filter(delta == 0)

summ$stroke <- ifelse(summ$delta == 0, H0.stroke, H1.stroke)
summ$fill <- ifelse(summ$delta == 0, H0.fill, H1.fill)

# store estimation quantiles in long format
summLong <- summ %>% 
		filter(method %in% c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve", "puniform", "3PSM")) %>% 		
		mutate(method = factor(method, levels=c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve", "puniform", "3PSM"), labels=c("RE", "TF", "PET", "PEESE", "PET-PEESE", "p-curve", "p-uniform", "3PSM"))) %>% 
		ungroup() %>% 
		select(k, delta, qrp.label, selProp, selProp.label, tau.label, qrpEnv, method, stroke, fill, meanEst.pos, perc2.5.pos, perc97.5.pos) %>% 
		melt(id.vars=c("k", "delta", "qrp.label", "selProp", "selProp.label", "tau.label", "qrpEnv", "method", "stroke", "fill"), na.rm=FALSE)



	# input <- list(tau.label="tau = 0.2", k=10, delta=0.5, selProp=0.6, qrpEnv = "none")
	# ggDat.H0 <- RR.H0 %>% filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp)
shinyServer(function(input, output, session) {

	ggDat.H0 <- reactive({
		gg0 <- RR.H0 %>% 
			filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp) %>% 
			select(qrp.label, method, TypeI, Power, shape)

			return(gg0)			
	})
	
	
	ggDat.H1 <- reactive({
		gg1 <- RR.H1 %>% 
			filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, delta == input$delta) %>% 
			select(qrp.label, method, TypeI, Power, shape)
			
			return(gg1)			
	})
	

	
	# renderer must be "canvas", otherwise NAs are displayed
	
	# ---------------------------------------------------------------------
	# Type I error plot
	ggvis(ggDat.H0, y=~method, x=~TypeI, shape := ~shape, stroke:=H0.stroke, fill := H0.fill) %>% 
		layer_points()  %>% 		
		add_axis("x", properties=axis_props(
				title = list(fontSize = 16, text="False positive rate"),
				labels = list(fontSize = 13)
			), title_offset=40) %>% 
		scale_numeric("x", domain = c(0, 1), nice = FALSE) %>%	
		add_axis("y", properties=axis_props(
				title = list(fontSize = 16, text="Method"),
				labels = list(fontSize = 13)
			), title_offset=100) %>% 	
		set_options(duration = 1000, renderer="canvas") %>%
		#add_legend(scales = "stroke", properties = legend_props(title = list(fontSize = 0), labels = list(fontSize = 0), symbols = list(size = 0))) %>% 
	  bind_shiny("ggvis_TypeI", "ggvis_ui_TypeI")
		
	# ---------------------------------------------------------------------
	# Power plot
	ggvis(ggDat.H1, y=~method, x=~Power, shape := ~shape, stroke:=H1.stroke, fill := H1.fill) %>% 
		layer_points()  %>% 
		add_axis("x", properties=axis_props(
				title = list(fontSize = 16, text="Statistical Power"),
				labels = list(fontSize = 13)
			), title_offset=40) %>% 
		scale_numeric("x", domain = c(0, 1), nice = FALSE) %>%	
		add_axis("y", properties=axis_props(
				title = list(fontSize = 16, text="Method"),
				labels = list(fontSize = 13)
			), title_offset=100) %>% 	
		set_options(duration = 1000, renderer="canvas") %>%
	  bind_shiny("ggvis_Power", "ggvis_ui_Power")
		
		


	## ======================================================================
	## Estimation plot
	## ======================================================================
	
	
	YLIM <- c(-0.1, 1.2)
	
	ggSumm0 <- reactive({
		summLong %>% 
		filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta == 0, variable == "meanEst.pos") %>% 
		arrange(method, stroke)
	})
	
	ggQ0 <- reactive({
		summLong %>% 
		filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta == 0, variable != "meanEst.pos") %>% 
		arrange(method, stroke)
	})
	
	ggSumm1 <- reactive({
		summLong %>% filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta == input$delta, variable == "meanEst.pos") %>% 
		arrange(method, stroke)
	})
	
	ggQ1 <- reactive({
		summLong %>% 
		filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta == input$delta, variable != "meanEst.pos") %>% 
		arrange(method, stroke)
	})
	
	ggH1Label <- reactive({return(paste0("Bias-corrected estimate for delta = ", input$delta))})
	
	ggvis(ggSumm0, y=~method) %>% 
		layer_points(x=~value, y= ~factor(method), stroke := ~stroke, fill := ~fill) %>% 
		layer_paths(x= ~value, stroke := ~stroke, fill := ~fill, data=ggQ0 %>% group_by(method)) %>%
		add_axis("x", properties=axis_props(
				title = list(fontSize = 16, text="Bias-corrected estimate for delta = 0"),
				labels = list(fontSize = 13)
			), title_offset=40) %>%
		scale_numeric("x", domain = YLIM, nice = FALSE) %>%
		add_axis("y", properties=axis_props(
				title = list(fontSize = 16, text="Method"),
				labels = list(fontSize = 13)
			), title_offset=100) %>%
		set_options(duration = 1000, renderer="canvas") %>%
	  bind_shiny("ggvis_Estimation_H0", "ggvis_ui_Estimation_H0")
		
		
	observe({
		ggvis(ggSumm1, y=~method) %>% 
		layer_points(x=~value, y= ~factor(method), stroke := ~stroke, fill := ~fill) %>% 
		layer_paths(x= ~value, stroke := ~stroke, fill := ~fill, data=ggQ1 %>% group_by(method)) %>%
		add_axis("x", properties=axis_props(
				title = list(fontSize = 16, text=paste0("Bias-corrected estimate for delta = ", input$delta)),
				labels = list(fontSize = 13)
			), title_offset=40) %>%
		scale_numeric("x", domain = YLIM, nice = FALSE) %>%
		add_axis("y", properties=axis_props(
				title = list(fontSize = 16, text="Method"),
				labels = list(fontSize = 13)
			), title_offset=100) %>%
		set_options(duration = 1000, renderer="canvas") %>%
	  bind_shiny("ggvis_Estimation_H1", "ggvis_ui_Estimation_H1")
	})

})
