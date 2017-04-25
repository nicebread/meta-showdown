library(shiny)
#library(shinyBS) # Additional Bootstrap Controls
#library(ggplot2)
library(dplyr)
library(ggvis)
library(reshape2)
library(htmltools)


getTable <- function(df, cbGetClass = NULL) {
	df <- as.data.frame(df)
  thead <- paste0('<th>', htmlEscape(names(df)), '</th>', collapse='')
  
  tbody <- rep("", nrow(df))
  
  for(row in 1:nrow(df)) {
    format <- '<td>%s</td>'

    tbody[row] <- paste0(sapply(df[row,], function(x){ sprintf('<td>%s</td>', htmlEscape(as.character(x))) }), collapse='');
    
    cls <- NULL
    if( is.function(cbGetClass) ) {
      cls <- cbGetClass(df[row,])
    } 
    
    if(is.character(cls)) {
      tbody[row] <- sprintf('<tr class="%s">%s</tr>', htmlEscape(cls), tbody[row])
    } else {
      tbody[row] <- sprintf('<tr>%s</tr>', tbody[row])
    }
  }
  
  tbody2 <- paste0(tbody, collapse='')
  
  HTML(
    '<div class="table-responsive"><table style="font-size:100%;" class="data table table-condensed"><thead><tr>', 
    thead, 
    '</tr></thead><tbody>',
    tbody2,
    '</tbody></table></div>'
  )
}

relabelMethod <- function(x) {
	levels(x)[levels(x)=="reMA"] <- "RE"
	levels(x)[levels(x)=="PET.lm"] <- "PET"
	levels(x)[levels(x)=="PET.rma"] <- "PET"
	levels(x)[levels(x)=="PEESE.lm"] <- "PEESE"
	levels(x)[levels(x)=="PEESE.rma"] <- "PEESE"
	levels(x)[levels(x)=="PETPEESE.lm"] <- "PET-PEESE"
	levels(x)[levels(x)=="PETPEESE.rma"] <- "PET-PEESE"
	levels(x)[levels(x)=="pcurve"] <- "p-curve"
	levels(x)[levels(x)=="pcurve.evidence"] <- "p-curve"
	levels(x)[levels(x)=="puniform"] <- "p-uniform"	
	return(x)
}

selectPETPEESEmodel <- function(x, model) {
	if (model == "lm") {
		x <- x %>% 
			filter(!method %in% c("PET.rma", "PEESE.rma", "PETPEESE.rma")) %>% 
			mutate(method = relabelMethod(method)) %>% 
			filter(method %in% methodOrder)
			
	} else if (model == "rma") {
		x <- x %>% 
			filter(!method %in% c("PET.lm", "PEESE.lm", "PETPEESE.lm")) %>% 
			mutate(method = relabelMethod(method)) %>% 
			filter(method %in% methodOrder)
	}
	return(x)
}

load("summ.RData")
load("hyp.wide.RData")

H1.stroke <- "black"
H1.fill <- "grey20"
H0.stroke <- "steelblue2"
H0.fill <- "skyblue"

# Prepare data for hypothesis test plot

#RR$TypeI.excess <- cut(RR$TypeI, breaks=c(0, .05, .10, 1), labels=c("skyblue", "orange", "red"))
#RR$qrpEnv <- factor(RR$qrp.label, levels=c("QRP = none", "QRP = med", "QRP = high"), labels=c("none", "med", "high"))
#RR$shape <- as.character(factor(RR$qrp.label, labels=c("circle", "square", "triangle-up")))
RR.H1 <- summ %>% select(k, delta, qrp.label, qrpEnv, selProp, selProp.label, tau.label, method, TypeI, TypeI.excess, Power)
RR.H0 <- RR.H1 %>% filter(delta == 0) %>% select(-Power)
RR.H1 <- RR.H1 %>% select(-TypeI, -TypeI.excess)


# Prepare data for estimation plot

summ$stroke <- ifelse(summ$delta == 0, H0.stroke, H1.stroke)
summ$fill <- ifelse(summ$delta == 0, H0.fill, H1.fill)

summ2 <- summ %>% 
		filter(method %in% c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "PET.rma", "PEESE.rma", "PETPEESE.rma", "3PSM", "pcurve", "puniform")) %>% 
		ungroup()

# store estimation quantiles in long format		
summLong <- summ2  %>% 
		select(k, delta, qrp.label, selProp, selProp.label, tau.label, qrpEnv, method, stroke, fill, meanEst.pos, perc2.5.pos, perc97.5.pos, meanEst, perc2.5, perc97.5) %>% 
		melt(id.vars=c("k", "delta", "qrp.label", "selProp", "selProp.label", "tau.label", "qrpEnv", "method", "stroke", "fill"), na.rm=FALSE)


methodOrder <- c("RE", "TF", "PET", "PEESE", "PET-PEESE", "3PSM", "p-curve", "p-uniform")


	# input <- list(tau.label="tau = 0.2", k=10, delta=0.5, selProp=0.6, qrpEnv = "none", dropNegatives=TRUE, PETPEESEmodel = "lm")
	# ggDat.H0 <- RR.H0 %>% filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp)
shinyServer(function(input, output, session) {

	ggDat.H0 <- reactive({
		RR.H0 %>% 
			filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv) %>% 
			select(qrp.label, method, TypeI) %>% 
			selectPETPEESEmodel(model=input$PETPEESEmodel)
	})
	
	
	ggDat.H1 <- reactive({
		RR.H1 %>% 
			filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta==input$delta) %>% 
			select(qrp.label, method, Power) %>% selectPETPEESEmodel(model=input$PETPEESEmodel)
	})
	

	
	# renderer must be "canvas", otherwise NAs are displayed
	
	# ---------------------------------------------------------------------
	# Type I error plot
	
	verticalLineDataTypeI <- data.frame(value=c(.05, .05), method=rep(c("RE", "p-uniform"), 1))	
	
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
	
	verticalLineDataPow <- data.frame(value=c(.50, .50, .8, .8, 1, 1), method=rep(c("RE", "p-uniform"), 3))	
	
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
			return(c(-0.1, 1.8))
		} else {
			return(c(-2, 1.8))
		}
	})
	
	ggSumm0 <- reactive({
		summLong %>% 
			filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta == 0) %>% 
			filter(variable == paste0("meanEst", ifelse(input$dropNegatives == TRUE, ".pos", ""))) %>% 
			selectPETPEESEmodel(model=input$PETPEESEmodel)
	})
	
	ggQ0 <- reactive({
		summLong %>% 
			filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta == 0) %>% 
			filter(variable %in% c(paste0("perc2.5", ifelse(input$dropNegatives == TRUE, ".pos", "")), paste0("perc97.5", ifelse(input$dropNegatives == TRUE, ".pos", "")))) %>% 
			selectPETPEESEmodel(model=input$PETPEESEmodel)
	})
	
	ggSumm1 <- reactive({
		summLong %>% 
			filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta == input$delta) %>% 
			filter(variable == paste0("meanEst", ifelse(input$dropNegatives == TRUE, ".pos", ""))) %>% 
			selectPETPEESEmodel(model=input$PETPEESEmodel)
	})
	
	ggQ1 <- reactive({
		summLong %>% 
			filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta == input$delta)  %>% 
			filter(variable %in% c(paste0("perc2.5", ifelse(input$dropNegatives == TRUE, ".pos", "")), paste0("perc97.5", ifelse(input$dropNegatives == TRUE, ".pos", "")))) %>% 
			selectPETPEESEmodel(model=input$PETPEESEmodel)
	})
	
	# ggH1 <- reactive({
	# 	ggH1Label <- paste0("Bias-corrected estimate for delta = ", input$delta)
	#
	# 	summLongH1 <- summLong %>% filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta == input$delta, variable == paste0("meanEst", ifelse(input$dropNegatives == TRUE, ".pos", "")))
	#
	# 	Q1 <- summLong %>%
	# 	filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta == input$delta, variable %in% c(paste0("perc2.5", ifelse(input$dropNegatives == TRUE, ".pos", "")), paste0("perc97.5", ifelse(input$dropNegatives == TRUE, ".pos", ""))))
	#
	# 	verticalLineDataH1 <- data.frame(value=c(input$delta, input$delta), method=c("RE", "3PSM"))
	#
	# 	return(list(ggH1Label=ggH1Label, summLongH1=summLongH1, Q1=Q1, verticalLineDataH1=verticalLineDataH1))
	# })
	
	
	
	
	verticalLineDataH0 <- data.frame(value=c(0, 0), method=c("RE", "p-uniform"))
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
	
		verticalLineDataH1 <- data.frame(value=c(.2, .2, .5, .5, .8, .8), method=rep(c("RE", "p-uniform"), 3))
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
		filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv) %>% 
		select(-delta, -selProp) %>% 
		selectPETPEESEmodel(model=input$PETPEESEmodel)

	RR.H0.specific <- RR.H0 %>% 
		filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv) %>% 
		select(-delta, -selProp, -TypeI.excess) %>% 
		selectPETPEESEmodel(model=input$PETPEESEmodel)
				
	RR.wide <- inner_join(RR.H0.specific, RR.H1.specific, by = c("k", "qrp.label", "qrpEnv", "selProp.label", "tau.label", "method")) %>% 
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
		filter(k == input$k, tau.label == input$tau.label, selProp == input$selProp, qrpEnv == input$qrpEnv, delta %in% c(0, input$delta)) %>% 
		select(-k, -qrp.label, -selProp.label, -tau) %>% 
		#select(-selProp, -qrpEnv, -stroke, -fill, -condition, -k.label, -delta.label, -tau.label, -MAD, -consisZero, -consisZero.pos) %>% 
		select(-selProp, -qrpEnv, -stroke, -fill, -condition, -k.label, -delta.label, -tau.label, -MAD) %>% 
		selectPETPEESEmodel(model=input$PETPEESEmodel)
	
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

})
