library(shiny)
library(shinyBS) # Additional Bootstrap Controls
library(ggplot2)
library(meta)
library(dplyr)
library(meta)
library(metafor)
library(reshape2)


source("../../MA-methods/1-RMA.R")
source("../../MA-methods/2-p-curve.R")
source("../../MA-methods/3-PET-PEESE.R")


#source("helpers.R")

load("simDemo.RData")

shinyServer(function(input, output, session) {
	
	simDat <- reactiveValues()
	simDat$sim <- sim	# loaded from simDemo.RData
	
	observe({		
		filt <- paste0(
			"selProp == ", input$selProp, " & ", 
			"tau == ", input$tau, " & ", 
			"k == ", input$k, " & ", 
			"qrpEnv == '", input$qrpEnv, "' & ", 
			"unique == '", input$rep, "' & ", 
			"delta == ", input$delta)
		simDat$currentDataSet <- filter_(simDat$sim, filt)
		
		if (nrow(simDat$currentDataSet) > 0) {
			re.est <- reEst(d=simDat$currentDataSet$d, v=simDat$currentDataSet$v, long=TRUE)
			lm.est <- lmVarEst(simDat$currentDataSet$d, simDat$currentDataSet$v, long=TRUE)
			pcurve.est <- pcurveEst(t=simDat$currentDataSet$t, df=simDat$currentDataSet$N-2, B=500, progress=FALSE, long=TRUE, CI=FALSE)
			
			# add average study ES (D.mean, D.median) and # of significant studies to results object
			pcurve.est <- rbind(pcurve.est, data.frame(
	            method="pcurve",
	            variable=c("D.mean", "D.median", "D.mean.sig", "D.median.sig", "sig.studies"),
	            value=c(
					mean(simDat$currentDataSet$D[simDat$currentDataSet$D > 0], na.rm=TRUE), 
					median(simDat$currentDataSet$D[simDat$currentDataSet$D > 0], na.rm=TRUE), 
					mean(simDat$currentDataSet$D[simDat$currentDataSet$D > 0 & simDat$currentDataSet$p <= .05], na.rm=TRUE), 
					median(simDat$currentDataSet$D[simDat$currentDataSet$D > 0 & simDat$currentDataSet$p <= .05], na.rm=TRUE), 
					sum(simDat$currentDataSet$p[simDat$currentDataSet$D > 0] <= .05))))

			# combine analysis results
			res <- rbind(re.est, lm.est, pcurve.est)
			res$value <- round(res$value, 3)
			simDat$res2 <- dcast(res, method ~ variable, value.var="value")
		} else {
			simDat$res2 <- NULL
		}
	})
	
	output$funnelplot <- renderPlot({
		if (!is.null(simDat$res2)) {
			meta1 <- metagen(simDat$currentDataSet$d, simDat$currentDataSet$se)
			meta::funnel(meta1, ref=0, xlab="Effect size", cex=.5, pch=19, contour=c(0.9, 0.95))
		}
	})
	
	output$estimateplot <- renderPlot({
		if (!is.null(simDat$res2)) {
			plot(density(simDat$currentDataSet$d), xlim=c(-1.5, 1.5), ylab="Density", xlab="Effect size (d)", main="Density of effect sizes in meta-analysis")
			abline(v=input$delta, col="darkgreen", lwd=2)
			abline(v=simDat$res2[simDat$res2$method=="RE", "d"], col="red", lwd=2)
			abline(v=simDat$res2[simDat$res2$method=="PET", "d"], col="blue", lwd=2)
			abline(v=simDat$res2[simDat$res2$method=="PEESE", "d"], col="orange", lwd=2)
			abline(v=simDat$res2[simDat$res2$method=="PET-PEESE", "d"], col="black", lwd=2)
			abline(v=simDat$res2[simDat$res2$method=="pcurve", "d"], col="brown", lwd=2)
		}
	})
	
	output$MA <- renderUI({
		if (!is.null(simDat$res2)) {
			return(list(
				HTML("<b>True d = ", input$delta, "</b>"),
				br(),
				HTML("RMA estimate = ", simDat$res2[simDat$res2$method=="RE", "d"], "[", simDat$res2[simDat$res2$method=="RE", "lb"], ";", simDat$res2[simDat$res2$method=="RE", "ub"], "]"),
				br(),
				HTML("PET estimate = ", simDat$res2[simDat$res2$method=="PET", "d"], "[", simDat$res2[simDat$res2$method=="PET", "lb"], ";", simDat$res2[simDat$res2$method=="PET", "ub"], "]"),
				br(),
				HTML("PEESE estimate = ", simDat$res2[simDat$res2$method=="PEESE", "d"], "[", simDat$res2[simDat$res2$method=="PEESE", "lb"], ";", simDat$res2[simDat$res2$method=="PEESE", "ub"], "]"),
				br(),
				HTML("PET-PEESE estimate = ", simDat$res2[simDat$res2$method=="PET-PEESE", "d"], "[", simDat$res2[simDat$res2$method=="PET-PEESE", "lb"], ";", simDat$res2[simDat$res2$method=="PET-PEESE", "ub"], "]"),
				br(),
				HTML("pcurve estimate = ", simDat$res2[simDat$res2$method=="pcurve", "d"], "[", simDat$res2[simDat$res2$method=="pcurve", "lb"], ";", simDat$res2[simDat$res2$method=="pcurve", "ub"], "]")			
			))
		}
	})

})
