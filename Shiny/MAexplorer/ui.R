library(shiny)
library(shinythemes)
library(shinyBS) # Additional Bootstrap Controls
library(ggvis)


shinyUI(fluidPage(theme = shinytheme("spacelab"),
	tags$head(tags$link(rel="stylesheet", type="text/css", href="accordion.css")),	
	
	title = "Meta-Showdown Explorer",
	
	titlePanel("Meta-Showdown Explorer"),
	
	fluidRow(
		
		# ---------------------------------------------------------------------
		# The input panels, on the left side
		
		column(width=4,
			br(),
			br(),
			br(),
				radioButtons("selProp", "How many % of original studies are submitted to publication bias?:",
				 			             c("0%" = 0, "60%" = 0.6, "90%" = 0.9), inline=TRUE),
				radioButtons("tau.label", "Heterogeneity (tau):",
				 			             c("0" = "tau = 0", "0.2" = "tau = 0.2", "0.4" = "tau = 0.4"), inline=TRUE),
  			radioButtons("k", "Number of studies in meta-analysis:",
  			             c("10" = 10, "30" = 30, "60" = 60, "100"=100)),
  			radioButtons("delta", "True effect size under H1 (for power computation)",
  			             c("0.2" = 0.2, "0.5" = 0.5, "0.8"=0.8)),
				radioButtons("qrpEnv", "QRP environment:",
				             c("none", "med", "high"), inline=TRUE)
		),
		
		# ---------------------------------------------------------------------
		# The output panels, on the right side
		
		column(width=8, 
			tabsetPanel(id ="tabs1",				
				tabPanel("Hypothesis test",
					h2("Is there an effect or not?"),
					uiOutput("ggvis_ui_TypeI"),
					ggvisOutput("ggvis_TypeI"),
					uiOutput("ggvis_ui_Power"),
					ggvisOutput("ggvis_Power")
				),
				tabPanel("Estimation",
					h2("How biased is the estimated true effect?"),
					uiOutput("ggvis_ui_Estimation_H0"),
					ggvisOutput("ggvis_Estimation_H0"),
					uiOutput("ggvis_ui_Estimation_H1"),
					ggvisOutput("ggvis_Estimation_H1")
				)
			)
		)
	) # of fluidrow 1
))
