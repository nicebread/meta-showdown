library(shiny)
library(shinythemes)
library(shinyBS) # Additional Bootstrap Controls


shinyUI(fluidPage(theme = shinytheme("spacelab"),
	tags$head(tags$link(rel="stylesheet", type="text/css", href="accordion.css")),	
	
	title = "Meta-Showdown Explorer",
	
	titlePanel("Meta-Showdown Explorer"),
	
	fluidRow(
		
		# ---------------------------------------------------------------------
		# The input panels, on the left side
		
		column(width=4,						
			radioButtons("selProp", "How many % of original studies are submitted to publication bias?:",
			             c("0%" = 0, "60%" = 0.6, "100%" = 1), inline=TRUE),
 			radioButtons("tau", "Heterogeneity (tau):",
 			             c("0" = 0, "0.25" = 0.25, "0.5" = 0.5), inline=TRUE),
  			radioButtons("k", "Number of studies in meta-analysis:",
  			             c("10" = 10, "30" = 30, "60" = 60, "100"=100)),
  			radioButtons("delta", "True effect size:",
  			             c("0" = 0, "0.2" = 0.2, "0.5" = 0.5, "0.8"=0.8)),
			radioButtons("qrpEnv", "QRP environment:",
			             c("none", "med", "high"), inline=TRUE),
			sliderInput("rep", "Simulated data set ID", 1, 10, value=1)
		),
		
		# ---------------------------------------------------------------------
		# The output panels, on the right side
		
		column(width=8, 
			fluidRow(
				column(width=5,
					htmlOutput("MA")					
				),
				column(width=7,			
					plotOutput("funnelplot")
				)
			), # of fluidrow 2
			fluidRow(
				column(width=12,
					plotOutput("estimateplot")					
				)
			) # of fluidrow 2
		)
	) # of fluidrow 1
))
