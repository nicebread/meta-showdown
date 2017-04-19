library(shiny)
library(shinythemes)
library(shinyBS) # Additional Bootstrap Controls
library(ggvis)


loadHTML <- function(filename) {
  fileConnection <- file(filename, encoding="UTF-8")
  text <- readChar(fileConnection, file.info(filename)$size, useBytes = TRUE)
  Encoding(text) <- "UTF-8"
  close(fileConnection)  
  HTML(text)
}



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
  			             c("10" = 10, "30" = 30, "60" = 60, "100"=100), inline=TRUE),
  			radioButtons("delta", "True effect size under H1 (for power computation)",
  			             c("0.2" = 0.2, "0.5" = 0.5, "0.8"=0.8), inline=TRUE),
				radioButtons("qrpEnv", "QRP environment:",
				             c("none", "med", "high"), inline=TRUE),
				conditionalPanel("input.tabs1 == 'Estimation'",
					 selectInput("dropNegatives", "Set negative estimates to zero & discard CIs of negative estimates:",
				             c("Set to zero" = TRUE, "Keep all estimates (regardless of sign)" = FALSE))
				),
				radioButtons("plotOrTable", "Output as:", c(Plot="Plot", Table="Table"), inline=TRUE)
		),
		
		# ---------------------------------------------------------------------
		# The output panels, on the right side
		
		column(width=8, 
			tabsetPanel(id ="tabs1",				
				tabPanel("Hypothesis test",
					h2("Is there an effect or not?"),
					helpText(HTML("Note: H0 is rejected if the p-value is < .05 <i>and</i> the estimate is in the expected direction.")),
					conditionalPanel(condition="input.plotOrTable == 'Plot'",
						h3("Under H0"),
						uiOutput("ggvis_ui_TypeI"),
						ggvisOutput("ggvis_TypeI"),
						h3("Under H1"),
						uiOutput("ggvis_ui_Power"),
						ggvisOutput("ggvis_Power")
					),
					conditionalPanel(condition="input.plotOrTable == 'Table'",
						uiOutput("hypTable")
					)
				),
				tabPanel("Estimation",
					h2("Bias-corrected estimates of the true effect"),
					conditionalPanel("input.dropNegatives == 'TRUE'",
						helpText("Note: Negative estimates are set to zero.")
					),
					conditionalPanel(condition="input.plotOrTable == 'Plot'",
						h3("Under H0"),
						uiOutput("ggvis_ui_Estimation_H0"),
						ggvisOutput("ggvis_Estimation_H0"),
						h3("Under H1"),
						uiOutput("ggvis_ui_Estimation_H1"),
						ggvisOutput("ggvis_Estimation_H1")
					),
					conditionalPanel(condition="input.plotOrTable == 'Table'",
						uiOutput("estTable")
					)
				),
				tabPanel("About", loadHTML('About.html'))
			) # of tabsetPanel
		) # of column
	) # of fluidrow 1
))
