library(shiny)
library(shinythemes)
library(shinyBS) # Additional Bootstrap Controls
library(ggvis)

alert.create <- function(content, style="info") {
  HTML(paste0('<div class="alert alert-', style, ' alert-dismissible" role="alert">'),
    '<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>',
    content, 
    '</div>'
  )
}

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
			
			# input options for the first three output panels
			conditionalPanel("input.tabs1 != 'Method performance check'",
			
				br(),
				helpText("What setting describes best the analyzed research environment?"),
			
				h2("Basic settings"),
				radioButtons("censor", "Severity of publication bias:",
				 			             c("none" = "none", "medium" = "med", "high" = "high"), inline=TRUE),
				radioButtons("tau", "Heterogeneity (tau):",
				 			             c("0" = 0, "0.2" = 0.2, "0.4" = 0.4), inline=TRUE),
				radioButtons("k", "Number of studies in meta-analysis:",
				             c("10" = 10, "30" = 30, "60" = 60, "100"=100), inline=TRUE),
									 
				conditionalPanel("input.tabs1 != 'Funnel plots'",
					radioButtons("delta", "True effect size under H1 (for power computation)",
					             c("0.2" = 0.2, "0.5" = 0.5, "0.8"=0.8), inline=TRUE)
				),
				conditionalPanel("input.tabs1 == 'Funnel plots'",
					radioButtons("deltaFull", "True effect size",
					             c("0" = 0, "0.2" = 0.2, "0.5" = 0.5, "0.8"=0.8), inline=TRUE)
				),
			
				radioButtons("qrpEnv", "QRP environment:",
				             c("none", "med", "high"), inline=TRUE)
			),
			
			# input options for the methods performance checke
			conditionalPanel("input.tabs1 == 'Method performance check'",
				
				br(),
				helpText("Please select all conditions that are plausible for the meta-analyzed research environment. Check at least one for each dimension!"),
				
				h2("Basic settings"),
				
				checkboxGroupInput("censor_perf", "Severity of publication bias:",
				 			       choices = c("none" = "none", "medium" = "med", "high" = "high"), 
										 selected = c("none" = "none", "medium" = "med", "high" = "high"), inline=TRUE),
				checkboxGroupInput("tau_perf", "Heterogeneity (tau):",
				 			       choices = c("0" = 0, "0.2" = 0.2, "0.4" = 0.4), 
										 selected = c("0" = 0, "0.2" = 0.2, "0.4" = 0.4), inline=TRUE),
				checkboxGroupInput("k_perf", "Number of studies in meta-analysis:",
				             choices = c("10" = 10, "30" = 30, "60" = 60, "100"=100),
										 selected = c("10" = 10, "30" = 30, "60" = 60, "100"=100), inline = TRUE),									 
				checkboxGroupInput("delta_H1_perf", "True effect size under H1",
					           choices = c("0.2" = 0.2, "0.5" = 0.5, "0.8"=0.8), 
										 selected = c("0" = 0, "0.2" = 0.2, "0.5" = 0.5, "0.8"=0.8), inline=TRUE),
				checkboxGroupInput("qrpEnv_perf", "QRP environment:",
				             choices = c("none", "med", "high"), 
										 selected = c("none", "med", "high"), inline=TRUE),
				radioButtons("evaluatedMethod", "Method to evaluate", c("reMA", "TF", "WAAP-WLS", "PET", "PEESE", "PETPEESE", "3PSM", "4PSM", "pcurve", "puniform")),

				
				h2("Good performance is defined as ..."),
				helpText("Fields without a value are not evaluated; all other fields are combined with a logical AND (i.e., all entered conditions must be true to result in a good performance).
				As p-curve does not provide CIs, it is never positively evaluated if you enter a number there."),
				textInput("ME_upper", "... a maximum upward bias (i.e., positive deviation of the average estimate from true delta): ", value = ""),
				textInput("ME_lower", "... a maximum downward bias (i.e., negative deviation of the average estimate from true delta): ", value = ""),
				textInput("MAD_upperbound", "... a maximum mean absolute error (MAE) of: ", value = ""),
				textInput("RMSE_upperbound", "... a maximum root mean square error (RMSE) of: ", value = ""),
				textInput("coverage_lowerbound", "... a minimum coverage of the 95% CI in percentage (default: 95): ", value = ""),
				textInput("TypeI_upperbound", "... a maximum false positive rate in percentage (default: 5): ", value = ""),
				checkboxInput("show_performance_table", "Show performance results in table", FALSE)
				
			), # of conditionalPanel
			
			
			# Advanced options
		 conditionalPanel("input.tabs1 != 'Funnel plots'",
				h2("Advanced options"),			
				#radioButtons("PETPEESEmodel", "Model PET/PEESE as:",
			             #c("lm (default)" = "lm", "rma" = "rma"), inline=TRUE),
								 
				conditionalPanel("input.tabs1 == 'Estimation'",
					selectInput("dropNegatives", "Set negative estimates to zero:",
			             c("Keep all estimates, regardless of sign (default)" = FALSE, "Set to zero" = TRUE))									 
				)
			),
						
			conditionalPanel("input.tabs1 == 'Funnel plots'",
 				sliderInput("demoDatID", "Demo data set (1 to 10)", min=1, max=10, step=1, value=1),
				helpText("For each condition, this app provides 10 demo data sets (the data sets are not simulated on the fly, as this would need too much computing time)."),
				checkboxInput("show_PET", "Show PET meta-regression in plot", TRUE),
				checkboxInput("show_PEESE", "Show PEESE meta-regression in plot", TRUE)
 			),
														 
			conditionalPanel("input.tabs1 != 'Funnel plots' & input.tabs1 != 'Method performance check'",
				h2("Output options"),
				radioButtons("plotOrTable", "Output as:", c(Plot="Plot", Table="Table"), inline=TRUE)
			)
		),
		
		
		
		## ======================================================================
		## The output panels, on the right side
		## ======================================================================
		
		column(width=8, 
			
			alert.create("Please note: All results covered in this app are based on two-group t-tests and assume a certain distribution of sample sizes in the primary studies."),
			alert.create("This is the updated app based on the current revision of our paper (submitted to AMPPS).", style="info"),
			
			conditionalPanel("input.tabs1 == 'Estimation'",
				uiOutput("cap_alert")
			),
			
			tabsetPanel(id ="tabs1",	
			
			# ---------------------------------------------------------------------
			# Funnel plots
			
				tabPanel("Funnel plots",
					h2("Typical funnel plots for this condition"),
					column(width=8,
						plotOutput("funnelplot"),
						helpText("<-- Slide through demo data set 1 to 10 to see some other exemplary funnel plots for this condition.")
					),
					column(width=4,
						#uiOutput("funnelplotAnnotation")
						HTML("<ul>
							<li>Blue triangle is the region of non-significance; dotted black triangle is the funnel of the naive random-effects meta-analysis.</li>
							<li>Red dot at the bottom shows the true effect size. Blue dots show the naive random-effects estimate, and PET and PEESE estimates, if selected.</li>
							</ul>
							")
					)					
				),
				
				# ---------------------------------------------------------------------
				# Hypothesis test plots
				
				tabPanel("Hypothesis test",
					h2("Is there an effect or not?"),
					helpText(HTML("Note: H0 is rejected if the p-value is < .05 <i>and</i> the estimate is in the expected direction.")),
					conditionalPanel(condition="input.plotOrTable == 'Plot'",
						h3("Under H0"),
						p("If in reality there is no effect: What is the probability that a method falsely concludes 'There is an effect'?"),
						uiOutput("ggvis_ui_TypeI"),
						ggvisOutput("ggvis_TypeI"),
						
						h3("Under H1"),
						p("If in reality there is an effect: What is the probability that a method detects it?"),
						uiOutput("ggvis_ui_Power"),
						ggvisOutput("ggvis_Power")
					),
					conditionalPanel(condition="input.plotOrTable == 'Table'",
						uiOutput("hypTable")
					),
					
					helpText("RE = random effects meta-analysis, TF = trim-and-fill, PET = precision effect test, PEESE = precision effect estimate with standard errors, PET-PEESE = conditional estimator, 3PSM = three parameter selection model, 4PSM = four parameter selection model, WAAP = weighted average of adequately powered studies, WLS = Weigthed least squares estimator, WAAP-WLS = conditional estimator")
				),
				
				# ---------------------------------------------------------------------
				# Estimation plots
				
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
					),
					
					helpText("RE = random effects meta-analysis, TF = trim-and-fill, PET = precision effect test, PEESE = precision effect estimate with standard errors, PET-PEESE = conditional estimator, 3PSM = three parameter selection model, 4PSM = four parameter selection model, WAAP = weighted average of adequately powered studies, WLS = Weigthed least squares estimator, WAAP-WLS = conditional estimator"),
					
					conditionalPanel(condition="input.plotOrTable != 'Table'",					
						helpText("Horizontal error bars are 95% quantiles (i.e., 95% of simulated replications were in that range).")
					)
				),
				
				
				# ---------------------------------------------------------------------
				# check performance of methods
				
				tabPanel("Method performance check",
					h2("Under which conditions does a method perform well?"),
					uiOutput("perfPlot")
				),	
				
				# ---------------------------------------------------------------------
				# About
				
				tabPanel("About", loadHTML('About.html'))
			) # of tabsetPanel
		) # of column
	) # of fluidrow 1
))
