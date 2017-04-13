library(shiny)
library(shinyBS) # Additional Bootstrap Controls
library(ggplot2)
library(dplyr)
library(ggvis)
library(reshape2)


load("summ.RData")
#load("res.hyp.RData")
load("RR.RData")

RR$TypeI.excess <- cut(RR$TypeI, breaks=c(0, .05, .10, 1), labels=c("skyblue", "orange", "red"))
RR.H1 <- RR %>% select(k, delta, qrp.label, selProp, selProp.label, tau.label, method, TypeI, TypeI.excess, Power)
RR.H0 <- RR.H1 %>% filter(delta == 0)



theme_metashowdown <- theme(
  panel.spacing =unit(.05, "lines"),
  panel.background = element_rect(fill="white"),
  panel.border = element_rect(color="grey90",fill=NA, size = 1), #element_blank(),
  panel.grid.minor= element_blank(),
  panel.grid.major= element_blank(),#element_line(color="grey90"),
  strip.background = element_rect(colour="white", fill="white"), #"grey93"
  axis.ticks = element_line(color="lightgrey"),
  legend.position = "bottom",
	axis.text = element_text(size = 20),
	axis.text.x = element_text(angle = 90, vjust=0.5)
)


	# input <- list(tau.label="tau = 0.2", k=10, delta=0.5)
shinyServer(function(input, output, session) {
	
	dat <- reactiveValues(
		RR.H0 = NULL,
		RR.H1 = NULL
	)
	
	observe({
		dat$RR.H0 <- RR.H0 %>% filter(k == input$k, tau.label == input$tau.label)
		dat$RR.H1 <- RR.H1 %>% filter(k == input$k, tau.label == input$tau.label, delta == input$delta)
	})
	
	output$plot <- renderUI({

		plotA <-  ggplot(dat$RR.H0, aes(x=method, shape=qrp.label)) +
		  geom_hline(yintercept=c(.05, .80), linetype="dotted", color="grey60") +
		  geom_point(aes(y=TypeI, colour=TypeI.excess, fill=TypeI.excess), position=position_dodge(width=.7), size = 5) +
			geom_point(data=dat$RR.H1, aes(y=Power), position=position_dodge(width=.7), size = 2.5, color="black", fill="black") +
		 # coord_flip() +
		  facet_grid(~selProp.label) +
		  scale_y_continuous(limits=c(0, 1), breaks = c(.05, .5, .8)) +
		  scale_shape_manual(values=c(21, 22, 24)) +
			scale_colour_manual(name="False positivr rate", values=c("skyblue", "orange", "red")) +
			scale_fill_manual(name="False positivr rate", values=c("skyblue", "orange", "red")) +
		  ylab("% false positives / Statistical Power") +
		  xlab("Heterogeneity (tau)") +
			theme_metashowdown

			return(list(
				HTML("<h1>False positive error rates and statistical power</h1>"),
				renderPlot(plotA)
			))
	})


		

		
	  # ggvis(dat, x=~method, ~TypeI) %>%
	  # layer_points() %>%
	  # bind_shiny("ggvis", "ggvis_ui")

})
