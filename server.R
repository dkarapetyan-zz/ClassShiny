# TODO: Createe RShiny version of graph_forecast server, and deploy
# 
# Author: David Karapetyan
###############################################################################

require(scales)
require("ClassModelForecast")
require("Hmisc")

shinyServer(function(input, output) {
			
			
			
			output$ui <- renderUI({
						if (is.null(input$book))
							return()
						
# Depending on input$book, we'll generate a different
# UI component and send it to the client.
						switch(input$book,
								"Capital" = selectInput("dynamic_var", "Choose a variable:",
										choices = c(
												"Allowed DTA",
												"Capital",
												"Dividends",
												"Leverage Ratio",
												"Net Income",
												"Net Income Before Tax",
												"Taxes",
												"Tier 1 Common Capital"),
										selected = "Net Income"),
								"LLL" = selectInput("dynamic_var", "Choose a variable:",
										choices = c(
												"Total Reserves",
												"Provision",
												"Total Net Charge-offs",
												"4-Qrt Net Charge-offs"	
										),
										selected = "Total Reserves"),
								"AFS" =  selectInput("dynamic_var", "Choose a variable:",
										choices = c(
												"Return on AFS Securities",
												"Total AFS Securities",
												"Gain AFS Securities"),
										selected = "Total AFS Securities"),
								"NCO" = selectInput("dynamic_var", "Choose a variable:",
										choices = c(
												"FirstLien Residential Real Estate",
												"Junior Lien Residential Real Estate",
												"HELOC Residential Real Estate",
												"Construction Commercial Real Estate",
												"Multifamily Commercial Real Estate",
												"NonFarm NonResidential CRE",
												"Credit Card",
												"Other Consumer",
												"CI", 
												"Leases",
												"Other Real Estate",
												"Loans to Foreign Governments",
												"Agriculture",
												"Loans to Depository Institutions",
												"Other"),
										selected = "CI"),
								"PPNR" = selectInput("dynamic_var", "Choose a variable:",
										choices = c("Net Interest Income",
												"Non-Int / Non-Trade Income",
												"Trading Income", 
												"Compensation Exp",
												"Fixed Asset Exp",
												"Other Exp",
												"PPNR"),
										selected = "Return on Trading Assets"),
								"Loss" =  selectInput("dynamic_var", "Choose a variable:",
										choices = c(
												"Agriculture",
												"CI", 
												"Construction Commercial Real Estate",
												"Credit Card",
												"FirstLien Residential Real Estate",
												"HELOC Residential Real Estate",
												"Junior Lien Residential Real Estate",
												"Leases",
												"Loans to Depository Institutions",
												"Loans to Foreign Governments",
												"Multifamily Commercial Real Estate",
												"NonFarm NonResidential CRE",
												"Other",
												"Other Consumer",
												"Other Real Estate"
										),
										selected = "Credit Card")
						)
					})
			
			
			plot_data <- reactive({
						PrepareForPlot(
								input$book,
								input$dynamic_var,
								input$bank,
								"2014Q3",
								nco_data,
								ppnr_data,
								total_assets,
								capital_data,
								model_coefficients_ey,
								macro_forecasts	
						)})
			
#TODO next two lines cause errors with RShiny. Debug
			
			
			
			output$plot <- renderPlot({
						p <- ggplot(
								plot_data(), aes_string(x="Index",
										y=make.names(input$dynamic_var))
						)
						p <- p + ggtitle(paste(input$book, "Forecast"))
						p <- p + xlab("")
						p <- p + ylab(input$dynamic_var)
						p <- p + geom_point()
						p <- p + stat_smooth(method = loess)
						p <- p + scale_y_continuous(labels = comma) 	
						print(p)
					})
		})