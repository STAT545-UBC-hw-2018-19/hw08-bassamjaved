library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)

#setwd("~/Documents/STAT\ 545/hw08-bassamjaved/IHDS")
load("ihds_excerpt.rda")
ihds <- ihds_excerpt

ui <- fluidPage(
	theme = shinytheme("journal"),
	titlePanel("India Human Development Survey"),
	sidebarLayout(
		sidebarPanel(
			sliderInput("incomeInput", "Select income range",
									min = -1037040, max = 11360000, value = c(0, 500000), pre="Rs "),
			checkboxInput("incomeSortInput", "Sort income by ascending in table", FALSE),
			radioButtons("vehicleInput", "Vehicle ownership",
									 choices = c("(1) Yes 1", "(0) No 0"),
									 selected = "(0) No 0"),
			conditionalPanel(condition = "input.vehicleInput == '(1) Yes 1'",
											 radioButtons("vehicleType", "Type of vehicle owned",
											 						 choices = c("Automobile", "Motor scooter", "Either"),
											 						 selected = "Either")
			),
			selectInput("metroInput", "Select metro region",
									choices = c("(0) non-metro", "(1) Mumbai 1", "(2) Delhi 2",
															"(3) Kolkata 3", "(4) Chennai 4", "(5) Bangalore 5",
															"(6) Hyderabad 6"),
									selected = "(0) non-metro")
		),
		mainPanel(
			tags$em("Note: This is an excerpt of vehicle ownership and electricity access data.",
							"For full dataset by University of Maryland, visit:"),
			tags$a(href="https://ihds.umd.edu",
						 "India Human Development Survey"),
			tabsetPanel(
				tabPanel("Table", dataTableOutput("ihds_table")),
				tabPanel("Plot", plotOutput("ihds_plot"))
			),
			tags$hr(),
			tags$b("Variables"),
			tags$ol("IDHH = participant ID", br(),
							"INCOME = total income", br(),
							"CGMOTORV = Ownership of any motor vehicle", br(),
							"CG21 = Automobile ownership", br(),
							"CG8 = Motor scooter ownership", br(),
							"FU1 = Electricity access", br(),
							"METRO = residence in metro region", br(),
							"METRO6 = metro region")
		)
		)
	)

server <- function(input, output) {
	observe(print(input$vehicleInput))
	ihds_filtered <- reactive({
		ihds %>% 
			filter(CGMOTORV == input$vehicleInput,
						 INCOME > input$incomeInput[1],
						 INCOME < input$incomeInput[2]) %>% 
			{if(input$incomeSortInput) arrange(., INCOME) else(.)} %>% 
			{if(input$vehicleType == "Motor scooter") filter(., CG8 == "(1) Yes 1") else(.)} %>% 
			{if(input$vehicleType == "Automobile") filter(., CG21 == "(1) Yes 1") else(.)} %>% 
			{if(input$metroInput == "(0) non-metro") 
				filter(., METRO == input$metroInput) else(.)} %>% 
			{if(input$metroInput != "(0) non-metro")
				filter(., METRO6 == input$metroInput) else(.)}
	}
	)
	output$ihds_plot <- renderPlot({
		ihds_filtered() %>%
			ggplot(aes(x = INCOME, color = FU1, fill = FU1)) +
			geom_histogram(alpha = 0.25, na.rm = TRUE) +
			theme_classic()
	})
	output$ihds_table <- renderDataTable({
		ihds_filtered()
	})
}

shinyApp(ui = ui, server = server)