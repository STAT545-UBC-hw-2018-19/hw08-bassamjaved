library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)

setwd("~/Documents/STAT\ 545/hw08")
load("IHDS-II.Rda")
ihds <- da36151.0002 %>% select(IDHH, INCOME, FU1, CGMOTORV, CG8, CG21, METRO, METRO6) %>% 
  head(5000)

ui <- fluidPage(
  titlePanel("India Human Development Survey"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("incomeInput", "Select income range",
                  min = 0, max = 1000000, value = c(0, 100000), pre="Rs"),
      radioButtons("vehicleInput", "Vehicle ownership",
                   choices = c("(1) Yes 1", "(0) No 0"),
                   selected = "(0) No 0"),
      conditionalPanel(condition = "input.vehicleInput == '(1) Yes 1'",
                       radioButtons("vehicleType", "Type of vehicle owned",
                                    choices = c("Automobile", "Motor scooter", "Either"),
                                    selected = "Either")
      ),
      radioButtons("electricityInput", "Electricity access",
                   choices = c("(1) Yes 1", "(0) No 0"),
                   selected = "(0) No 0"),
      checkboxInput("incomeSortInput", "Sort income by ascending", FALSE),
      selectInput("metroType", "Select metro type",
                  choices = c("(0) non-metro", "(1) metro"),
                  selected = "(0) non-metro")
 #     conditionalPanel(condition = "input.metroType == (1) metro",
  #                     radioButtons("metroInput", "Select metro region",
   #                                 choices = c(1,2,3,4,5,6),
    #                                selected = 1)
     # )
    ),
    mainPanel(tabsetPanel(
      tabPanel("Table", dataTableOutput("ihds_table")),
      tabPanel("Plot", plotOutput("ihds_plot"))
      )
    )
  ),
  theme = shinytheme("journal")
)

server <- function(input, output) {
  observe(print(input$vehicleInput))
  ihds_filtered <- reactive({
    ihds %>% 
      filter(CGMOTORV == input$vehicleInput,
             INCOME > input$incomeInput[1],
             INCOME < input$incomeInput[2],
             METRO == input$metroType) %>% 
      {if(input$incomeSortInput) arrange(., INCOME) else(.)} %>% 
      {if(input$vehicleType == "Motor scooter") filter(., CG8 == "(1) Yes 1") else(.)} %>% 
      {if(input$vehicleType == "Automobile") filter(., CG21 == "(1) Yes 1") else(.)} #%>% 
      #{if(input$electricityInput == "(1) Yes 1") filter(., FU1 == "(1) Yes 1") else(.)} %>% 
      #{if(input$electricityInput == "(0) No 0") filter(., FU1 == "(0) No 0") else(.)}
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

#references
#https://stackoverflow.com/questions/30604107/r-conditional-evaluation-when-using-the-pipe-operator
