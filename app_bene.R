#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(tidyverse)
library(Rtsne)
library(plotly)
library(reshape2)
setwd("C:/Studium/Computervisualistik/Informationsvisualisierung/Projekt/WS_project")
source(paste0(getwd(), "/auxil_bene.R"), echo = F)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("RECOMMANDER"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        radioButtons("comp_col", "Column to compare to:", 
                     c("None" = "none",
                       "Relevancies 2" = "relevancies_2",
                       "Relevancies 3" = "relevancies_3"),
                     select = "none")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        #verbatimTextOutput("debug")
        plotlyOutput("parallelBlocksPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output$debug <- renderText({
  #   columns = input$comp_col
  # })
  
  output$parallelBlocksPlot <- renderPlotly({
    p <- plot.new()
    columns <- c("relevancies_1")
    if(input$comp_col != "none") {
      columns <- c(columns, input$comp_col)
    }
    parallel_blocks(columns)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

