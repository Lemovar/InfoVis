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

relevancies_df <- read.csv("relevancies.csv", sep = ";")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("RECOMMANDER"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(4,
           p("."),
           p("."),
           # verbatimTextOutput("debug"),
           fluidRow(
             column(7,
                    htmlOutput('label')
             ),
             column(5,
                    htmlOutput('state'),
                    hr(),
                    selectInput("lymph", label = NULL,
                               c("no value (current)" = "none",
                                 "bilateral" = "relevancies_2"),
                               selected = "none"),
                    selectInput("metha", label = NULL,
                                c("no value (current)" = "none",
                                  "M0" = "relevancies_3"),
                                selected = "none")
             )
           )
           ),
    column(8,
           plotlyOutput("parallelBlocksPlot")
           )
  )
)
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output$debug <- renderText({
  #   relevancies_df <- read.csv("relevancies.csv", sep = ";")
  #   relevancies_df$label
  # })
  output$label <- renderUI({
    HTML(paste(relevancies_df$label,collapse="<hr {border-top: 1px solid #000000;}/>"))
  })
  
  output$state <- renderUI({
    HTML(paste(relevancies_df$state_1[1:9],collapse=" (current)<hr {border-top: 1px solid #000000;}/>"))
  })
  
  output$attributeTable <- renderTable({
    relevancies_df <- read.csv("relevancies.csv", sep = ";")
    #relevancies_df[c("label")]
    relevancies_df[c("label", "state_2")]
  })
  
  # output$attributeTable <- renderDataTable({
  #   relevancies_df <- read.csv("relevancies.csv", sep = ";")
  #   relevancies_df[c("label", "state_1", "relevancies_1")]
  # })
  
  output$parallelBlocksPlot <- renderPlotly({
    p <- plot.new()
    columns <- c("relevancies_1")
    if(input$metha != "none") {
      columns <- c(columns, input$metha)
    } else if(input$lymph != "none") {
      columns <- c(columns, input$lymph)
    }
    parallel_blocks(columns)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

