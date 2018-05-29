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



# Define UI for application that draws a histogram ----
ui <- fluidPage(
  
  # Application title
  titlePanel("Thuggest"),
  mainPanel(
    tabsetPanel(type = "pills",
                tabPanel("Overview",
                         plotlyOutput("probsOverviewPlot")),
                tabPanel("Details", 
                         #fluidRow(),
                         fluidRow(
                           column(5,
                                  plotlyOutput("probsChangesPlot")
                           ),
                           column(6,
                                  HTML("<br>"),
                                  HTML("<br>"),
                                  # verbatimTextOutput("debug"),
                                  fluidRow(
                                    column(7, style='padding:0px;',
                                           htmlOutput('label')
                                    ),
                                    column(5, style='padding:0px;', 
                                           htmlOutput('state'),
                                           HTML("<br>"),
                                           selectInput("lymph", label = NULL,
                                                       c("no value (current)" = "none",
                                                         "bilateral" = "2"),
                                                       selected = "none"),
                                           selectInput("metha", label = NULL,
                                                       c("no value (current)" = "none",
                                                         "M0" = "3"),
                                                       selected = "none")
                                    )
                                  )
                           ),
                           column(1,
                                  plotlyOutput("parallelBlocksPlot")
                           )
                         )
                )
           )
      )
)
  

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  relevancies_df <- read.csv("relevancies.csv", sep = ";")
  relevancies_df <- relevancies_df[order(-relevancies_df$relevancies_1),]
  
  probs_df <- read.csv("probs.csv", sep = ",")
  # cols <- c("#CAE2EE", "#79AED2", "#D1ECB9", "#85C680", "#FDC2C2", "#EE7677", 
  #           "#FED9A9", "#FFB266", "#DFD1E6", "#A68BC2", "#C0E4C0")
  cols <- c("#C0E4C0", "#A68BC2", "#DFD1E6", "#FFB266", "#FED9A9", "#EE7677",
            "#FDC2C2", "#85C680", "#D1ECB9", "#79AED2", "#CAE2EE")

  
  # output$debug <- renderText({
  #   relevancies_df <- read.csv("relevancies.csv", sep = ";")
  #   relevancies_df$label
  # })
  output$probsOverviewPlot <- renderPlotly({
    prob_bars(col_new = "distribution_1")
  })
  
  output$probsChangesPlot <- renderPlotly({
    p <- plot.new()
    if(input$metha != "none") {
      p <- prob_changes(x = probs_df[["therapy"]], y_old = probs_df[["distribution_1"]], 
                     y_new = probs_df[[paste("distribution_", input$metha, sep="")]])
    } else if(input$lymph != "none") {
      p <- prob_changes(x = probs_df[["therapy"]], y_old = probs_df[["distribution_1"]], 
                     y_new = probs_df[[paste("distribution_", input$lymph, sep="")]])
    } else {
      p <- prob_bars(col_new = "distribution_1")
    }
  })
  
  output$probsBarPlot <- renderPlotly({
    p <- plot.new()
    if(input$metha != "none") {
      p <- prob_bars(col_new = paste("distribution_", input$metha, sep=""), col_old = "distribution_1")
    } else if(input$lymph != "none") {
      p <- prob_bars(col_new = paste("distribution_", input$lymph, sep=""), col_old = "distribution_1")
    } else {
      p <- prob_bars(col_new = "distribution_1")
    }
  })
  
  output$state <- renderUI({
    #HTML(paste(relevancies_df$state_1[1:9],collapse=" (current)<hr {border-top: 1px solid #000000;}/>"))
    html_output <- paste('<head><style>
            table, th, td {
                         border: 1px solid black;
  }
                         </style></head><body><table>')
    
    for(i in 1:(length(relevancies_df$state_1)-2)) {
      html_output = paste(html_output,
                          '<tr><td height=60, width=350>',
                          as.character(relevancies_df$state_1[i])[1],
                          ' (current)',
                          '</td></tr>',
                          collapse = '')
    }
    
    html_output = paste(html_output, '</table></body>', sep = '')
    
    HTML(html_output)
  })
  
  output$label <- renderUI({
    #HTML(paste(relevancies_df$label,collapse="<hr {border-top: 1px solid #000000;}/>"))
    html_output <- paste('<head><style>
            table, th, td {
              border: 1px solid black;
            }
          </style></head><body><table>')

    for(i in 1:length(relevancies_df$label)) {
      html_output = paste(html_output,
                          '<tr><td bgcolor="', cols[i], '", height=60, width=450>',
                          as.character(relevancies_df$label[i])[1],
                          '</td></tr>',
                          collapse = '')
    }
    
    html_output = paste(html_output, '</table></body>', sep = '')
    
    HTML(html_output)
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
    columns <- c("relevancies_1")
    if(input$metha != "none") {
      columns <- c(columns, paste("relevancies_", input$metha, sep=""))
    } else if(input$lymph != "none") {
      columns <- c(columns, paste("relevancies_", input$lymph, sep=""))
    }
    parallel_blocks(columns)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

