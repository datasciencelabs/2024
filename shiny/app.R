## TO DO
## add an action button (all dates) and an observeEvent in server
# -- Set up
library(tidyverse)
library(lubridate)
library(DT)
library(shiny)
library(shinythemes)
library(plotly)

download.file("https://github.com/rafalab/covidpr/raw/refs/heads/main/rdas/data.rda", "data.rda")

source("functions.R")

first_day <- make_date(2020, 3, 12)
last_day <- today() - days(1)
button_style <- "color: black; background-color: rgb(230, 220, 205); position: relative; 
                     text-align:center;border-radius: 6px; border-width: 2px"


ui <- fluidPage(theme = shinytheme("sandstone"), 
                ## Application title
                titlePanel("COVID-19 Dashboard"),
                
                # Sidebar with a slider input for number of bins
                sidebarLayout(
                  sidebarPanel(
                    dateRangeInput("range", "Period", 
                                   format = "M-dd-yyyy",
                                   start = last_day - days(90),
                                   end = last_day,
                                   min = first_day,
                                   max = last_day,
                                   width = "100%"),
                    br(), 
                    actionButton("alldates", "All dates", 
                                 style = button_style),
                    width = 3),
                  
                  mainPanel(
                    tabsetPanel(id = "tabs",
                                tabPanel("Data",
                                         dataTableOutput("table")
                                ),
                                
                                tabPanel("Positivity",
                                         h4("Positivity rate"),
                                         plotOutput("positivity"))
                    )
                  )
                )
)

server <- function(input, output, session){
  
  load("data.rda")

  observeEvent(input$alldates, {
    updateDateRangeInput(session, "range",
                         start = first_day,
                         end   = last_day)
  })
  
  ## -- This is used to print table in app
  output$table <- DT::renderDataTable({
    make_table(tests, hosp_mort,
               start_date = input$range[1],
               end_date = input$range[2])
    
  }, server = FALSE)

  ## -- This creates the positivity rate figure
  output$positivity <- renderPlot(
    plot_positivity(tests, start_date = input$range[1],
                    end_date = input$range[2])
  )
}


shinyApp(ui = ui, server = server)



