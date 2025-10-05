
library(shiny)
library(shinyjs)
library(tidyverse)
library(shinyWidgets)

load("all_results.RDa")

ui <- fluidPage(
  titlePanel("parkrun mutuals"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("group", "", c("ADAPAT" = "adapat", "WNR" = "wnr", "Bate Famliy" = "bate"), selected = "adapat", inline = TRUE),
      pickerInput("name", "Parkrunner", choices = NULL, selected = NULL),
      radioButtons("eventsruns", "Events or Runs", c("Events" = "events", "Runs" = "runs"), selected = "events", inline = TRUE),
      pickerInput("parkrun_name", "Parkruns", choices = NULL, selected = NULL, multiple = TRUE),
      switchInput("exclude_gm", "Exclude GM", value = FALSE, width = "100%"),
      numericInput("min", "Minimum", value=3)
      
    ),
    mainPanel(
      plotOutput("main")
    )
  )
)

server <- function(input, output, session) {
  
  group_names <- reactive({
    switch(input$group,
           "adapat" = c(
             "Adam BURNETT", "Alex BUCKLEY", "Andrew CARLSON", "Charlotte TURNER",
             "Frankie BALE", "Jonathan O'DONNELL", "Joseph GUNTRIP", "Luke DONALD",
             "Max LETCHFIELD", "Michael PETER", "Natalie HARPER", "Philip MOYLE",
             "Rachel BROWN", "Rob MOONEY", "Sebastian BATE", "Suzy HILL",
             "Tom ALMOND"
           ),
           "bate" = c(
             "Catherine BATE", "Ewan BATE", "Lawrence BATE", "Sebastian BATE"
           ),
           "wnr" = c(
             "Alexandra BERESFORD", "Callum SHINGLER", "Colm MULHERN",
             "Kirsty WATKINSON", "Gary SCOTT", "Helen ANDREWS",
             "Isabel PRECIOUS-BIRDS", "Jon SHAW", "Lindsay HASTON",
             "Lynda CLIFFORD", "Paul CLIFFORD", "Paul Thomas MULDOON",
             "Sebastian Bate"
           )
    )
  })
  
  observeEvent(group_names(), {
    updatePickerInput(session, "name", choices = group_names())
  })
  
  
  parkruns_for_name <- reactive({
    req(input$name)
    sort(unique((all_results %>% filter(name == input$name))$event))
  })
  
  observeEvent(parkruns_for_name(), {
    updatePickerInput(session, "parkrun_name", choices = parkruns_for_name(), selected = parkruns_for_name())
  })
  
  gm <- c("Alexandra", "Bolton", "Bramhall", "Burnage", "Chadderton Hall", "Cheadle Hulme", "Clarence", "Fletcher Moss", "Haigh Woodland", "Heaton", "Hyde", "Marple", "Oldham", "Peel", "Pennington Flash", "Philips Park", "Sale Water", "South Manchester", "Stamford Park", "Stretford", "Watergrove", "Woodbank", "Wythenshawe", "Worsley Woods")
  
  observe({
    current_parks <- input$parkrun_name
    
    if (input$exclude_gm) {
      prs <- setdiff(current_parks, gm[gm %in% current_parks])
    } else {
      prs <- union(current_parks, gm[!gm %in% current_parks])
    }
    
    updatePickerInput(session, "parkrun_name", selected = prs)
  })
  
  observeEvent(input$eventsruns,{
    min_val <- switch(input$eventsruns,
                      "events" = 3,
                      "runs" = 8)
    updateNumericInput(session, "min", value = min_val)
  })
  
  output$main=renderPlot({
    req(input$name, input$parkrun_name, input$eventsruns, input$group, input$min)
    
    source("bubbles.R")
    
    bubble(input$eventsruns,input$name,input$parkrun_name,input$group,input$min, data_in=all_results)
  })
  
}

shinyApp(ui = ui, server = server)

