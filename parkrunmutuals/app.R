library(shiny)
library(shinyjs)
library(tidyverse)
library(shinyWidgets)
library(DT)
load("all_results.RDa")

all_results2=all_results %>% count(name, parkrunner) %>% filter(n>=3) %>% arrange(name,-n) %>% filter(name!=parkrunner)
ui <- navbarPage(
  "parkrun mutuals",
  tabPanel("Aggregate",
           sidebarLayout(
             sidebarPanel(
               radioButtons("group", "", c("ADAPAT" = "adapat", "WNR" = "wnr", "Bate Famliy" = "bate", "Others"="others"), selected = "adapat", inline = TRUE),
               pickerInput("name", "parkrunner", choices = NULL, selected = NULL),
               radioButtons("eventsruns", "Events or Runs", c("Events" = "events", "Runs" = "runs"), selected = "events", inline = TRUE),
               pickerInput("parkrun_name", "parkruns", choices = NULL, selected = NULL, multiple = TRUE),
               switchInput("exclude_gm", "Exclude GM", value = FALSE, width = "100%"),
               numericInput("min", "Minimum", value=3)
               
             ),
             mainPanel(
               plotOutput("main")
             )
           )),
  tabPanel("Head-to-Head",
           sidebarLayout(
             sidebarPanel(
               radioButtons("group2", "", c("ADAPAT" = "adapat", "WNR" = "wnr", "Bate Famliy" = "bate", "Others"="others"), selected = "adapat", inline = TRUE),
               pickerInput("name2", "parkrunner", choices = NULL, selected = NULL),
               selectizeInput("name_h2h", "Head-to-head", 
                              choices = as.character(0),
                              selected=NULL,
                              options = list(
                                placeholder = "Start typing…",
                                create = FALSE
                              )),
               pickerInput("parkrun_name2", "parkruns", choices = NULL, selected = NULL, multiple = TRUE),
               switchInput("exclude_all", "Exclude ALL", value = FALSE, width = "100%"),
               HTML("Minimum of 3 parkruns")
             ),
             mainPanel(
               htmlOutput("text"),
               dataTableOutput("main2")
             )
           ))
)


server <- function(input, output, session) {
  
  #panel 1
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
           ),
           "others"=c("Bob BAYMAN", "Helena ROBINSON", "Lawrence BATE","iamh CONROY VAN LEEUWEN")
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
  
  #panel 2
  
  #flow select names from the group
  #pick the rivals that have done the min prs
  #only display mutual prs
  # 
  # 
  group_names2 <- reactive({
    switch(input$group2,
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
           ),
           "others"=c("Bob BAYMAN", "Helena ROBINSON", "Lawrence BATE","iamh CONROY VAN LEEUWEN")
    )
  })
  
  observeEvent(group_names2(), {
    updatePickerInput(session, "name2", choices = group_names2())
  })
  
  get_rivals <- reactive({
    req(input$name2)
    unique((all_results2 %>% filter(name == input$name2))$parkrunner)
  })
  
  observeEvent(get_rivals(), {
    updatePickerInput(session, "name_h2h", choices = get_rivals(), selected = get_rivals())
    
  })
  
  
  
  observeEvent(get_rivals(), {
    req(length(get_rivals()) > 0)
    updateSelectizeInput(
      session,
      inputId = "name_h2h",
      choices = get_rivals(),
      selected = NULL,
      server = TRUE
    )
  })
  
  mutual_parkruns <- reactive({
    req(input$name2)
    req(input$name_h2h)
    sort(unique((all_results %>% filter(name == input$name2, parkrunner==input$name_h2h))$event) )
  })
  
  
  
  observeEvent(mutual_parkruns(), {
    updatePickerInput(session, "parkrun_name2", choices = mutual_parkruns(), selected = mutual_parkruns())
    
  })
  
  observe({
    current_parks2 <- mutual_parkruns()
    
    if (!input$exclude_all) {
      prs2 <- current_parks2
    } else {
      prs2 <- character(0)
    }
    
    updatePickerInput(session, "parkrun_name2", selected = prs2)
  })
  
  head_to_head <- reactive({
    req(input$name_h2h)
    req(input$name2)
    req(input$parkrun_name2)
    
    all_results %>%
      filter(name == input$name2, parkrunner %in% c(input$name_h2h, input$name2), event %in% input$parkrun_name2) %>%
      dplyr::select(-name) %>%
      mutate(rank=2-rank(pos), .by=c("event", "eventno"))
    
  })
  
  
  output$main2=renderDataTable({
    head_to_head() %>%
      dplyr::select(-rank, -pos) %>%
      pivot_wider(names_from = parkrunner, values_from = time) %>%
      na.omit() %>%
      dplyr::select(event, eventno, any_of(c(input$name2,input$name_h2h))) %>% 
      rename("Event"=event, "Number"=eventno)
  },options = list(
    autoWidth = TRUE,
    # scrollX=T,
    pageLength=20))
  
  output$text=renderUI({
    x=head_to_head()%>%
      dplyr::select(-pos,-time) %>%
      pivot_wider(names_from = parkrunner, values_from = rank) %>%
      na.omit() %>%
      pivot_longer(cols = c(input$name2,input$name_h2h), names_to="parkrunner", values_to = "rank")%>%
      summarise(wins=sum(rank), .by="parkrunner") %>%
      pivot_wider(names_from = parkrunner, values_from = wins) %>%
      select(any_of(c(input$name2,input$name_h2h)))
    
    leftwins=x[1,1]
    rightwins=x[1,2]
    
    HTML(paste("<h2>",input$name2, leftwins, "-", rightwins, input$name_h2h, "</h2>" ))
  })
}

shinyApp(ui = ui, server = server)
