library(shiny)
library(shinyjs)
library(tidyverse)
library(shinyWidgets)
library(DT)

options(gargle_oauth_cache = ".secrets", gargle_oauth_email = TRUE)
googledrive::drive_auth(
  cache = ".secrets",
  email = "sebastiangbate@gmail.com"
)
googledrive::drive_download(
  googledrive::as_id("11d6wDY_ryjx5sxv5sVAS1x_wbquVQqLY"),
  path = "all_results.RDa",
  overwrite = T
)

source("bubbles.R")

load("all_results.RDa")
load("distances.RDa")
prs_short = parkruns_list$name
names(prs_short) = parkruns_list$short
# names_all = all_results |> count(id, parkrunner)

all_results2 = names_all |>
  filter(n >= 3) |>
  arrange(name, -n) |>
  filter(name != parkrunner)

names_all2 = all_results2 |>
  summarise(n = sum(n), .by = c(id, parkrunner))

ui <- navbarPage(
  "parkrun mutuals",
  tabPanel(
    "Aggregate",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "group",
          "",
          c(
            "ADAPAT" = "adapat",
            "WNR" = "wnr",
            "Bate Famliy" = "bate",
            "Others" = "others"
          ),
          selected = "adapat",
          inline = TRUE
        ),
        pickerInput("name", "parkrunner", choices = NULL, selected = NULL),
        radioButtons(
          "eventsruns",
          "Events or Runs",
          c("Events" = "events", "Runs" = "runs"),
          selected = "events",
          inline = TRUE
        ),
        pickerInput(
          "parkrun_name",
          "parkruns",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        switchInput("exclude_gm", "Exclude GM", value = FALSE, width = "100%"),
        numericInput("min", "Minimum", value = 3),
        uiOutput("update_time")
      ),
      mainPanel(
        tags$head(tags$script(
          '$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            '
        )),
        plotOutput("main")
      )
    )
  ),
  tabPanel(
    "Head-to-Head",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "group2",
          "",
          c(
            "ADAPAT" = "adapat",
            "WNR" = "wnr",
            "Bate Famliy" = "bate",
            "Others" = "others"
          ),
          selected = "adapat",
          inline = TRUE
        ),
        pickerInput("name2", "parkrunner", choices = NULL, selected = NULL),
        selectizeInput(
          "name_h2h",
          "Head-to-head",
          choices = as.character(0),
          selected = NULL,
          options = list(
            placeholder = "Start typing…",
            create = FALSE
          )
        ),
        radioButtons(
          "timeage",
          "Time or Age Grade",
          c("Time" = "time", "Age Grade" = "ag"),
          selected = "time",
          inline = TRUE
        ),
        radioButtons(
          "filter_wins",
          "",
          c("All Results" = "all", "Your Wins" = "y", "Rival Wins" = "r"),
          selected = "all",
          inline = T
        ),
        pickerInput(
          "parkrun_name2",
          "parkruns",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        switchInput(
          "exclude_all",
          "Exclude ALL",
          value = FALSE,
          width = "100%"
        ),
        HTML("Minimum of 3 parkruns")
      ),
      mainPanel(
        htmlOutput("text"),
        dataTableOutput("main2")
      )
    )
  ),
  tabPanel(
    "Mutual NENDY",
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          "name3",
          "parkrunner",
          choices = c(
            "Adam BURNETT",
            "Alex BUCKLEY",
            "Andrew CARLSON",
            "Charlotte TURNER",
            "Frankie BALE",
            "Jonathan O'DONNELL",
            "Joseph GUNTRIP",
            "Luke DONALD",
            "Max LETCHFIELD",
            "Michael PETER",
            "Natalie HARPER",
            "Philip MOYLE",
            "Rachel BROWN",
            "Rob MOONEY",
            "Sebastian BATE",
            "Suzy HILL",
            "Tom ALMOND",
            "Catherine BATE",
            "Ewan BATE",
            "Lawrence BATE",
            "Alexandra BERESFORD",
            "Callum SHINGLER",
            "Colm MULHERN",
            "Kirsty WATKINSON",
            "Gary SCOTT",
            "Helen ANDREWS",
            "Isabel PRECIOUS-BIRDS",
            "Jon SHAW",
            "Lindsay HASTON",
            "Lynda CLIFFORD",
            "Paul CLIFFORD",
            "Paul Thomas MULDOON",
            "Bob BAYMAN",
            "Brita BAYMAN",
            "Helena ROBINSON",
            "Lawrence BATE",
            "Niamh CONROY VAN LEEUWEN"
          ),
          selected = NULL,
          multiple = T
        ),
        # pickerInput("home", "Home location", choices = prs_short, selected = "southmanchester")
        selectizeInput(
          "home",
          "Home location",
          choices = prs_short,
          selected = "southmanchester",
          options = list(
            placeholder = "Start typing…",
            create = FALSE
          )
        )
      ),
      mainPanel(
        dataTableOutput("mnendy")
      )
    )
  )
)


server <- function(input, output, session) {
  output$update_time = renderUI({
    HTML(paste0("Last update: ", format(date, format = "%Y-%m-%d")))
  })

  #panel 1
  group_names <- reactive({
    switch(
      input$group,
      "adapat" = c(
        "Adam BURNETT",
        "Alex BUCKLEY",
        "Andrew CARLSON",
        "Charlotte TURNER",
        "Frankie BALE",
        "Jonathan O'DONNELL",
        "Joseph GUNTRIP",
        "Luke DONALD",
        "Max LETCHFIELD",
        "Michael PETER",
        "Natalie HARPER",
        "Philip MOYLE",
        "Rachel BROWN",
        "Rob MOONEY",
        "Sebastian BATE",
        "Suzy HILL",
        "Tom ALMOND"
      ),
      "bate" = c(
        "Catherine BATE",
        "Ewan BATE",
        "Lawrence BATE",
        "Sebastian BATE"
      ),
      "wnr" = c(
        "Alexandra BERESFORD",
        "Callum SHINGLER",
        "Colm MULHERN",
        "Kirsty WATKINSON",
        "Gary SCOTT",
        "Helen ANDREWS",
        "Isabel PRECIOUS-BIRDS",
        "Jon SHAW",
        "Lindsay HASTON",
        "Lynda CLIFFORD",
        "Paul CLIFFORD",
        "Paul Thomas MULDOON",
        "Sebastian Bate"
      ),
      "others" = c(
        "Bob BAYMAN",
        "Brita BAYMAN",
        "Helena ROBINSON",
        "Lawrence BATE",
        "Niamh CONROY VAN LEEUWEN"
      )
    )
  })

  observeEvent(group_names(), {
    updatePickerInput(session, "name", choices = group_names())
  })

  parkruns_for_name <- reactive({
    req(input$name)
    sort(unique((all_results |> filter(name == input$name))$event))
  })

  observeEvent(parkruns_for_name(), {
    updatePickerInput(
      session,
      "parkrun_name",
      choices = parkruns_for_name(),
      selected = parkruns_for_name()
    )
  })

  gm <- c(
    "Alexandra",
    "Bolton",
    "Bramhall Park",
    "Burnage",
    "Chadderton Hall",
    "Cheadle Hulme",
    "Clarence",
    "Fletcher Moss",
    "Haigh Woodland",
    "Heaton",
    "Hyde",
    "Marple",
    "Oldham",
    "Peel",
    "Pennington Flash",
    "Philips Park",
    "Sale Water",
    "South Manchester",
    "Stamford Park",
    "Stretford",
    "Watergrove",
    "Woodbank",
    "Wythenshawe",
    "Worsley Woods"
  )

  observe({
    current_parks <- input$parkrun_name

    if (input$exclude_gm) {
      prs <- setdiff(current_parks, gm[gm %in% current_parks])
    } else {
      prs <- union(current_parks, gm[!gm %in% current_parks])
    }

    updatePickerInput(session, "parkrun_name", selected = prs)
  })

  observeEvent(input$eventsruns, {
    min_val <- switch(input$eventsruns, "events" = 3, "runs" = 8)
    updateNumericInput(session, "min", value = min_val)
  })

  output$main = renderPlot(
    {
      req(
        input$name,
        input$parkrun_name,
        input$eventsruns,
        input$group,
        input$min
      )
      bubble(
        var = input$eventsruns,
        name_in = input$name,
        prs = input$parkrun_name,
        others = group_names(),
        min = input$min,
        data_in = all_results,
        ids = names_ids
      )
    },
    height = reactive(ifelse(
      !is.null(input$innerWidth),
      input$innerWidth * 2 / 5,
      0
    ))
  )

  #panel 2

  #flow select names from the group
  #pick the rivals that have done the min prs
  #only display mutual prs
  #
  #
  group_names2 <- reactive({
    switch(
      input$group2,
      "adapat" = c(
        "Adam BURNETT",
        "Alex BUCKLEY",
        "Andrew CARLSON",
        "Charlotte TURNER",
        "Frankie BALE",
        "Jonathan O'DONNELL",
        "Joseph GUNTRIP",
        "Luke DONALD",
        "Max LETCHFIELD",
        "Michael PETER",
        "Natalie HARPER",
        "Philip MOYLE",
        "Rachel BROWN",
        "Rob MOONEY",
        "Sebastian BATE",
        "Suzy HILL",
        "Tom ALMOND"
      ),
      "bate" = c(
        "Catherine BATE",
        "Ewan BATE",
        "Lawrence BATE",
        "Sebastian BATE"
      ),
      "wnr" = c(
        "Alexandra BERESFORD",
        "Callum SHINGLER",
        "Colm MULHERN",
        "Kirsty WATKINSON",
        "Gary SCOTT",
        "Helen ANDREWS",
        "Isabel PRECIOUS-BIRDS",
        "Jon SHAW",
        "Lindsay HASTON",
        "Lynda CLIFFORD",
        "Paul CLIFFORD",
        "Paul Thomas MULDOON",
        "Sebastian Bate"
      ),
      "others" = c(
        "Bob BAYMAN",
        "Brita BAYMAN",
        "Helena ROBINSON",
        "Lawrence BATE",
        "Niamh CONROY VAN LEEUWEN"
      )
    )
  })

  observeEvent(group_names2(), {
    updatePickerInput(session, "name2", choices = group_names2())
  })

  get_rivals <- reactive({
    req(input$name2)
    unique((all_results2 |> filter(name == input$name2))$parkrunner)
  })

  observeEvent(get_rivals(), {
    updatePickerInput(
      session,
      "name_h2h",
      choices = get_rivals(),
      selected = get_rivals()
    )
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
    sort(unique(
      (all_results |>
        filter(name == input$name2, parkrunner == input$name_h2h))$event
    ))
  })

  observeEvent(mutual_parkruns(), {
    updatePickerInput(
      session,
      "parkrun_name2",
      choices = mutual_parkruns(),
      selected = mutual_parkruns()
    )
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
    req(input$timeage)

    h2h = all_results |>
      filter(
        name == input$name2,
        id %in%
          c(
            recode_values(
              input$name_h2h,
              from = names_all2$parkrunner,
              to = names_all2$id
            ),
            recode_values(
              input$name2,
              from = names_all2$parkrunner,
              to = names_all2$id
            )
          ),
        event %in% input$parkrun_name2
      ) |>
      dplyr::select(-name) |>
      mutate(
        rank_time = 2 - rank(pos),
        rank_ag = 2 - rank(-ag),
        .by = c("event", "event_no")
      )

    if (input$timeage == "time") {
      head_to_head = h2h |> rename(rank = rank_time) |> select(-rank_ag)
    } else {
      head_to_head = h2h |> rename(rank = rank_ag) |> select(-rank_time)
    }
    head_to_head
  })

  output$main2 = renderDataTable(
    {
      req(input$filter_wins)

      x = head_to_head() |>
        dplyr::select(-rank, -pos, -id) |>
        pivot_wider(names_from = parkrunner, values_from = c("time", "ag")) |>
        na.omit() |>
        dplyr::select(
          event,
          event_no,
          any_of(paste(
            input$timeage,
            c(input$name2, input$name_h2h),
            sep = "_"
          ))
        )

      x1 = x |>
        merge(
          head_to_head() |>
            filter(parkrunner == input$name_h2h) |>
            dplyr::select(event, event_no, rank),
          sort = FALSE
        ) |>
        rename(
          "Event" = event,
          "Number" = event_no
        ) |>
        rename_with(
          ~ gsub(paste0(input$timeage, "_"), "", .x),
          .cols = starts_with(input$timeage)
        )

      if (input$filter_wins == "all") {
        x2 = x1 |> dplyr::select(-rank)
      } else if (input$filter_wins == "y") {
        x2 = x1 |> filter(rank == 0) |> dplyr::select(-rank)
      } else if (input$filter_wins == "r") {
        x2 = x1 |> filter(rank == 1) |> dplyr::select(-rank)
      }

      x2
    },
    options = list(
      autoWidth = TRUE,
      # scrollX=T,
      pageLength = 20
    )
  )

  output$text = renderUI({
    x = head_to_head() |>
      dplyr::select(-pos, -time, -id, -ag) |>
      pivot_wider(names_from = parkrunner, values_from = rank) |>
      na.omit() |>
      pivot_longer(
        cols = c(input$name2, input$name_h2h),
        names_to = "parkrunner",
        values_to = "rank"
      ) |>
      summarise(wins = sum(rank), .by = "parkrunner") |>
      pivot_wider(names_from = parkrunner, values_from = wins) |>
      select(any_of(c(input$name2, input$name_h2h)))

    leftwins = x[1, 1]
    rightwins = x[1, 2]

    HTML(paste(
      "<h2>",
      input$name2,
      leftwins,
      "-",
      rightwins,
      input$name_h2h,
      "</h2>"
    ))
  })

  output$mnendy = renderDataTable(
    {
      events_done2 = events_done |>
        filter(name %in% input$name3) |>
        pull(event)

      x = distance |>
        filter(name.x == input$home) |>
        filter(!short %in% events_done2) |>
        arrange(miles) |>
        mutate(miles = sprintf("%.0f", miles)) |>
        dplyr::select("Name" = short, "Distance (mi)" = miles)

      x
    },
    options = list(
      autoWidth = TRUE,
      # scrollX=T,
      pageLength = 20
    )
  )
}

shinyApp(ui = ui, server = server)
