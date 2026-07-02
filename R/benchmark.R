load("data/all_parkruns.RDa")

library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(stringr)
library(stringi)
all_results_test <- tribble(
  ~"event" , ~"event_no" , ~"pos" , ~"parkrunner" , ~"id" , ~"time" , ~"short"
)
folder <- "data/results/"
ls <- list.files("data/results", full.names = T)
for (i in ls) {
  all_results_test |>
    rbind.data.frame(read.csv(i)) -> all_results_test
}

rbenchmark::benchmark(
  "as_is" = {
    all_results <- tribble(
      ~"name" , ~"event" , ~"event_no" , ~"pos" , ~"parkrunner" , ~"time" , ~"short"
    )
    folder <- "data/results/"
    for (j in "alexbuckley") {
      #cat(crayon::blue(paste(j, "\n")))
      for (i in 1:nrow(all_parkruns[[j]][["results"]])) {
        event <- all_parkruns[[j]][["results"]][["event"]][i]
        short <- all_parkruns[[j]][["results"]][["short"]][i]
        event_no <- all_parkruns[[j]][["results"]][["event_no"]][i]
        file <- paste0(folder, event, event_no, ".csv")
        if (file.exists(file)) {
          x <- read.csv(file) |>
            mutate(
              name = all_parkruns[[j]][["name"]],
              event = all_parkruns[[j]][["results"]][["event"]][i],
              event_no = all_parkruns[[j]][["results"]][["event_no"]][i],
              short = all_parkruns[[j]][["results"]][["short"]][i]
            )
        }
        all_results <- all_results |> rbind.data.frame(x)
      }
    }
  },
  "append" = {
    folder <- "data/results/"
    x <- tribble(
      ~"name" , ~"event" , ~"event_no" , ~"pos" , ~"parkrunner" , ~"time" , ~"short"
    )

    x |>
      write.csv("data/all_results.csv", row.names = F)

    for (j in "alexbuckley") {
      #cat(crayon::blue(paste(j, "\n")))
      for (i in 1:nrow(all_parkruns[[j]][["results"]])) {
        event <- all_parkruns[[j]][["results"]][["event"]][i]
        short <- all_parkruns[[j]][["results"]][["short"]][i]
        event_no <- all_parkruns[[j]][["results"]][["event_no"]][i]
        file <- paste0(folder, event, event_no, ".csv")
        if (file.exists(file)) {
          read.csv(file) |>
            mutate(
              name = all_parkruns[[j]][["name"]],
              event = all_parkruns[[j]][["results"]][["event"]][i],
              event_no = all_parkruns[[j]][["results"]][["event_no"]][i],
              short = all_parkruns[[j]][["results"]][["short"]][i]
            ) |>
            select(all_of(names(x))) |>
            write.table(
              "data/all_results.csv",
              row.names = F,
              col.names = F,
              sep = ",",
              append = TRUE
            )
        }
      }
    }
    all_results <- read.csv("data/all_results.csv")
  },
  replications = 10
)
