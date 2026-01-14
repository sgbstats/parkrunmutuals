library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(stringr)

source("R/get_results.R")
get_all_results = function(
  parkrunner,
  folder = "data/results/",
  skip_errors = T,
  log_file = "error_log.txt",
  ...
) {
  if (class(parkrunner) != "parkrun_results") {
    break
  }
  results = parkrunner[["results"]]
  for (i in 1:nrow(results)) {
    event = results$event[i]
    eventno = results$run_number[i]
    url = results$url[i]
    errors = read.csv(log_file, header = T)

    if (event %in% errors$event && skip_errors) {
      next
    }

    file = paste0(folder, event, eventno, ".csv")

    if (file.exists(file)) {
      next
    }

    cat(paste(event, eventno, "\n"))
    tryCatch(
      {
        x = get_results(url = url)[["results"]]

        write.csv(x, file, row.names = F)
        Sys.sleep(25)
      },
      error = function(e) {
        message("Error: ", conditionMessage(e))
        write(paste(event, eventno, sep = ","), file = log_file, append = TRUE)
      },
      warning = function(e) {
        message(conditionMessage(e))
      }
    )
  }
  return(0)
}


load("C:/R/git/parkrunmutuals/data/all_parkruns.RDa")

for (j in names(all_parkruns)) {
  cat(crayon::blue(paste(j, "\n")))
  get_all_results(all_parkruns[[j]])
}
ls = list.files("data/results", full.names = T)
ls = ls[grepl(".csv", ls)]
for (i in ls) {
  read.csv(i) |>
    dplyr::select(pos, parkrunner, time, id) |>
    write.csv(i, row.names = F)
}
