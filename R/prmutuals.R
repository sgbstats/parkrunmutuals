library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(stringr)
library(stringi)

#pak::pak("sgbstats/parkrunfunctions")
library(parkrunfunctions)

ids = tribble(
  ~id       ,
  '7232608' , # Adam
  '4087050' , # Alex
  '7402459' , # Andy
  '7072913' , # Nat
  '4458732' , # Frankie
  '593256'  , # Jonny
  '2407768' , # Luke
  '582473'  , # Max
  '7266141' , # Phil
  '1539187' , # Rachel
  '5349926' , # Rob
  '493595'  , # Seb
  '7756087' , # Suzy
  '5243446' , # Tom A
  '2548951' , #charlotte
  '5107378' , #mike
  '7073694' , #jgunz
  #bate
  '147910'  , #mum
  '144486'  , #ewan
  '140961'  , #dad

  #wnr

  "42804"   , #paul M
  "9334474" , #Alex beresford
  "6391679" , #kirsty
  "7433025" , #Helen
  "4301378" , #Lindsay
  "4051781" , #Callum
  "16568"   , #Paul C
  "16569"   , #Lynda
  "3629365" , #Isabel
  "888332"  , #Colm
  "1166640" , #gary
  "81779"   , #jon

  "5301549" , #bob
  "3965294" , #helena
  "3590867" , #niamh
  "5401482" , #britt
)

all_parkruns = list()
for (i in ids$id) {
  hold = parkrunfunctions::get_all_runs(i)
  cat(paste(hold$name, "\n"))
  all_parkruns[[str_to_lower(str_remove_all(hold$name, "\\s"))]] = hold
  Sys.sleep(25)
}


save(all_parkruns, file = "data/all_parkruns.RDa")


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
        x = get_result(url = url)[["results"]]

        write.csv(x, file, row.names = F)
        Sys.sleep(25)
      },
      error = function(e) {
        message(conditionMessage(e))
        write(paste(event, eventno, sep = ","), file = log_file, append = TRUE)
      },
      warning = function(e) {
        message(conditionMessage(e))
      }
    )
  }
  return(0)
}


load("data/all_parkruns.RDa")

for (j in names(all_parkruns)) {
  cat(crayon::blue(paste(j, "\n")))
  get_all_results(all_parkruns[[j]])
}
ls = list.files("data/results", full.names = T)
for (i in ls) {
  read.csv(i) |>
    dplyr::select(pos, parkrunner, time) |>
    write.csv(i, row.names = F)
}


all_results = tribble(
  ~"name" , ~"event" , ~"eventno" , ~"pos" , ~"parkrunner" , ~"time" , ~"short"
)
folder = "data/results/"
for (j in names(all_parkruns)) {
  cat(crayon::blue(paste(j, "\n")))
  for (i in 1:nrow(all_parkruns[[j]][["results"]])) {
    event = all_parkruns[[j]][["results"]][["event"]][i]
    short = all_parkruns[[j]][["results"]][["short"]][i]
    eventno = all_parkruns[[j]][["results"]][["run_number"]][i]
    file = paste0(folder, event, eventno, ".csv")
    if (file.exists(file)) {
      x = read.csv(file) |>
        mutate(
          name = all_parkruns[[j]][["name"]],
          event = event,
          eventno = eventno,
          #  time = as.numeric(hms::as.hms(time))
        )
    }
    all_results = all_results |> rbind.data.frame(x)
  }
}

runners = unique(all_results$name)
parkruns <- sort(unique(all_results$event))
events_done = all_results |> count(name, event) |> dplyr::select(-n)
date = Sys.Date()
save(
  all_results,
  runners,
  parkruns,
  date,
  events_done,
  file = "data/all_results.RDa"
)

googledrive::drive_auth(
  email = TRUE,
  path = NULL,
  subject = NULL,
  scopes = "drive",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

googledrive::drive_update(
  media = "data/all_results.RDa",
  file = googledrive::as_id("11d6wDY_ryjx5sxv5sVAS1x_wbquVQqLY"),
)
