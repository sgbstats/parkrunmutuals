library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(stringr)
library(stringi)

# pak::pak("sgbstats/parkrunfunctions")
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
all_parkruns[["names_ids"]] <- map_dfr(
  all_parkruns,
  ~ tibble(
    name = .x[["name"]],
    id = .x[["id"]]
  )
)

save(all_parkruns, file = "data/all_parkruns.RDa")
load("data/all_parkruns.RDa")


folder = "data/results/"
combined_df <- purrr::map_df(all_parkruns, ~ .x[["results"]]) |>
  dplyr::select(event, event_no, url) |>
  dplyr::distinct() |>
  arrange(event, event_no) |>
  mutate(file = paste0(folder, event, event_no, ".csv"))

ls = list.files("data/results", full.names = T)

filtered_df <- combined_df |>
  filter(!file %in% ls) #|>
#filter(grepl("[O-Z]", substr(event, 1, 1))) # to split the work

skip_errors = T
log_file = "error_log.txt"

for (i in 1:nrow(filtered_df)) {
  errors = read.csv(log_file, header = T)
  if (filtered_df$event[i] %in% errors$name && skip_errors) {
    next
  }
  cat(paste(filtered_df$event[i], filtered_df$event_no[i], "\n"))
  tryCatch(
    {
      get_result(url = filtered_df$url[i])[["results"]] |>
        write.csv(filtered_df$file[i], row.names = F)
      Sys.sleep(30)
    },
    error = function(e) {
      message(conditionMessage(e))
      write(
        paste(filtered_df$event[i], filtered_df$event_no[i], sep = ","),
        file = log_file,
        append = TRUE
      )
    },
    warning = function(e) {
      message(conditionMessage(e))
    }
  )
}

all_results = tribble(
  ~"name" , ~"event" , ~"event_no" , ~"pos" , ~"parkrunner" , ~"time" , ~"short"
)
folder = "data/results/"
for (j in (names(all_parkruns)[names(all_parkruns) != "names_ids"])) {
  cat(crayon::blue(paste(j, "\n")))
  for (i in 1:nrow(all_parkruns[[j]][["results"]])) {
    event = all_parkruns[[j]][["results"]][["event"]][i]
    short = all_parkruns[[j]][["results"]][["short"]][i]
    event_no = all_parkruns[[j]][["results"]][["event_no"]][i]
    file = paste0(folder, event, event_no, ".csv")
    if (file.exists(file)) {
      x = read.csv(file) |>
        mutate(
          name = all_parkruns[[j]][["name"]],
          event = all_parkruns[[j]][["results"]][["event"]][i],
          short = all_parkruns[[j]][["results"]][["short"]][i],
          event_no = all_parkruns[[j]][["results"]][["event_no"]][i]
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

names_ids = all_parkruns[["names_ids"]]
names_all = all_results |>
  count(name, parkrunner, id)
save(
  all_results,
  runners,
  parkruns,
  date,
  events_done,
  names_ids,
  names_all,
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
