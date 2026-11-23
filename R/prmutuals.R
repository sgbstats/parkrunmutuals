library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(stringr)
library(stringi)

# pak::pak("sgbstats/parkrunfunctions")
library(parkrunfunctions)
source("R/supabase-cache.R")
source("R/distance.R")

ids <- tribble(
  ~id        ,
  '7232608'  , # Adam
  '4087050'  , # Alex
  '7402459'  , # Andy
  '7072913'  , # Nat
  '4458732'  , # Frankie
  '593256'   , # Jonny
  '2407768'  , # Luke
  '582473'   , # Max
  '7266141'  , # Phil
  '1539187'  , # Rachel
  '5349926'  , # Rob
  '493595'   , # Seb
  '7756087'  , # Suzy
  '5243446'  , # Tom A
  '2548951'  , #charlotte
  '5107378'  , #mike
  '7073694'  , #jgunz
  #bate
  '147910'   , #mum
  '144486'   , #ewan
  '140961'   , #dad
  #wnr
  "42804"    , #paul M
  "9334474"  , #Alex beresford
  "6391679"  , #kirsty
  "7433025"  , #Helen
  "4301378"  , #Lindsay
  "4051781"  , #Callum
  "16568"    , #Paul C
  "16569"    , #Lynda
  "3629365"  , #Isabel
  "888332"   , #Colm
  "4228499"  , #sarah
  "10000894" , #andrew
  "1166640"  , #gary
  "81779"    , #jon
  "8326421"  , #steve
  "5301549"  , #bob
  "3965294"  , #helena
  "3590867"  , #niamh
  "5401482"  , #britt
)

publish_prmutuals <- function(
  runner_ids = ids$id,
  today = Sys.Date(),
  config = supabase_storage_config(),
  history_fetch = parkrunfunctions::get_all_runs,
  fetch = parkrunfunctions::get_result,
  distance_builder = build_distance_data,
  sleep = Sys.sleep
) {
  all_parkruns <- list()
  failed_ids <- character()
  for (i in runner_ids) {
    hold <- tryCatch(
      cache_fetch_rds(
        sprintf("data/runner-histories/%s/%s.rds", i, today),
        function() {
          runner <- history_fetch(i)
          if (
            length(runner$name) != 1L || is.na(runner$name) ||
              !nzchar(trimws(runner$name)) || !is.data.frame(runner$results) ||
              !all(c("event", "event_no", "url", "short") %in% names(runner$results)) ||
              length(runner$id) != 1L || is.na(runner$id) ||
              as.character(runner$id) != as.character(i)
          ) {
            stop("Runner history has no valid name, ID or results")
          }
          runner
        },
        config = config,
        sleep = sleep
      ),
      error = function(e) {
        warning(sprintf("Could not fetch runner %s: %s", i, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(hold)) {
      failed_ids <- c(failed_ids, i)
      next
    }
    key <- str_to_lower(str_remove_all(hold$name, "\\s"))
    if (key %in% names(all_parkruns)) {
      warning(sprintf("Skipping runner %s: duplicate name key %s", i, key))
      failed_ids <- c(failed_ids, i)
      next
    }
    message(hold$name)
    all_parkruns[[key]] <- hold
  }
  if (length(failed_ids)) {
    stop(
      "Runner histories could not be loaded for IDs: ",
      paste(failed_ids, collapse = ", "),
      ". Existing Supabase runner histories were not overwritten; retry later."
    )
  }
  all_parkruns[["names_ids"]] <- map_dfr(
    all_parkruns,
    ~ tibble(name = .x[["name"]], id = .x[["id"]])
  )

  folder <- "data/results/"
  combined_df <- purrr::map_df(all_parkruns, ~ .x[["results"]]) |>
    dplyr::select(event, event_no, url) |>
    dplyr::distinct() |>
    arrange(event, event_no) |>
    mutate(file = paste0(folder, event, event_no, ".csv"))
  if (!nrow(combined_df)) {
    stop("No runner results available; the published bundle was not replaced")
  }
  cache_fetch_results(
    combined_df$file, combined_df$url, fetch = fetch, config = config, sleep = sleep
  )
  result_frames <- setNames(
    lapply(combined_df$file, function(path) {
      cache_read(
        path,
        function(file) readr::read_csv(
          file, show_col_types = FALSE,
          col_types = readr::cols(pos = readr::col_integer(), id = readr::col_integer())
        ),
        config
      )
    }),
    combined_df$file
  )
  names_list <- names(all_parkruns)[names(all_parkruns) != "names_ids"]
  all_results_list <- map(names_list, function(j) {
    res <- all_parkruns[[j]][["results"]]
    if (nrow(res) == 0L) {
      return(tibble())
    }
    files <- paste0(folder, res$event, res$event_no, ".csv")
    map2_dfr(
      files, seq_len(nrow(res)),
      function(file, idx) {
        result_frames[[file]] |>
          mutate(
            name = all_parkruns[[j]][["name"]],
            event = res$event[idx],
            short = res$short[idx],
            event_no = res$event_no[idx]
          )
      }
    )
  })
  all_results <- bind_rows(all_results_list)
  runners <- unique(all_results$name)
  parkruns <- sort(unique(all_results$event))
  events_done <- all_results |> count(name, event) |> dplyr::select(-n)
  date <- today
  names_ids <- all_parkruns[["names_ids"]]
  names_all <- all_results |> count(name, parkrunner, id)
  distance_data <- distance_builder()
  distance <- distance_data$distance
  parkruns_list <- distance_data$parkruns_list

  bundle_path <- tempfile(fileext = ".RDa")
  on.exit(unlink(bundle_path), add = TRUE)
  save(
    all_results, runners, parkruns, date, events_done, names_ids, names_all,
    distance, parkruns_list, file = bundle_path
  )
  load_parkrunmutuals_bundle(bundle_path)
  cache_save(all_parkruns, file = "data/all_parkruns.RDa", config = config)
  supabase_upload_object(bundle_path, config = config)
  verify_parkrunmutuals_bundle(config = config)
  message("Supabase publication completed successfully.")
  invisible(TRUE)

}

if (sys.nframe() == 0L) publish_prmutuals()
