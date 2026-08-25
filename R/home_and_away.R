library(tidyverse)
library(parkrunfunctions)
library(lubridate)

# One-time study of where runners from GM home parkruns go when their home event is cancelled.
# Any loop that calls parkrunfunctions must sleep for 23 seconds after each call.

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

load_data <- function(path) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  as.list(env)
}

cache_root <- file.path("data", "home_and_away", "cache")
event_cache_dir <- file.path(cache_root, "events")
runner_cache_dir <- file.path(cache_root, "runners")
dir.create(event_cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(runner_cache_dir, recursive = TRUE, showWarnings = FALSE)

get_event_history_safe <- function(event_slug) {
  message("Event history: ", event_slug)
  res <- tryCatch(
    parkrunfunctions::get_event_history(event = event_slug),
    error = function(e) NULL
  )
  Sys.sleep(23)
  res
}

get_event_results_safe <- function(event_slug, event_no) {
  message("  Results: ", event_slug, " #", event_no)
  res <- tryCatch(
    parkrunfunctions::get_result(
      event = event_slug,
      event_no = event_no,
      as_Date = TRUE
    ),
    error = function(e) NULL
  )
  Sys.sleep(23)
  res
}

runner_cache_file <- function(runner_id) {
  file.path(runner_cache_dir, paste0(runner_id, ".rds"))
}

event_cache_file <- function(event_slug) {
  file.path(event_cache_dir, paste0(event_slug, ".rds"))
}

get_runner_runs_safe <- function(runner_id) {
  cache_file <- runner_cache_file(runner_id)
  if (file.exists(cache_file)) {
    message("Runner: ", runner_id, " [cached]")
    return(readRDS(cache_file))
  }

  message("Runner: ", runner_id)
  res <- tryCatch(
    parkrunfunctions::get_all_runs(id = runner_id, as_Date = TRUE),
    error = function(e) NULL
  )
  Sys.sleep(23)

  if (is.null(res)) {
    return(NULL)
  }

  out <- res$results |>
    dplyr::select(event_date, event, short) |>
    mutate(runner_id = runner_id)

  saveRDS(out, cache_file)
  out
}

load_event_checkpoint <- function(path) {
  if (!file.exists(path)) {
    return(list(detail = tibble(), next_j = 1L, finished = FALSE))
  }

  x <- readRDS(path)
  if (is.list(x) && !is.null(x$detail) && !is.null(x$next_j)) {
    return(x)
  }

  list(detail = x, next_j = 1L, finished = TRUE)
}

save_event_checkpoint <- function(path, detail, next_j, finished = FALSE) {
  saveRDS(
    list(detail = detail, next_j = next_j, finished = finished),
    path
  )
  invisible(path)
}

study_home_event <- function(home_parkrun, event_slug, cutoff_date) {
  cache_file <- event_cache_file(event_slug)
  checkpoint <- load_event_checkpoint(cache_file)

  if (isTRUE(checkpoint$finished)) {
    message("Event: ", home_parkrun, " [cached]")
    return(checkpoint$detail)
  }

  if (checkpoint$next_j > 1L) {
    message("Event: ", home_parkrun, " [resume gap ", checkpoint$next_j, "]")
  } else {
    message("Event: ", home_parkrun)
  }

  history_obj <- get_event_history_safe(event_slug)
  if (is.null(history_obj)) {
    save_event_checkpoint(
      cache_file,
      checkpoint$detail,
      checkpoint$next_j,
      finished = FALSE
    )
    return(checkpoint$detail)
  }

  history <- history_obj$history |>
    arrange(date) |>
    filter(date >= cutoff_date)

  if (nrow(history) < 2) {
    save_event_checkpoint(
      cache_file,
      checkpoint$detail,
      checkpoint$next_j,
      finished = TRUE
    )
    return(checkpoint$detail)
  }

  detail <- checkpoint$detail
  start_j <- max(1L, checkpoint$next_j)

  for (j in seq.int(start_j, nrow(history) - 1L)) {
    prev_row <- history[j, ]
    next_row <- history[j + 1L, ]
    gap_days <- as.integer(next_row$date - prev_row$date)

    if (gap_days <= 7 || gap_days %% 7 != 0) {
      save_event_checkpoint(cache_file, detail, j + 1L, finished = FALSE)
      next
    }

    prev_results_obj <- get_event_results_safe(event_slug, prev_row$event_no)
    next_results_obj <- get_event_results_safe(event_slug, next_row$event_no)

    if (is.null(prev_results_obj) || is.null(next_results_obj)) {
      save_event_checkpoint(cache_file, detail, j + 1L, finished = FALSE)
      next
    }

    prev_results <- prev_results_obj$results |>
      dplyr::select(id, parkrunner) |>
      filter(!is.na(id))
    next_results <- next_results_obj$results |>
      dplyr::select(id, parkrunner) |>
      filter(!is.na(id))

    common_ids <- intersect(prev_results$id, next_results$id)
    if (length(common_ids) == 0) {
      save_event_checkpoint(cache_file, detail, j + 1L, finished = FALSE)
      next
    }

    runner_names <- prev_results |>
      filter(id %in% common_ids) |>
      distinct(id, parkrunner)

    cancellation_dates <- seq(
      prev_row$date + 7,
      next_row$date - 7,
      by = "7 days"
    )
    if (length(cancellation_dates) == 0) {
      save_event_checkpoint(cache_file, detail, j + 1L, finished = FALSE)
      next
    }

    gap_rows <- tidyr::crossing(
      tibble(
        home_parkrun = home_parkrun,
        home_event = event_slug,
        prev_event_no = prev_row$event_no,
        prev_date = prev_row$date,
        next_event_no = next_row$event_no,
        next_date = next_row$date
      ),
      cancellation_date = cancellation_dates,
      runner_id = common_ids
    ) |>
      left_join(runner_names, by = c("runner_id" = "id"))

    detail <- bind_rows(detail, gap_rows)
    save_event_checkpoint(cache_file, detail, j + 1L, finished = FALSE)
  }

  save_event_checkpoint(cache_file, detail, nrow(history), finished = TRUE)
  detail
}

resolve_gap_week <- function(detail) {
  if (nrow(detail) == 0) {
    return(detail)
  }

  runner_ids <- sort(unique(detail$runner_id))
  runner_runs <- list()

  for (runner_id in runner_ids) {
    cache_file <- runner_cache_file(runner_id)
    if (file.exists(cache_file)) {
      message("Runner: ", runner_id, " [cached]")
      runner_runs[[as.character(runner_id)]] <- readRDS(cache_file)
      next
    }

    runner_runs[[as.character(runner_id)]] <- get_runner_runs_safe(runner_id)
  }

  runner_runs <- bind_rows(runner_runs)

  detail |>
    left_join(
      runner_runs |>
        rename(
          cancellation_date = event_date,
          gap_week_parkrun = event,
          gap_week_short = short
        ),
      by = c("runner_id", "cancellation_date")
    ) |>
    mutate(gap_week_parkrun = coalesce(gap_week_parkrun, "Did not run"))
}

main <- function() {
  distances_path <- file.path("parkrunmutuals", "distances.RDa")
  if (!file.exists(distances_path)) {
    stop("Missing parkrunmutuals/distances.RDa")
  }

  parkruns_list <- load_data(distances_path)$parkruns_list
  if (is.null(parkruns_list)) {
    stop("parkruns_list object not found")
  }

  cutoff_date <- Sys.Date() - years(2)

  gm_meta <- tibble(home_parkrun = gm) |>
    mutate(
      event_slug = parkruns_list$name[match(home_parkrun, parkruns_list$short)]
    ) |>
    filter(!is.na(event_slug))

  event_details <- purrr::map_dfr(
    seq_len(nrow(gm_meta)),
    function(i) {
      study_home_event(
        home_parkrun = gm_meta$home_parkrun[i],
        event_slug = gm_meta$event_slug[i],
        cutoff_date = cutoff_date
      )
    }
  )

  detail <- resolve_gap_week(event_details) |>
    select(-parkrunner)

  summary <- detail |>
    count(home_parkrun, gap_week_parkrun, name = "runners") |>
    arrange(home_parkrun, desc(runners), gap_week_parkrun)

  out_dir <- file.path("data", "home_and_away")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  write_csv(detail, file.path(out_dir, "gm_home_and_away_detail.csv"))
  write_csv(summary, file.path(out_dir, "gm_home_and_away_summary.csv"))
  saveRDS(detail, file.path(out_dir, "gm_home_and_away_detail.rds"))
  saveRDS(summary, file.path(out_dir, "gm_home_and_away_summary.rds"))

  invisible(list(detail = detail, summary = summary))
}

if (sys.nframe() == 0) {
  main()
}
