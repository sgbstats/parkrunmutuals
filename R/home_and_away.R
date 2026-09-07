library(tidyverse)
library(parkrunfunctions)
library(lubridate)
library(ggalluvial)

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
  "Heaton Park",
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
  "Worsley Woods",
  "Kingsway Park, Urmston"
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
  cache_rows <- nrow(checkpoint$detail)
  pull_failed <- FALSE

  make_metadata <- function(detail, cache_rows, pull_failed, cached = FALSE) {
    tibble(
      home_parkrun = home_parkrun,
      event_slug = event_slug,
      cancellations_identified = n_distinct(detail$cancellation_date),
      cache_rows = if (cached) nrow(detail) else cache_rows,
      new_rows = if (cached) 0L else max(0L, nrow(detail) - cache_rows),
      failed_to_pull = pull_failed,
      cache_hit = cached
    )
  }

  if (isTRUE(checkpoint$finished)) {
    message("Event: ", home_parkrun, " [cached]")
    return(list(
      detail = checkpoint$detail,
      metadata = make_metadata(
        checkpoint$detail,
        cache_rows,
        pull_failed,
        cached = TRUE
      )
    ))
  }

  if (checkpoint$next_j > 1L) {
    message("Event: ", home_parkrun, " [resume gap ", checkpoint$next_j, "]")
  } else {
    message("Event: ", home_parkrun)
  }

  history_obj <- get_event_history_safe(event_slug)
  if (is.null(history_obj)) {
    pull_failed <- TRUE
    save_event_checkpoint(
      cache_file,
      checkpoint$detail,
      checkpoint$next_j,
      finished = FALSE
    )
    return(list(
      detail = checkpoint$detail,
      metadata = make_metadata(checkpoint$detail, cache_rows, pull_failed)
    ))
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
    return(list(
      detail = checkpoint$detail,
      metadata = make_metadata(checkpoint$detail, cache_rows, pull_failed)
    ))
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
      pull_failed <- TRUE
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
  list(
    detail = detail,
    metadata = make_metadata(detail, cache_rows, pull_failed)
  )
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

  event_studies <- purrr::map(
    seq_len(nrow(gm_meta)),
    function(i) {
      study_home_event(
        home_parkrun = gm_meta$home_parkrun[i],
        event_slug = gm_meta$event_slug[i],
        cutoff_date = cutoff_date
      )
    }
  )

  event_details <- purrr::map_dfr(event_studies, "detail")
  metadata_summary <- purrr::map_dfr(event_studies, "metadata")

  detail <- resolve_gap_week(event_details) |>
    select(-parkrunner)

  metadata_summary <- metadata_summary |>
    arrange(home_parkrun) |>
    select(
      event = home_parkrun,
      event_slug,
      cancellations_identified,
      cache_rows,
      new_rows,
      failed_to_pull,
      cache_hit
    )

  print(metadata_summary, n = Inf)
  if (any(metadata_summary$failed_to_pull)) {
    failed_events <- metadata_summary |>
      filter(failed_to_pull) |>
      pull(event)
    message("Failed to pull data for: ", paste(failed_events, collapse = ", "))
  }

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


##analysis
library(tidyverse)
detail <- read_csv("data/home_and_away/gm_home_and_away_detail.csv")
summary <- read_csv("data/home_and_away/gm_home_and_away_summary.csv")

detail |>
  summarise(
    total_runners = n_distinct(runner_id),
    total_gaps = n_distinct(cancellation_date),
    .by = home_parkrun
  ) |>
  mutate(average_runners_per_pr = total_runners / total_gaps) |>
  select(
    "home" = home_parkrun,
    "eligible_cancellations" = total_gaps,
    "total_runners" = total_runners,
    "average_runners_per_pr" = average_runners_per_pr
  ) |>
  write_csv("data/home_and_away/gm_home_and_away_summary_by_pr.csv")


summary2 <- summary |>
  mutate(
    placeholder_name = if_else(runners <= 3, "Other events", gap_week_parkrun)
  ) |>
  group_by(home_parkrun, placeholder_name) |>
  summarise(
    runners = sum(runners),
    destinations = n_distinct(gap_week_parkrun),
    .groups = "drop"
  ) |>
  mutate(
    placeholder_name = if_else(
      placeholder_name == "Other events",
      glue::glue("Other events ({destinations})"),
      placeholder_name
    )
  ) |>
  arrange(home_parkrun, desc(runners)) |>
  select(home_parkrun, "destination" = placeholder_name, runners) |>
  ungroup() |>
  mutate(pc = round(100 * runners / sum(runners), 1), .by = home_parkrun)

summary |>
  summarise(runners = sum(runners), .by = gap_week_parkrun) |>
  mutate(pc = 100 * runners / sum(runners)) |>
  arrange(desc(runners)) |>
  head()

summary |>
  filter_out(gap_week_parkrun == "Did not run") |>
  summarise(runners = sum(runners), .by = gap_week_parkrun) |>
  mutate(pc = 100 * runners / sum(runners)) |>
  arrange(desc(runners)) |>
  filter(runners >= 10) |>
  write.csv("data/home_and_away/runners_share.csv")

make_home_sankey_plot <- function(home_name, data, include_did_not_run = TRUE) {
  plot_data <- data |>
    dplyr::filter(.data$home_parkrun == .env$home_name)

  if (!include_did_not_run) {
    plot_data <- plot_data |>
      dplyr::filter(.data$destination != "Did not run")
  }

  destination_summary <- plot_data |>
    dplyr::group_by(destination) |>
    dplyr::summarise(runners = sum(runners), .groups = "drop")

  other_events_levels <- destination_summary$destination[grepl(
    "^Other events",
    destination_summary$destination
  )]
  regular_levels <- destination_summary |>
    dplyr::filter(
      !.data$destination %in% c("Did not run", other_events_levels)
    ) |>
    dplyr::arrange(dplyr::desc(.data$runners), .data$destination) |>
    dplyr::pull(.data$destination)

  destination_order <- c(
    if (include_did_not_run) "Did not run",
    regular_levels,
    other_events_levels
  ) |>
    unique()
  destination_order <- destination_order[
    !is.na(destination_order) & destination_order != ""
  ]

  plot_data <- plot_data |>
    mutate(destination = factor(destination, levels = destination_order))

  destination_levels <- levels(plot_data$destination)
  other_levels <- setdiff(
    destination_levels,
    c("Did not run", other_events_levels)
  )
  other_cols <- if (length(other_levels) > 0) {
    setNames(
      scales::hue_pal(l = 70, c = 100)(length(other_levels)),
      other_levels
    )
  } else {
    character()
  }
  destination_cols <- c(
    if (include_did_not_run) c("Did not run" = "grey35") else character(),
    setNames(rep("grey80", length(other_events_levels)), other_events_levels),
    other_cols
  )

  ggplot(plot_data, aes(axis1 = home_name, axis2 = destination, y = runners)) +
    ggalluvial::geom_alluvium(
      aes(fill = destination),
      width = 0.18,
      alpha = 0.9
    ) +
    ggalluvial::geom_stratum(width = 0.18, fill = "grey95", color = "grey70") +
    ggalluvial::stat_stratum(
      geom = "text",
      aes(label = after_stat(stratum)),
      size = 3
    ) +
    scale_fill_manual(values = destination_cols, drop = FALSE) +
    scale_y_reverse() +
    guides(fill = "none") +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

home_sankey_dir <- file.path("data", "home_and_away", "sankey_plots")
dir.create(home_sankey_dir, recursive = TRUE, showWarnings = FALSE)

home_sankey_plots <- purrr::map(
  unique(summary2$home_parkrun),
  ~ make_home_sankey_plot(.x, summary2)
)
names(home_sankey_plots) <- unique(summary2$home_parkrun)

home_sankey_send_plots <- purrr::map(
  unique(summary2$home_parkrun),
  ~ make_home_sankey_plot(.x, summary2, include_did_not_run = FALSE)
)
names(home_sankey_send_plots) <- unique(summary2$home_parkrun)

purrr::iwalk(
  home_sankey_plots,
  ~ ggsave(
    filename = file.path(
      home_sankey_dir,
      paste0(stringr::str_replace_all(.y, "[^A-Za-z0-9]+", "_"), ".png")
    ),
    plot = .x,
    width = 9,
    height = 5,
    dpi = 300
  )
)

purrr::iwalk(
  home_sankey_send_plots,
  ~ ggsave(
    filename = file.path(
      home_sankey_dir,
      paste0(
        stringr::str_replace_all(.y, "[^A-Za-z0-9]+", "_"),
        "_exclude_dnr.png"
      )
    ),
    plot = .x,
    width = 9,
    height = 5,
    dpi = 300
  )
)


library(dplyr)

season_test <- detail |>
  mutate(
    cancel_month = as.integer(format(as.Date(cancellation_date), "%m")),
    cancel_season = if_else(
      cancel_month %in% c(10, 11, 12, 1, 2, 3),
      "Oct-Mar",
      "Apr-Sep"
    ),
    did_not_run = gap_week_parkrun == "Did not run"
  ) |>
  count(cancel_season, did_not_run) |>
  tidyr::pivot_wider(
    names_from = did_not_run,
    values_from = n,
    values_fill = 0
  ) |>
  rename(did_not_run = `TRUE`, other = `FALSE`) |>
  mutate(total = did_not_run + other, rate = did_not_run / total)

season_test
