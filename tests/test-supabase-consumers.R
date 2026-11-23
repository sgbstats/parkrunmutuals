source("R/supabase-cache.R")
source("tests/fake-supabase.R")
library(tidyverse)

evaluate_before <- function(path, expression, envir) {
  for (item in parse(path)) {
    if (identical(item, expression)) break
    eval(item, envir)
  }
}

with_fake_supabase({
  study <- new.env(parent = globalenv())
  evaluate_before("R/home_and_away.R", quote(if (sys.nframe() == 0) { main() }), study)
  calls <- c(history = 0L, results = 0L, runner = 0L)
  sleeps <- numeric()
  sleep <- function(seconds) sleeps <<- c(sleeps, seconds)
  study$cache_fetch_rds <- function(path, fetch) {
    cache_fetch_rds(path, fetch, config, sleep)
  }
  mocks <- list(
    get_event_history = function(event) {
      calls["history"] <<- calls["history"] + 1L
      list(history = data.frame(
        date = as.Date(c("2026-07-04", "2026-07-18")), event_no = 1:2
      ))
    },
    get_result = function(event, event_no, as_Date) {
      calls["results"] <<- calls["results"] + 1L
      list(results = data.frame(id = 1L, parkrunner = "Runner"))
    },
    get_all_runs = function(id, as_Date) {
      calls["runner"] <<- calls["runner"] + 1L
      list(results = data.frame(
        event_date = as.Date("2026-07-11"), event = "Away", short = "away"
      ))
    }
  )
  with_namespace_mocks("parkrunfunctions", mocks, {
    first <- study$study_home_event("Home", "home", as.Date("2026-07-01"))
    second <- study$study_home_event("Home", "home", as.Date("2026-07-01"))
    stopifnot(
      nrow(first$detail) == 1L, second$metadata$cache_hit,
      identical(first$detail, second$detail)
    )
    detail <- study$resolve_gap_week(first$detail)
    repeated <- study$resolve_gap_week(first$detail)
    stopifnot(
      identical(detail, repeated), detail$gap_week_parkrun == "Away",
      identical(unname(calls), c(1L, 2L, 1L)), identical(sleeps, rep(23, 4))
    )
    # Partially completed checkpoints and raw results survive a second invocation.
    study$save_event_checkpoint(study$event_cache_file("home"), tibble(), 1L)
    resumed <- study$study_home_event("Home", "home", as.Date("2026-07-01"))
    stopifnot(nrow(resumed$detail) == 1L, identical(unname(calls), c(1L, 2L, 1L)))
    study$save_event_checkpoint(study$event_cache_file("home"), resumed$detail, 2L, TRUE)
    checkpoint <- study$load_event_checkpoint(study$event_cache_file("home"))
    stopifnot(checkpoint$finished, checkpoint$next_j == 2L)
    store$fail$PUT <- 500L
    expect_error(study$save_event_checkpoint(study$event_cache_file("home"), tibble(), 1L), "HTTP 500")
    store$fail$PUT <- NULL
    stopifnot(study$load_event_checkpoint(study$event_cache_file("home"))$finished)
  })
})

with_fake_supabase({
  metadata <- list(events = list(features = list(list(
    properties = list(
      eventname = "test", EventShortName = "Test", EventLongName = "Test parkrun",
      countrycode = 97, EventLocation = "Test"
    ),
    geometry = list(coordinates = c(-2, 53))
  ))))
  cache_save_rds(metadata, "data/swb/events.rds", config)
  swb <- new.env(parent = globalenv())
  calls <- c(results = 0L, runner = 0L)
  sleeps <- numeric()
  fail_runner <- TRUE
  swb$cache_fetch_rds <- function(path, fetch) {
    cache_fetch_rds(path, fetch, config, function(seconds) sleeps <<- c(sleeps, seconds))
  }
  swb$get_result <- function(url, extra_data) {
    calls["results"] <<- calls["results"] + 1L
    list(results = data.frame(id = 1L, finishes = 150L))
  }
  swb$get_all_runs <- function(id, as_Date, as_hms) {
    calls["runner"] <<- calls["runner"] + 1L
    if (fail_runner) stop("runner temporarily unavailable")
    list(results = data.frame(
      event_date = as.Date("2020-01-01") + seq_len(120L),
      event = "test", time = sprintf("25:%02d", rep(0:59, 2))
    ))
  }
  suppressWarnings(sys.source("R/swb/swb.R", swb))
  stopifnot(!cache_exists("data/pr_done.RDa", config))
  fail_runner <- FALSE
  sys.source("R/swb/swb.R", swb)
  sys.source("R/swb/swb.R", swb)
  stopifnot(
    identical(unname(calls), c(1L, 2L)), identical(sleeps, rep(23, 3)),
    swb$id_done$swb == 1L, swb$id_done$events == 60L
  )
  sys.source("R/swb/swb2.R", swb)
  stopifnot(swb$out$all == 1L, swb$out$finishes150 == 1L)
  id_done2 <- tibble(id = integer(), events = integer(), swb = integer(), distinct_events = integer(), tq = double())
  cache_save(id_done2, file = "data/swb_id2.RDa", config = config)
  evaluate_before(
    "R/swb/swb3.R",
    quote(fit <- survival::survfit(Surv(events, swb) ~ 1, data = id_done2)),
    swb
  )
  stopifnot(swb$id_done2$swb == 1L, swb$id_done2$events == 120L)
  stopifnot(identical(unname(calls), c(1L, 2L)))
  restored <- new.env(parent = emptyenv())
  cache_load("data/swb_id2.RDa", restored, config)
  stopifnot(identical(restored$id_done2, swb$id_done2))
})
cat("Home-and-away and SWB consumer tests passed\n")

with_fake_supabase({
  source("R/wnr-league-helpers.R")
  all_parkruns <- list(
    seb = list(
      id = "493595", name = "Seb",
      results = data.frame(time = "25:00", event_date = "06/06/2026")
    )
  )
  cache_save(all_parkruns, file = "data/all_parkruns.RDa", config = config)
  events <- c("peel", "penningtonflash", "alexandra", "fletchermoss", "wythenshawe")
  for (month in seq_along(events)) {
    dates <- league_dates(2026, month + 6L, as.Date("2026-11-23"))
    for (index in seq_along(dates)) {
      result <- list(
        date = dates[index],
        results = data.frame(id = "493595", time = "24:00"),
        volunteers = data.frame(id = "493595", parkrunner = "Seb")
      )
      key <- paste0(events[month], index)
      cache_write(
        paste0("data/wnr/", key, ".RDa"), result,
        function(value, file) {
          env <- new.env(parent = emptyenv())
          env[[key]] <- value
          save(list = key, file = file, envir = env)
        },
        config
      )
    }
  }
  league <- new.env(parent = globalenv())
  league$Sys.Date <- function() as.Date("2026-11-23")
  league$auth_wnr_sheets <- function() invisible(TRUE)
  scores <- NULL
  league$write_sheet <- function(data, ss, sheet) {
    stopifnot(sheet == "Scores")
    scores <<- data
  }
  league$fetch_league_month <- function(event, dates, cache_dir) {
    fetch_league_month(
      event, dates, cache_dir,
      fetch = function(...) stop("Cached league results were fetched again"),
      history_fetch = function(...) stop("Cached league history was fetched again"),
      sleep = function(...) stop("No parkrun request should have occurred")
    )
  }
  sys.source("R/wnr-parkrun-league.R", league)
  stopifnot(
    nrow(scores) == 1L, scores$name == "Seb", scores$total_pts == 55,
    scores$hc_peel == "00:25:00", scores$time_peel == "00:24:00",
    scores$pts_peel == 10, scores$vol_peel == 1
  )
})
cat("League scoring integration test passed\n")

with_fake_supabase({
  all_parkruns <- list(alexbuckley = list(
    name = "Alex", results = data.frame(event = "Test Event", event_no = 1L, short = "test")
  ))
  cache_save(all_parkruns, file = "data/all_parkruns.RDa", config = config)
  cache_write_csv(
    data.frame(
      event = "Test Event", event_no = 1L, pos = 1L, parkrunner = "Alex",
      id = 1L, time = "25:00", short = "test"
    ),
    "data/results/Test Event1.csv", config
  )
  benchmark <- new.env(parent = globalenv())
  sys.source("R/benchmark.R", benchmark)
  stopifnot(
    nrow(benchmark$all_results_test) == 1L,
    benchmark$all_results$name == "Alex",
    !file.exists(benchmark$benchmark_file)
  )
})
cat("Remote benchmark input test passed\n")

with_fake_supabase({
  check <- new.env(parent = globalenv())
  calls <- c(history = 0L, results = 0L)
  sleeps <- numeric()
  check$cache_fetch_rds <- function(path, fetch) {
    cache_fetch_rds(path, fetch, config, function(seconds) sleeps <<- c(sleeps, seconds))
  }
  mocks <- list(
    get_all_runs = function(id) {
      calls["history"] <<- calls["history"] + 1L
      list(results = data.frame(event = "Test", event_no = 1L, url = "test"))
    },
    get_result = function(url) {
      calls["results"] <<- calls["results"] + 1L
      list(volunteers = data.frame(id = "1026950", role = "Marshal"))
    }
  )
  with_namespace_mocks("parkrunfunctions", mocks, {
    sys.source("R/pr_check.R", check)
    sys.source("R/pr_check.R", check)
    stopifnot(
      check$volunteer_roles$role == "Marshal",
      identical(unname(calls), c(1L, 1L)), identical(sleeps, c(23, 23))
    )
  })
})
cat("Volunteer-check caching test passed\n")
