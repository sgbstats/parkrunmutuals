source("R/supabase-cache.R")
source("tests/fake-supabase.R")

with_fake_supabase({
  stopifnot(identical(cache_key("data\\results\\Test Event1.csv"), "data/results/Test Event1.csv"))
  for (path in c("/data/a", "other/a", "data/../a", "data//a", "data/a/", "data/./a")) {
    expect_error(cache_key(path), "relative paths")
  }
  expect_error(cache_key(NA_character_), "relative paths")
  cache_save_rds(list(date = as.Date("2026-07-04"), values = 1:3), "data/a.rds", config)
  stopifnot(identical(cache_read_rds("data/a.rds", config)$values, 1:3))
  x <- data.frame(id = 1L, name = "Runner")
  cache_save(x, file = "data/objects.RDa", config = config)
  loaded <- new.env(parent = emptyenv())
  cache_load("data/objects.RDa", loaded, config)
  stopifnot(identical(loaded$x, x))
  cache_write_csv(x, "data/export.csv", config)
  stopifnot(identical(as.data.frame(cache_read_csv("data/export.csv", config))$name, "Runner"))
  expect_error(cache_read_rds("data/missing.rds", config), "HTTP 404")
  store$empty <- TRUE
  expect_error(cache_read_rds("data/a.rds", config), "empty file")
  store$empty <- FALSE
  store$fail$POST <- 403L
  expect_error(cache_exists("data/missing.rds", config), "HTTP 403")
  store$fail$POST <- NULL

  for (i in seq_len(1001L)) store$objects[[sprintf("data/results/Event%04d.csv", i)]] <- raw()
  store$objects[["data/results/nested/a.csv"]] <- raw()
  paths <- cache_list("data/results", config)
  stopifnot(length(paths) == 1001L, !any(grepl("nested", paths)))
  stopifnot(length(cache_list("data/results", config, recursive = TRUE)) == 1002L)
  stopifnot(cache_exists("data/results/Event1001.csv", config))
  stopifnot(!cache_exists("data/results/Event100.csv", config))
})

with_fake_supabase({
  calls <- character()
  sleeps <- numeric()
  sleep <- function(seconds) sleeps <<- c(sleeps, seconds)
  result <- list(results = data.frame(pos = 1L, parkrunner = "Runner", id = 1L, time = "25:00"))
  fetch <- function(url) { calls <<- c(calls, url); result }
  paths <- c("data/results/Test Event1.csv", "data/results/Test Event2.csv")
  urls <- c("event1", "event2")
  cache_fetch_results(paths[1L], urls[1L], fetch, config, sleep)
  cache_fetch_results(paths, urls, fetch, config, sleep)
  cache_fetch_results(paths, urls, fetch, config, sleep)
  stopifnot(identical(calls, urls), identical(sleeps, c(23, 23)))
  stopifnot(any(grepl("Test%20Event1.csv", vapply(store$requests, `[[`, "", "url"), fixed = TRUE)))
  cache_fetch_results(character(), character(), fetch, config, sleep)
  stopifnot(length(calls) == 2L)
  expect_error(cache_fetch_results(rep(paths[1], 2), urls, fetch, config, sleep), "unique keys")
  expect_error(
    cache_fetch_results(c(paths[1], gsub("/", "\\\\", paths[1])), urls, fetch, config, sleep),
    "unique keys"
  )
  store$fail$POST <- 500L
  expect_error(cache_fetch_results(paths, urls, fetch, config, sleep), "HTTP 500")
  stopifnot(length(calls) == 2L)
  store$fail$POST <- NULL

  failing <- function(url) stop("parkrun unavailable")
  suppressWarnings(cache_fetch_results("data/results/Fail.csv", "fail", failing, config, sleep))
  error_path <- "data/result-errors/Fail.csv.rds"
  attempts <- cache_read_rds(error_path, config)
  stopifnot(length(attempts) == 1L, attempts[[1]]$error == "parkrun unavailable")
  invalid <- function(url) list(results = data.frame())
  suppressWarnings(cache_fetch_results(
    "data/results/Fail.csv", "fail", invalid, config, sleep, retry_failed = TRUE
  ))
  attempts <- cache_read_rds(error_path, config)
  stopifnot(length(attempts) == 2L, attempts[[2]]$error == "Invalid or empty results returned")
  stopifnot(!cache_exists("data/results/Fail.csv", config), tail(sleeps, 1) == 23)
  suppressWarnings(cache_fetch_results("data/results/Fail.csv", "fail", failing, config, sleep))
  stopifnot(length(cache_read_rds(error_path, config)) == 2L)
  store$fail$PUT <- 500L
  expect_error(cache_fetch_results(
    "data/results/Fail.csv", "fail", fetch, config, sleep, retry_failed = TRUE
  ), "HTTP 500")
  stopifnot(!cache_exists("data/results/Fail.csv", config))
  store$fail$PUT <- NULL
  store$fail$PUT <- 500L
  expect_error(
    cache_fetch_results(
      "data/results/UnableToLog.csv", "broken", failing, config, sleep
    ),
    "HTTP 500"
  )
  stopifnot(!cache_exists("data/result-errors/UnableToLog.csv.rds", config))
  store$fail$PUT <- NULL
  cache_fetch_results(c(paths, "data/results/Fail.csv"), c(urls, "fail"), fetch, config, sleep, retry_failed = TRUE)
  stopifnot(identical(calls, c(urls, "fail", "fail")))
  stopifnot(length(cache_read_rds(error_path, config)) == 2L)
  expect_error(
    cache_write("data/results/Fail.csv", result, saveRDS, config, upsert = FALSE),
    "HTTP 409"
  )

  history_calls <- 0L
  history <- function() { history_calls <<- history_calls + 1L; list(id = 1L) }
  cache_fetch_rds("data/history/1/2026-11-23.rds", history, config, sleep)
  cache_fetch_rds("data/history/1/2026-11-23.rds", history, config, sleep)
  cache_fetch_rds("data/history/1/2026-11-24.rds", history, config, sleep)
  stopifnot(history_calls == 2L)
  expect_error(cache_fetch_rds("data/null.rds", function() NULL, config, sleep), "No data")
  stopifnot(!cache_exists("data/null.rds", config))
})
with_fake_supabase({
  legacy_log <- tempfile(fileext = ".csv")
  writeLines(c("name,event_no,", "Bespoke,1", "Other,155"), legacy_log)
  calls <- character()
  fetch <- function(url) {
    calls <<- c(calls, url)
    if (url == "broken") stop("Request failed [404]")
    list(results = data.frame(pos = 1L, parkrunner = "A", id = 1L, time = "25:00"))
  }
  sleep <- function(seconds) stopifnot(seconds == 23)
  paths <- c("data/results/Bespoke1.csv", "data/results/Good1.csv", "data/results/Bad1.csv")
  urls <- c("bespoke", "good", "broken")
  events <- c("Bespoke", "Good", "Bad")
  available <- suppressWarnings(cache_fetch_results(
    paths, urls, fetch, config, sleep, events, legacy_log
  ))
  stopifnot(identical(available, paths[2]), identical(calls, c("good", "broken")))
  attempts <- cache_read_rds("data/result-errors/Bad1.csv.rds", config)
  stopifnot(
    attempts[[1]]$event == "Bad", attempts[[1]]$url == "broken",
    grepl("404", attempts[[1]]$error), nzchar(attempts[[1]]$time)
  )
  calls <- character()
  suppressWarnings(cache_fetch_results(paths, urls, fetch, config, sleep, events, legacy_log))
  stopifnot(length(calls) == 0L)
  available <- suppressWarnings(cache_fetch_results(
    paths, urls, fetch, config, sleep, events, legacy_log, retry_failed = TRUE
  ))
  stopifnot(identical(available, paths[1:2]), identical(calls, c("bespoke", "broken")))
  stopifnot(length(cache_read_rds("data/result-errors/Bad1.csv.rds", config)) == 2L)
  stopifnot(identical(readLines(legacy_log), c("name,event_no,", "Bespoke,1", "Other,155")))
  unlink(legacy_log)
})
with_fake_supabase({
  path <- "data/results/Bootstrap-test-123.csv"
  stopifnot(!file.exists(path))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(
    data.frame(pos = 1L, parkrunner = "Legacy", id = 1L, time = "25:00"),
    path, row.names = FALSE
  )
  tryCatch({
    original <- readBin(path, "raw", file.info(path)$size)
    fetch <- function(url) stop("A legacy result must not be fetched again")
    sleep <- function(seconds) stop("A legacy import does not call parkrun")
    cache_fetch_results(path, "legacy", fetch, config, sleep)
    cache_fetch_results(path, "legacy", fetch, config, sleep)
    stopifnot(identical(store$objects[[path]], original))
    stopifnot(sum(vapply(store$requests, function(x) x$method == "PUT", logical(1))) == 1L)
  }, finally = unlink(path))
})
cat("Supabase cache tests passed\n")
