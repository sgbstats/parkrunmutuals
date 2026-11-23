source("tests/fake-supabase.R")
source("R/prmutuals.R")

with_fake_supabase({
  history_calls <- character()
  result_calls <- character()
  sleeps <- numeric()
  sleep <- function(seconds) sleeps <<- c(sleeps, seconds)
  history <- function(id) {
    history_calls <<- c(history_calls, id)
    list(
      id = id, name = paste("Runner", id),
      results = data.frame(
        event = "Test Event", event_no = c(1L, 2L),
        short = "test", url = c("event1", "event2")
      )
    )
  }
  fetch <- function(url) {
    result_calls <<- c(result_calls, url)
    list(results = data.frame(
      pos = 1:2, parkrunner = c("Runner 1", "Runner 2"),
      id = 1:2, time = c("25:00", "26:00")
    ))
  }
  distances <- function() list(
    distance = data.frame(name.x = "test", name.y = "test", miles = 0, dist = 0, short = "test"),
    parkruns_list = data.frame(name = "test", short = "Test Event")
  )
  run <- function(today = as.Date("2026-11-23")) {
    publish_prmutuals(c("1", "2"), today, config, history, fetch, distances, sleep)
  }
  run()
  first_bundle <- store$objects[["parkrunmutuals/data.RDa"]]
  # A new invocation represents a second machine: no local result files are used.
  run()
  stopifnot(
    identical(history_calls, c("1", "2")),
    identical(result_calls, c("event1", "event2")),
    identical(sleeps, rep(23, 4))
  )
  run(as.Date("2026-11-24"))
  stopifnot(length(history_calls) == 4L, length(result_calls) == 2L)
  bundle_file <- tempfile(fileext = ".RDa")
  writeBin(store$objects[["parkrunmutuals/data.RDa"]], bundle_file)
  app_storage <- new.env(parent = globalenv())
  sys.source("parkrunmutuals/supabase-storage.R", app_storage)
  bundle <- app_storage$load_parkrunmutuals_bundle(bundle_file)
  unlink(bundle_file)
  app_globals <- new.env(parent = emptyenv())
  app_storage$load_parkrunmutuals_data(app_globals, config)
  stopifnot(identical(app_globals$all_results, bundle$all_results))
  analysis_globals <- new.env(parent = emptyenv())
  load_parkrunmutuals_data(analysis_globals, config)
  stopifnot(identical(analysis_globals$names_all, bundle$names_all))
  stopifnot(
    nrow(bundle$all_results) == 8L,
    identical(bundle$runners, c("Runner 1", "Runner 2")),
    identical(bundle$parkruns, "Test Event"),
    nrow(bundle$events_done) == 2L,
    identical(bundle$names_ids$id, c("1", "2")),
    all(bundle$names_all$n == 2L),
    identical(bundle$date, as.Date("2026-11-24"))
  )
  previous <- store$objects[["parkrunmutuals/data.RDa"]]
  invalid_history <- function(id) list(name = "Bad")
  suppressWarnings(expect_error(
    publish_prmutuals("3", as.Date("2026-11-24"), config, invalid_history, fetch, distances, sleep),
    "could not be loaded"
  ))
  stopifnot(!cache_exists("data/runner-histories/3/2026-11-24.rds", config))
  stopifnot(identical(previous, store$objects[["parkrunmutuals/data.RDa"]]))
  new_history <- function(id) {
    runner <- history(id)
    runner$results$event_no <- 3:4
    runner$results$url <- c("event3", "event4")
    runner
  }
  expect_error(
    publish_prmutuals(
      "3", as.Date("2026-11-25"), config, new_history,
      function(url) stop("result unavailable"), distances, sleep
    ),
    "result unavailable"
  )
  stopifnot(identical(previous, store$objects[["parkrunmutuals/data.RDa"]]))
  stopifnot(!cache_exists("data/results/Test Event3.csv", config))
  expect_error(
    publish_prmutuals(
      c("1", "2"), as.Date("2026-11-24"), config, history, fetch,
      function() list(distance = "invalid", parkruns_list = data.frame()), sleep
    ),
    "invalid data-frame objects"
  )
  stopifnot(identical(previous, store$objects[["parkrunmutuals/data.RDa"]]))
  store$fail$GET <- 403L
  suppressWarnings(expect_error(run(), "could not be loaded"))
  stopifnot(identical(previous, store$objects[["parkrunmutuals/data.RDa"]]))
})

with_fake_supabase({
  migration <- new.env(parent = globalenv())
  sys.source("R/migrate-supabase-cache.R", migration)
  paths <- c("data/migration-test.rds", "data/migration test.csv")
  dir.create("data", showWarnings = FALSE)
  saveRDS(list(id = 1L), paths[1])
  write.csv(data.frame(id = 1L), paths[2], row.names = FALSE)
  untracked <- list()
  migration$system2 <- function(command, args, stdout = FALSE) {
    stopifnot(command == "git")
    if ("ls-files" %in% args) return(paths)
    stopifnot(identical(args[1:3], c("rm", "--cached", "--")))
    untracked[[length(untracked) + 1L]] <<- args
    0L
  }
  tryCatch({
    migration$migrate_supabase_cache(config, untrack = TRUE)
    stopifnot(length(untracked) == 1L, all(file.exists(paths)))
    uploads <- sum(vapply(store$requests, function(x) x$method == "PUT", logical(1)))
    migration$migrate_supabase_cache(config, untrack = FALSE)
    stopifnot(
      sum(vapply(store$requests, function(x) x$method == "PUT", logical(1))) == uploads
    )
    store$corrupt <- TRUE
    expect_error(migration$migrate_supabase_cache(config, untrack = TRUE), "verification failed")
    stopifnot(length(untracked) == 1L, all(file.exists(paths)))
    store$corrupt <- FALSE
    store$fail$GET <- 403L
    expect_error(migration$migrate_supabase_cache(config, untrack = TRUE), "HTTP 403")
    stopifnot(length(untracked) == 1L)
    store$fail$GET <- NULL
    rm(list = "data/migration-test.rds", envir = store$objects)
    store$fail$PUT <- 500L
    expect_error(migration$migrate_supabase_cache(config, untrack = TRUE), "HTTP 500")
    stopifnot(length(untracked) == 1L)
  }, finally = unlink(paths))
})

source("R/wnr-league-tests.R")
cat("Publication, app compatibility and migration tests passed\n")
