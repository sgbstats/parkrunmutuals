source("R/wnr-league-helpers.R")

stopifnot(
  identical(
    league_dates(2026, 12, as.Date("2026-12-31")),
    as.Date(c(
      "2026-12-05",
      "2026-12-12",
      "2026-12-19",
      "2026-12-25",
      "2026-12-26"
    ))
  ),
  as.Date("2027-01-01") %in% league_dates(2027, 1, as.Date("2027-01-31")),
  !as.Date("2026-10-10") %in% league_dates(2026, 10, as.Date("2026-10-03")),
  handicap_start(2027, 1) == as.Date("2026-07-01"),
  handicap_start(2026, 10) == as.Date("2026-01-01")
)

entries <- list(
  list(date = as.Date("2026-08-22"), event_no = 688L),
  list(date = as.Date("2026-09-05"), event_no = 689L)
)
stopifnot(identical(
  confirmed_missing_dates(entries, as.Date("2026-08-29")),
  as.Date("2026-08-29")
))
entries[[2L]]$event_no <- 690L
stopifnot(length(confirmed_missing_dates(entries, as.Date("2026-08-29"))) == 0L)

cache_dir <- tempfile("wnr-test-")
dir.create(cache_dir)
calls <- 0L
sleeps <- numeric()
mock_sleep <- function(seconds) sleeps <<- c(sleeps, seconds)
mock_history <- function(event) {
  list(
    history = data.frame(
      date = as.Date(c("2026-07-04", "2026-07-18")),
      event_no = c(232L, 233L)
    )
  )
}
mock_fetch <- function(url, as_hms, as_Date) {
  calls <<- calls + 1L
  list(
    date = as.Date(sub(".*/results/([0-9-]+)/$", "\\1", url)),
    results = data.frame(id = "1", time = "00:25:00"),
    volunteers = data.frame(id = "1", parkrunner = "A")
  )
}
date <- as.Date("2026-07-04")
stopifnot(
  length(fetch_league_month(
    "peel",
    date,
    cache_dir,
    mock_fetch,
    mock_sleep,
    mock_history
  )) ==
    1L
)
stopifnot(
  length(fetch_league_month(
    "peel",
    date,
    cache_dir,
    mock_fetch,
    mock_sleep,
    mock_history
  )) ==
    1L
)
stopifnot(calls == 1L, identical(sleeps, c(23, 23)))

bad_fetch <- function(url, as_hms, as_Date) {
  calls <<- calls + 1L
  list(date = as.Date("2026-07-11"))
}
suppressWarnings(fetch_league_month(
  "peel",
  as.Date("2026-07-18"),
  cache_dir,
  bad_fetch,
  mock_sleep,
  mock_history
))
stopifnot(
  calls == 2L,
  length(read_league_cache(cache_dir, "peel")) == 1L,
  identical(sleeps, c(23, 23, 23))
)

failing_fetch <- function(url, as_hms, as_Date) {
  calls <<- calls + 1L
  stop("temporary error")
}
suppressWarnings(fetch_league_month(
  "peel",
  as.Date("2026-07-25"),
  cache_dir,
  failing_fetch,
  mock_sleep,
  mock_history
))
suppressWarnings(fetch_league_month(
  "peel",
  as.Date("2026-07-25"),
  cache_dir,
  failing_fetch,
  mock_sleep,
  mock_history
))
stopifnot(calls == 4L, identical(sleeps, rep(23, 7)))
fetch_league_month(
  "peel",
  as.Date("2099-01-01"),
  cache_dir,
  failing_fetch,
  mock_sleep,
  mock_history
)
stopifnot(calls == 4L, identical(sleeps, rep(23, 7)))
unlink(cache_dir, recursive = TRUE)
