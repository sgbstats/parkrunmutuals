league_dates <- function(year, month, today = Sys.Date()) {
  start <- as.Date(sprintf("%04d-%02d-01", year, month))
  end <- seq(start, by = "month", length.out = 2L)[2L] - 1L
  days <- seq(start, end, by = "day")
  holidays <- as.Date(c(
    sprintf("%04d-01-01", year),
    sprintf("%04d-12-25", year)
  ))
  dates <- sort(unique(c(
    days[as.POSIXlt(days)$wday == 6L],
    holidays[holidays >= start & holidays <= end]
  )))
  dates[dates <= today]
}

handicap_start <- function(year, month) {
  if (month == 1L) {
    as.Date(sprintf("%04d-07-01", year - 1L))
  } else {
    as.Date(sprintf("%04d-01-01", year))
  }
}

read_league_cache <- function(cache_dir, event) {
  paths <- list.files(cache_dir, pattern = "\\.RDa$", full.names = TRUE)
  paths <- paths[grepl(paste0("^", event, "[0-9]+\\.RDa$"), basename(paths))]
  entries <- lapply(paths, function(path) {
    env <- new.env(parent = emptyenv())
    keys <- load(path, envir = env)
    if (length(keys) != 1L) {
      stop("Unexpected cache format: ", path)
    }
    x <- env[[keys]]
    if (is.null(x$date) || is.na(as.Date(x$date))) {
      warning("Cache has no valid date: ", path)
      return(NULL)
    }
    list(
      date = as.Date(x$date),
      event_no = as.integer(sub(
        paste0("^", event, "([0-9]+)\\.RDa$"),
        "\\1",
        basename(path)
      )),
      result = x
    )
  })
  date_paths <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  date_paths <- date_paths[grepl(
    paste0("^", event, "-[0-9]{4}-[0-9]{2}-[0-9]{2}\\.rds$"),
    basename(date_paths)
  )]
  entries <- Filter(Negate(is.null), entries)
  numbered_dates <- vapply(
    entries,
    function(x) as.character(x$date),
    character(1)
  )
  c(
    entries,
    lapply(
      date_paths[
        !sub(
          "\\.rds$",
          "",
          sub(paste0("^", event, "-"), "", basename(date_paths))
        ) %in%
          numbered_dates
      ],
      function(path) {
        x <- readRDS(path)
        expected <- sub(
          "\\.rds$",
          "",
          sub(paste0("^", event, "-"), "", basename(path))
        )
        if (
          is.null(x$date) ||
            is.na(as.Date(x$date)) ||
            as.character(as.Date(x$date)) != expected
        ) {
          warning("Cache date does not match filename: ", path)
          return(NULL)
        }
        list(date = as.Date(x$date), event_no = NA_integer_, result = x)
      }
    ) |>
      Filter(f = Negate(is.null))
  )
}

confirmed_missing_dates <- function(entries, dates) {
  numbered <- Filter(function(x) !is.na(x$event_no), entries)
  if (length(numbered) < 2L) {
    return(as.Date(character()))
  }
  numbered <- numbered[order(vapply(
    numbered,
    function(x) as.numeric(x$date),
    numeric(1)
  ))]
  missing <- as.Date(character())
  for (k in seq_len(length(numbered) - 1L)) {
    previous <- numbered[[k]]
    following <- numbered[[k + 1L]]
    if (following$event_no == previous$event_no + 1L) {
      missing <- c(
        missing,
        dates[dates > previous$date & dates < following$date]
      )
    }
  }
  unique(missing)
}

fetch_league_month <- function(
  event,
  dates,
  cache_dir,
  fetch = parkrunfunctions::get_result,
  sleep = Sys.sleep,
  history_fetch = parkrunfunctions::get_event_history
) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dates <- dates[dates <= Sys.Date()]
  entries <- read_league_cache(cache_dir, event)
  # A cached history resolves cancelled dates and event numbers in new months.
  history_path <- file.path(cache_dir, paste0(event, "-history.rds"))
  history <- if (file.exists(history_path)) readRDS(history_path) else NULL
  history_refreshed <- FALSE
  cached_dates <- as.Date(vapply(
    entries,
    function(x) as.character(x$date),
    character(1)
  ))
  unknown <- dates[
    !dates %in% cached_dates &
      !dates %in% confirmed_missing_dates(entries, dates)
  ]
  if (length(unknown) && is.null(history)) {
    history_refreshed <- TRUE
    history <- tryCatch(
      history_fetch(event = event)$history,
      error = function(e) {
        warning(sprintf("History for %s: %s", event, conditionMessage(e)))
        NULL
      },
      finally = sleep(23)
    )
    if (!is.null(history)) saveRDS(history, history_path)
  }
  for (index in seq_along(dates)) {
    date <- dates[index]
    cached_dates <- as.Date(vapply(
      entries,
      function(x) as.character(x$date),
      character(1)
    ))
    if (
      date %in%
        cached_dates ||
        date %in% confirmed_missing_dates(entries, dates)
    ) {
      next
    }
    if (
      !history_refreshed &&
        !is.null(history) &&
        nrow(history) &&
        date > max(as.Date(history$date))
    ) {
      history_refreshed <- TRUE
      refreshed <- tryCatch(
        history_fetch(event = event)$history,
        error = function(e) {
          warning(sprintf("History for %s: %s", event, conditionMessage(e)))
          NULL
        },
        finally = sleep(23)
      )
      if (!is.null(refreshed)) {
        history <- refreshed
        saveRDS(history, history_path)
      }
    }
    event_no <- NA_integer_
    if (!is.null(history)) {
      match_date <- which(as.Date(history$date) == date)
      if (!length(match_date)) {
        # A missing date in a retrieved history is not a permanent cancellation:
        # the latest results may not yet be published.
        if (date < max(as.Date(history$date))) next
      } else {
        event_no <- history$event_no[match_date[1L]]
      }
    }
    url <- sprintf("https://www.parkrun.org.uk/%s/results/%s/", event, date)
    x <- tryCatch(
      fetch(url = url, as_hms = TRUE, as_Date = TRUE),
      error = function(e) {
        warning(sprintf("%s on %s: %s", event, date, conditionMessage(e)))
        NULL
      },
      finally = sleep(23)
    )
    if (is.null(x)) {
      next
    }
    if (is.null(x$date) || is.na(as.Date(x$date)) || as.Date(x$date) != date) {
      warning(sprintf(
        "%s on %s returned a different event date; ignoring",
        event,
        date
      ))
      next
    }
    if (!is.na(event_no)) {
      key <- paste0(event, event_no)
      env <- new.env(parent = emptyenv())
      env[[key]] <- x
      save(
        list = key,
        file = file.path(cache_dir, paste0(key, ".RDa")),
        envir = env
      )
    } else {
      saveRDS(x, file.path(cache_dir, sprintf("%s-%s.rds", event, date)))
    }
    entries[[length(entries) + 1L]] <- list(
      date = date,
      event_no = event_no,
      result = x
    )
  }
  entries[vapply(entries, function(x) x$date %in% dates, logical(1))]
}
