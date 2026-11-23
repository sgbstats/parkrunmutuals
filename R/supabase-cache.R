source("R/supabase-storage.R")

cache_key <- function(path) {
  if (length(path) != 1L || is.na(path)) {
    stop("Cache paths must be relative paths under data/", call. = FALSE)
  }
  key <- gsub("\\\\", "/", path)
  parts <- strsplit(key, "/", fixed = TRUE)[[1]]
  if (
    !startsWith(key, "data/") || grepl("//|/$", key) ||
      any(parts %in% c("", ".", ".."))
  ) {
    stop("Cache paths must be relative paths under data/", call. = FALSE)
  }
  key
}

cache_list <- function(
  directory, config = supabase_storage_config(), recursive = FALSE, search = ""
) {
  prefix <- cache_key(paste0(sub("[/\\\\]+$", "", directory), "/placeholder"))
  prefix <- sub("/placeholder$", "", prefix)
  offset <- 0L
  paths <- character()
  repeat {
    response <- httr::POST(
      paste0(config$url, "/storage/v1/object/list/", config$bucket),
      httr::add_headers(.headers = supabase_storage_headers(config)),
      body = list(
        prefix = prefix, limit = 1000L, offset = offset, search = search,
        sortBy = list(column = "name", order = "asc")
      ),
      encode = "json",
      httr::timeout(120)
    )
    supabase_require_success(response, "cache listing")
    entries <- httr::content(response, as = "parsed", simplifyVector = FALSE)
    for (entry in entries) {
      path <- paste(prefix, entry$name, sep = "/")
      if (!is.null(entry$id)) {
        paths <- c(paths, path)
      } else if (recursive) {
        paths <- c(paths, cache_list(path, config, recursive = TRUE))
      }
    }
    if (length(entries) < 1000L) break
    offset <- offset + length(entries)
  }
  paths
}

cache_exists <- function(path, config = supabase_storage_config()) {
  key <- cache_key(path)
  key %in% cache_list(dirname(key), config, search = basename(key))
}

cache_read <- function(path, reader, config = supabase_storage_config()) {
  destination <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  on.exit(unlink(destination), add = TRUE)
  supabase_download_object(cache_key(path), destination, config)
  reader(destination)
}

cache_write <- function(
  path, value, writer, config = supabase_storage_config(), upsert = TRUE
) {
  destination <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  on.exit(unlink(destination), add = TRUE)
  writer(value, destination)
  supabase_upload_object(destination, cache_key(path), config, upsert = upsert)
  invisible(value)
}

cache_read_rds <- function(path, config = supabase_storage_config()) {
  cache_read(path, readRDS, config)
}

cache_save_rds <- function(value, path, config = supabase_storage_config()) {
  cache_write(path, value, saveRDS, config)
}

cache_load <- function(
  path, envir = parent.frame(), config = supabase_storage_config()
) {
  cache_read(path, function(file) load(file, envir = envir), config)
}

cache_save <- function(
  ..., file, envir = parent.frame(), config = supabase_storage_config()
) {
  objects <- as.character(substitute(list(...)))[-1L]
  cache_write(
    file, objects,
    function(objects, destination) {
      save(list = objects, file = destination, envir = envir)
    },
    config
  )
}

cache_read_csv <- function(path, config = supabase_storage_config()) {
  cache_read(
    path,
    function(file) readr::read_csv(file, show_col_types = FALSE),
    config
  )
}

cache_write_csv <- function(value, path, config = supabase_storage_config()) {
  cache_write(
    path, value,
    function(value, file) utils::write.csv(value, file, row.names = FALSE),
    config
  )
}

cache_fetch_rds <- function(
  path, fetch, config = supabase_storage_config(), sleep = Sys.sleep
) {
  if (cache_exists(path, config)) return(cache_read_rds(path, config))
  value <- tryCatch(fetch(), finally = sleep(23))
  if (is.null(value)) stop("No data returned for ", path, call. = FALSE)
  cache_write(path, value, saveRDS, config, upsert = FALSE)
  value
}

cache_fetch_results <- function(
  paths, urls, fetch = parkrunfunctions::get_result,
  config = supabase_storage_config(), sleep = Sys.sleep,
  events = rep(NA_character_, length(paths)),
  legacy_log = "error_log.txt", retry_failed = FALSE
) {
  paths <- vapply(paths, cache_key, character(1), USE.NAMES = FALSE)
  if (
    length(paths) != length(urls) || length(paths) != length(events) ||
      anyDuplicated(paths)
  ) {
    stop("Result paths and URLs must have matching, unique keys", call. = FALSE)
  }
  legacy_events <- character()
  if (file.exists(legacy_log)) {
    legacy <- utils::read.csv(legacy_log, stringsAsFactors = FALSE)
    if (!"name" %in% names(legacy)) {
      stop("Legacy error log is missing the name column: ", legacy_log)
    }
    legacy_events <- legacy$name[!is.na(legacy$name) & nzchar(legacy$name)]
  }
  stored <- cache_list("data/results", config)
  error_paths <- paste0("data/result-errors/", basename(paths), ".rds")
  logged <- cache_list("data/result-errors", config)
  skipped <- character()
  for (index in which(!paths %in% stored)) {
    error_path <- error_paths[index]
    legacy_failed <- !is.na(events[index]) && events[index] %in% legacy_events
    if (!retry_failed && (legacy_failed || error_path %in% logged)) {
      skipped <- c(skipped, paths[index])
      next
    }
    legacy_file <- file.exists(paths[index])
    result <- if (legacy_file) {
      list(results = utils::read.csv(paths[index]))
    } else {
      tryCatch(
        fetch(url = urls[index]),
        error = identity,
        finally = sleep(23)
      )
    }
    failure <- if (inherits(result, "error")) {
      conditionMessage(result)
    } else if (
      !is.list(result) || !is.data.frame(result$results) ||
        !all(c("pos", "parkrunner", "id", "time") %in% names(result$results)) ||
        nrow(result$results) == 0L
    ) {
      "Invalid or empty results returned"
    } else {
      NULL
    }
    if (!is.null(failure)) {
      attempts <- if (error_path %in% logged) {
        cache_read_rds(error_path, config)
      } else {
        list()
      }
      attempts[[length(attempts) + 1L]] <- list(
        event = events[index], path = paths[index], url = urls[index],
        time = format(Sys.time(), tz = "UTC", usetz = TRUE), error = failure
      )
      cache_save_rds(attempts, error_path, config)
      logged <- union(logged, error_path)
      skipped <- c(skipped, paths[index])
      warning(sprintf("Result skipped for %s: %s", urls[index], failure), call. = FALSE)
      next
    }
    # Preserve legacy bytes so the verified migration can still untrack them.
    if (legacy_file) {
      supabase_upload_object(paths[index], paths[index], config, upsert = FALSE)
    } else {
      cache_write(
        paths[index], result$results,
        function(value, file) utils::write.csv(value, file, row.names = FALSE),
        config, upsert = FALSE
      )
    }
    stored <- c(stored, paths[index])
  }
  if (length(skipped)) {
    warning(
      sprintf(
        "%d result(s) omitted because of recorded errors: %s%s; see error_log.txt and data/result-errors/ in Supabase",
        length(skipped), paste(head(skipped, 10L), collapse = ", "),
        if (length(skipped) > 10L) ", ..." else ""
      ),
      call. = FALSE
    )
  }
  invisible(paths[paths %in% stored])
}
