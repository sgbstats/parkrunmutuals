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
  config = supabase_storage_config(), sleep = Sys.sleep
) {
  paths <- vapply(paths, cache_key, character(1), USE.NAMES = FALSE)
  if (length(paths) != length(urls) || anyDuplicated(paths)) {
    stop("Result paths and URLs must have matching, unique keys", call. = FALSE)
  }
  stored <- cache_list("data/results", config)
  for (index in which(!paths %in% stored)) {
    legacy_file <- file.exists(paths[index])
    result <- if (legacy_file) {
      list(results = utils::read.csv(paths[index]))
    } else {
      tryCatch(fetch(url = urls[index]), finally = sleep(23))
    }
    if (
      !is.list(result) || !is.data.frame(result$results) ||
        !all(c("pos", "parkrunner", "id", "time") %in% names(result$results)) ||
        nrow(result$results) == 0L
    ) {
      stop("Invalid results returned for ", urls[index], call. = FALSE)
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
  }
  invisible(paths)
}
