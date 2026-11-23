expect_error <- function(expression, pattern) {
  error <- tryCatch({ force(expression); NULL }, error = identity)
  if (is.null(error)) stop("Expected error containing: ", pattern)
  if (!grepl(pattern, conditionMessage(error), fixed = TRUE)) {
    stop("Expected: ", pattern, "; received: ", conditionMessage(error))
  }
}

with_namespace_mocks <- function(package, replacements, code) {
  namespace <- asNamespace(package)
  originals <- mget(names(replacements), namespace, inherits = FALSE)
  replace <- function(values) {
    for (name in names(values)) {
      unlockBinding(name, namespace)
      assign(name, values[[name]], namespace)
      lockBinding(name, namespace)
    }
  }
  on.exit(replace(originals), add = TRUE)
  replace(replacements)
  eval(substitute(code), parent.frame())
}

with_fake_supabase <- function(code) {
  store <- new.env(parent = emptyenv())
  store$objects <- new.env(parent = emptyenv())
  store$requests <- list()
  store$fail <- list()
  store$corrupt <- FALSE
  store$empty <- FALSE
  config <- list(url = "https://example.supabase.co", api_key = "test-key", bucket = "test")
  settings <- c(
    SUPABASE_URL = config$url, SUPABASE_API_KEY = config$api_key,
    PARKRUNMUTUALS_SUPABASE_BUCKET = config$bucket
  )
  original_settings <- setNames(
    Sys.getenv(names(settings), unset = NA_character_), names(settings)
  )
  on.exit({
    Sys.unsetenv(names(settings))
    previous <- original_settings[!is.na(original_settings)]
    if (length(previous)) do.call(Sys.setenv, as.list(previous))
  }, add = TRUE)
  do.call(Sys.setenv, as.list(settings))

  response <- function(status = 200L, content = list()) {
    structure(
      list(
        status_code = as.integer(status),
        headers = list(`Content-Type` = "application/json"),
        content = charToRaw(as.character(jsonlite::toJSON(
          content, auto_unbox = TRUE, null = "null"
        ))),
        url = config$url
      ),
      class = "response"
    )
  }
  record <- function(method, url, args) {
    headers <- unlist(lapply(args, function(arg) {
      if (inherits(arg, "request")) arg$headers else NULL
    }))
    stopifnot(
      identical(unname(headers["apikey"]), config$api_key),
      identical(unname(headers["Authorization"]), paste("Bearer", config$api_key))
    )
    store$requests[[length(store$requests) + 1L]] <- list(
      method = method, url = url, headers = headers
    )
    utils::URLdecode(sub(
      paste0(config$url, "/storage/v1/object/", config$bucket, "/"),
      "", url, fixed = TRUE
    ))
  }
  verbs <- list(
    PUT = function(url, ..., body, encode) {
      key <- record("PUT", url, list(...))
      if (!is.null(store$fail$PUT)) return(response(store$fail$PUT, list(error = "upload failed")))
      headers <- store$requests[[length(store$requests)]]$headers
      if (exists(key, store$objects, inherits = FALSE) && headers["x-upsert"] == "false") {
        return(response(409, list(error = "already exists")))
      }
      stopifnot(is.raw(body), identical(encode, "raw"))
      store$objects[[key]] <- body
      response()
    },
    GET = function(url, ...) {
      args <- list(...)
      key <- record("GET", url, args)
      if (!is.null(store$fail$GET)) return(response(store$fail$GET, list(error = "download failed")))
      if (!exists(key, store$objects, inherits = FALSE)) {
        return(response(404, list(error = "not found")))
      }
      destination <- Filter(function(arg) {
        inherits(arg, "request") && !is.null(arg$output)
      }, args)[[1L]]$output$path
      bytes <- store$objects[[key]]
      if (store$corrupt) bytes <- charToRaw("corrupted")
      if (store$empty) bytes <- raw()
      writeBin(bytes, destination)
      response()
    },
    POST = function(url, ..., body, encode) {
      record("POST", url, list(...))
      if (!is.null(store$fail$POST)) return(response(store$fail$POST, list(error = "listing failed")))
      stopifnot(identical(encode, "json"))
      prefix <- paste0(body$prefix, "/")
      keys <- ls(store$objects, all.names = TRUE)
      keys <- keys[startsWith(keys, prefix)]
      relative <- substring(keys, nchar(prefix) + 1L)
      names <- sort(unique(sub("/.*", "", relative)))
      names <- names[grepl(body$search, names, fixed = TRUE)]
      page <- head(names[seq_along(names) > body$offset], body$limit)
      entries <- lapply(page, function(name) {
        list(name = name, id = if (name %in% relative) name else NULL)
      })
      response(content = entries)
    }
  )
  scope <- new.env(parent = parent.frame())
  scope$store <- store
  scope$config <- config
  with_namespace_mocks("httr", verbs, eval(substitute(code), scope))
}
