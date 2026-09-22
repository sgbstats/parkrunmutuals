supabase_storage_config <- function(
  url = Sys.getenv(
    "SUPABASE_URL",
    "https://uapfyikxspmcifgdergl.supabase.co"
  ),
  api_key = Sys.getenv("SUPABASE_API_KEY", ""),
  bucket = Sys.getenv("PARKRUNMUTUALS_SUPABASE_BUCKET", "prmutuals")
) {
  url <- sub("/+$", "", url)
  if (!grepl("^https://[^/]+$", url)) {
    stop("SUPABASE_URL must be an HTTPS project URL", call. = FALSE)
  }
  if (!nzchar(api_key)) {
    stop("SUPABASE_API_KEY is not configured", call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9_-]+$", bucket)) {
    stop(
      "PARKRUNMUTUALS_SUPABASE_BUCKET contains invalid characters",
      call. = FALSE
    )
  }

  list(url = url, api_key = api_key, bucket = bucket)
}

supabase_object_url <- function(config, object_path) {
  if (!nzchar(object_path) || grepl("^/|//", object_path)) {
    stop("object_path must be a relative, non-empty path", call. = FALSE)
  }

  encoded_path <- vapply(
    strsplit(object_path, "/", fixed = TRUE)[[1]],
    utils::URLencode,
    character(1),
    reserved = TRUE
  )
  paste0(
    config$url,
    "/storage/v1/object/",
    utils::URLencode(config$bucket, reserved = TRUE),
    "/",
    paste(encoded_path, collapse = "/")
  )
}

supabase_storage_headers <- function(config) {
  c(
    Authorization = paste("Bearer", config$api_key),
    apikey = config$api_key
  )
}

supabase_require_success <- function(response, action) {
  if (httr::http_error(response)) {
    stop(
      sprintf(
        "Supabase Storage %s failed (HTTP %s): %s",
        action,
        httr::status_code(response),
        httr::content(response, as = "text", encoding = "UTF-8")
      ),
      call. = FALSE
    )
  }
  invisible(response)
}

supabase_upload_object <- function(
  local_path,
  object_path = "parkrunmutuals/data.RDa",
  config = supabase_storage_config(),
  timeout_seconds = 120
) {
  if (!file.exists(local_path)) {
    stop("Local upload file does not exist: ", local_path, call. = FALSE)
  }

  bytes <- readBin(
    local_path,
    what = "raw",
    n = file.info(local_path)$size[[1]]
  )
  response <- httr::PUT(
    supabase_object_url(config, object_path),
    httr::add_headers(
      .headers = c(
        supabase_storage_headers(config),
        `Content-Type` = "application/octet-stream",
        `x-upsert` = "true"
      )
    ),
    body = bytes,
    encode = "raw",
    httr::timeout(timeout_seconds)
  )
  supabase_require_success(response, "upload")
  invisible(list(
    object_path = object_path,
    bytes = unname(file.info(local_path)$size[[1]]),
    status_code = httr::status_code(response)
  ))
}

supabase_download_object <- function(
  object_path = "parkrunmutuals/data.RDa",
  destination,
  config = supabase_storage_config(),
  timeout_seconds = 120
) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  response <- httr::GET(
    supabase_object_url(config, object_path),
    httr::add_headers(.headers = supabase_storage_headers(config)),
    httr::write_disk(destination, overwrite = TRUE),
    httr::timeout(timeout_seconds)
  )
  supabase_require_success(response, "download")
  if (!file.exists(destination) || file.info(destination)$size[[1]] == 0) {
    stop("Supabase Storage download produced an empty file", call. = FALSE)
  }
  invisible(list(
    object_path = object_path,
    bytes = unname(file.info(destination)$size[[1]]),
    status_code = httr::status_code(response)
  ))
}

parkrunmutuals_required_objects <- function() {
  c(
    "all_results",
    "runners",
    "parkruns",
    "date",
    "events_done",
    "names_ids",
    "names_all",
    "distance",
    "parkruns_list"
  )
}

load_parkrunmutuals_bundle <- function(path) {
  bundle <- new.env(parent = emptyenv())
  load(path, envir = bundle)
  required_objects <- parkrunmutuals_required_objects()
  missing_objects <- required_objects[
    !vapply(
      required_objects,
      exists,
      logical(1),
      envir = bundle,
      inherits = FALSE
    )
  ]
  if (length(missing_objects) > 0) {
    stop(
      "Data bundle is missing objects: ",
      paste(missing_objects, collapse = ", "),
      call. = FALSE
    )
  }

  data_objects <- c(
    "all_results",
    "events_done",
    "names_ids",
    "names_all",
    "distance",
    "parkruns_list"
  )
  invalid_objects <- data_objects[
    !vapply(
      data_objects,
      function(name) {
        inherits(get(name, envir = bundle, inherits = FALSE), "data.frame")
      },
      logical(1)
    )
  ]
  if (length(invalid_objects) > 0) {
    stop(
      "Data bundle has invalid data-frame objects: ",
      paste(invalid_objects, collapse = ", "),
      call. = FALSE
    )
  }

  bundle
}

verify_parkrunmutuals_bundle <- function(
  object_path = "parkrunmutuals/data.RDa",
  config = supabase_storage_config()
) {
  destination <- tempfile(fileext = ".RDa")
  on.exit(unlink(destination), add = TRUE)
  supabase_download_object(object_path, destination, config = config)
  load_parkrunmutuals_bundle(destination)
  invisible(TRUE)
}
