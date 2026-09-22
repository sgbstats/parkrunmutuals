source("R/supabase-storage.R")

expect_error <- function(expression, pattern) {
  error <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = identity
  )
  stopifnot(!is.null(error), grepl(pattern, conditionMessage(error), fixed = TRUE))
}

config <- supabase_storage_config(
  url = "https://example.supabase.co/",
  api_key = "test-key",
  bucket = "prmutuals"
)
stopifnot(
  identical(config$url, "https://example.supabase.co"),
  identical(config$bucket, "prmutuals"),
  identical(
    supabase_object_url(config, "parkrunmutuals/data.RDa"),
    "https://example.supabase.co/storage/v1/object/prmutuals/parkrunmutuals/data.RDa"
  )
)
expect_error(
  supabase_storage_config(url = "http://example.supabase.co", api_key = "test-key"),
  "SUPABASE_URL must be an HTTPS project URL"
)
expect_error(
  supabase_storage_config(url = "https://example.supabase.co", api_key = ""),
  "SUPABASE_API_KEY is not configured"
)
expect_error(supabase_object_url(config, "/data.RDa"), "relative, non-empty path")

all_results <- data.frame(name = "Runner")
runners <- "Runner"
parkruns <- "Event"
date <- as.Date("2026-09-21")
events_done <- data.frame(name = "Runner", event = "Event")
names_ids <- data.frame(name = "Runner", id = 1L)
names_all <- data.frame(name = "Runner", parkrunner = "Runner", id = 1L, n = 1L)
distance <- data.frame(name.x = "Event", name.y = "Event", dist = 0, miles = 0, short = "Event")
parkruns_list <- data.frame(name = "Event", short = "Event")

bundle_path <- tempfile(fileext = ".RDa")
save(
  all_results, runners, parkruns, date, events_done, names_ids, names_all,
  distance, parkruns_list, file = bundle_path
)
bundle <- load_parkrunmutuals_bundle(bundle_path)
stopifnot(identical(get("runners", envir = bundle), "Runner"))
unlink(bundle_path)

incomplete_bundle_path <- tempfile(fileext = ".RDa")
save(all_results, file = incomplete_bundle_path)
expect_error(
  load_parkrunmutuals_bundle(incomplete_bundle_path),
  "Data bundle is missing objects"
)
unlink(incomplete_bundle_path)
