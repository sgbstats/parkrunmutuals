source("R/supabase-cache.R")

migrate_supabase_cache <- function(
  config = supabase_storage_config(),
  untrack = "--untrack" %in% commandArgs(trailingOnly = TRUE)
) {
  paths <- system2(
    "git", c("-c", "core.quotePath=false", "ls-files", "data", "all_results.RDa",
             "parkrunmutuals/all_results.RDa", "parkrunmutuals/distances.RDa"),
    stdout = TRUE
  )
  if (!is.null(attr(paths, "status"))) stop("Could not list tracked data")
  paths <- paths[grepl("\\.(csv|rds|RDa)$", paths)]
  stored <- cache_list("data", config, recursive = TRUE)
  for (path in paths) {
    key <- if (startsWith(path, "data/")) path else paste0("data/legacy/", path)
    if (!key %in% stored) {
      supabase_upload_object(path, key, config, upsert = FALSE)
    }
    downloaded <- tempfile()
    tryCatch(
      {
        supabase_download_object(key, downloaded, config)
        if (!identical(unname(tools::md5sum(path)), unname(tools::md5sum(downloaded)))) {
          stop("Migration verification failed for ", path, "; no files removed")
        }
      },
      finally = unlink(downloaded)
    )
    message("Verified: ", key)
  }
  # Only untrack after every object has been verified. Local files remain intact.
  if (untrack && length(paths)) {
    batches <- split(paths, ceiling(seq_along(paths) / 100L))
    for (batch in batches) {
      status <- system2("git", c("rm", "--cached", "--", shQuote(batch)))
      if (status != 0L) stop("Could not untrack migrated data")
    }
  }
  invisible(paths)
}

if (sys.nframe() == 0L) migrate_supabase_cache()
