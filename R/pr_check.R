source("R/supabase-cache.R")
runner_id <- "1026950"
history <- cache_fetch_rds(
  sprintf("data/volunteer-check/runners/%s/%s.rds", runner_id, Sys.Date()),
  function() parkrunfunctions::get_all_runs(id = runner_id)
)

if (is.null(history)) {
  stop("Could not retrieve the runner's results")
}

runs <- history$results
volunteer_roles <- data.frame(
  event = character(),
  event_no = integer(),
  role = character()
)

for (i in seq_len(nrow(runs))) {
  svMisc::progress(i, nrow(runs), progress.bar = TRUE)
  result <- cache_fetch_rds(
    sprintf("data/volunteer-check/results/%s%s.rds", runs$event[i], runs$event_no[i]),
    function() parkrunfunctions::get_result(url = runs$url[i])
  )
  if (is.null(result)) {
    next
  }

  roles <- result$volunteers$role[
    !is.na(result$volunteers$id) &
      as.character(result$volunteers$id) == runner_id
  ]

  if (length(roles)) {
    volunteer_roles <- rbind(
      volunteer_roles,
      data.frame(
        event = rep(runs$event[i], length(roles)),
        event_no = rep(runs$event_no[i], length(roles)),
        role = roles
      )
    )
  }
}

volunteer_roles
