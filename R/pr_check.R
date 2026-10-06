runner_id <- "1026950"
history <- parkrunfunctions::get_all_runs(id = runner_id)
Sys.sleep(23)

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
  result <- tryCatch(
    parkrunfunctions::get_result(url = runs$url[i]),
    error = function(e) {
      warning(conditionMessage(e))
      NULL
    }
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
  Sys.sleep(23)
}

volunteer_roles
