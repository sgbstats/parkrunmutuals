load("data/swb_id.RDa")
library(tidyverse)
library(parkrunfunctions)
library(glue)

coupon_event_time_double <- function(vec) {
  # validate
  if (!is.numeric(vec)) {
    stop("vec must be numeric")
  }
  n <- length(vec)
  if (n == 0L) {
    return(data.frame("event" = 1L, "time" = 0))
  }
  if (any(is.na(vec))) {
    stop("vec must not contain NA")
  }
  if (any(vec %% 1 != 0)) {
    stop("vec must contain integers")
  }
  if (any(vec < 0 | vec > 59)) {
    stop("values must be in 0..59")
  }
  # track counts for values 0..59
  seen <- integer(60L)
  for (i in seq_len(n)) {
    seen[vec[i] + 1L] <- seen[vec[i] + 1L] + 1L
    if (all(seen >= 2L)) return(data.frame("event" = 1L, "time" = i))
  }
  data.frame("event" = 0L, "time" = n)
}

# id_done2 = tribble(
#   ~"id" , ~"event" , ~"time"
# )
load("data/swb_id2.RDa")
df <- id_done |> filter(swb == 1)

for (j in df$id) {
  if (j %in% id_done2$id) {
    next
  }

  cat(paste0(j, "\n"))

  tryCatch(
    {
      x <- get_all_runs(j, as_Date = TRUE, as_hms = FALSE)

      y <- x[["results"]] |>
        mutate(event_date = as.Date(event_date)) |>
        arrange(event_date) |>
        mutate(
          secs = as.numeric(substr(
            time,
            str_length(time) - 1,
            str_length(time)
          ))
        )

      swb <- y |>
        pull(secs) |>
        coupon_event_time_double() |>
        rename("events" = "time", "swb" = "event")

      n_events <- y |>
        head(swb$events) |>
        summarise(distinct_events = n_distinct(event), done = n()) |>
        mutate(tq = distinct_events / done) |>
        select(-done)
      id_done2 <- id_done2 |>
        rbind(
          cbind(
            tribble(
              ~id , j
            ),
            swb,
            n_events
          )
        )

      save(id_done2, file = "data/swb_id2.RDa")

      Sys.sleep(23)

      error_counter <- 0
    },
    error = function(e) {
      error_counter <<- error_counter + 1

      if (error_counter >= 10) {
        cat(paste0("Error for ID: ", j, " - ", conditionMessage(e), "\n"))
        stop("10 consecutive errors — stopping execution")
      }
    }
  )
}
