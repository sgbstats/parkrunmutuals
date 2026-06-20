library(tidyverse)
library(parkrunfunctions)
library(glue)


coupon_event_time <- function(vec) {
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
  # track seen values with a logical vector of length 60
  seen <- rep(FALSE, 60L)
  for (i in seq_len(n)) {
    seen[vec[i] + 1L] <- TRUE
    if (all(seen)) return(data.frame("event" = 1L, "time" = i))
  }
  data.frame("event" = 0L, "time" = n)
}


parkrunsall = RJSONIO::fromJSON("https://images.parkrun.com/events.json")
nevents = length(parkrunsall$events$features)

short = character(nevents)
long = character(nevents)
location = character(nevents)
countrycode = numeric(nevents)
name = numeric(nevents)
coords = matrix(0, ncol = 2, nrow = nevents)


for (i in 1:nevents) {
  name[i] = parkrunsall$events$features[[i]]$properties$eventname
  short[i] = parkrunsall$events$features[[i]]$properties$EventShortName
  long[i] = parkrunsall$events$features[[i]]$properties$EventLongName
  countrycode[i] = parkrunsall$events$features[[i]]$properties$countrycode
  coords[i, ] = parkrunsall$events$features[[i]]$geometry$coordinates
  location[i] = parkrunsall$events$features[[i]]$properties$EventLocation
}


parkrunsuk = cbind.data.frame(
  short,
  long,
  countrycode,
  coords,
  location,
  name
) |>
  rename("lat" = "2", "lon" = "1") |>
  filter(countrycode == 97) |>
  filter(
    !grepl("junior", long),
    !short %in%
      c(
        "Cape Pembroke Lighthouse",
        "Jersey",
        "Guernsey",
        "Douglas",
        "Nobles"
      )
  ) |>
  # arrange(short) |>
  pull(name)


# pr_done = character(0)
# id_done = tribble(
#   ~"id" , ~"event" , ~"time"
# )

if (file.exists("data/pr_done.RDa")) {
  load("data/pr_done.RDa")
} else {
  pr_done = character(0)
}

if (file.exists("data/swb_id.RDa")) {
  load("data/swb_id.RDa")
} else {
  id_done = tribble(
    ~"id" , ~"events" , ~"swb" , ~"distinct_events" , ~"tq"
  )
}

error_counter <- 0

for (i in parkrunsuk) {
  crayon::red(cat(paste0(i, "\n")))
  if (i %in% pr_done) {
    next
  }

  tryCatch(
    {
      df <- get_result(
        glue("https://www.parkrun.org.uk/{i}/results/2026-06-13/"),
        extra_data = TRUE
      )[["results"]] |>
        filter(finishes >= 150)

      error_counter <- 0
    },
    error = function(e) {
      error_counter <<- error_counter + 1

      if (error_counter >= 10) {
        stop("10 consecutive errors — stopping execution")
      }

      next
    }
  )

  for (j in df$id) {
    if (j %in% id_done$id) {
      next
    }

    cat(paste0(j, "\n"))

    tryCatch(
      {
        x <- get_all_runs(j, as_Date = TRUE, as_hms = FALSE)

        y = x[["results"]] |>
          mutate(event_date = as.Date(event_date)) |>
          arrange(event_date) |>
          mutate(
            secs = as.numeric(substr(
              time,
              str_length(time) - 1,
              str_length(time)
            ))
          )

        swb = y |>
          pull(secs) |>
          coupon_event_time() |>
          rename("events" = "time", "swb" = "event")

        n_events = y |>
          head(swb$events) |>
          summarise(distinct_events = n_distinct(event), done = n()) |>
          mutate(tq = distinct_events / done) |>
          select(-done)
        id_done <- id_done |>
          rbind(
            cbind(
              tribble(
                ~id , j
              ),
              swb,
              n_events
            )
          )

        save(id_done, file = "data/swb_id.RDa")

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
  pr_done = c(pr_done, i)
  save(pr_done, file = "data/pr_done.RDa")
}
