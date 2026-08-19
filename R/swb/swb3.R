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


fit <- survival::survfit(
  Surv(events, swb) ~ 1,
  data = id_done2
)
fit
library(survival)
library(ggsurvfit)
library(ggplot2)

# use existing fit if present; otherwise recreate from `out`
if (!exists("fit")) {
  fit <- survfit(Surv(events, swb) ~ 1, data = id_done2)
}
# compute median time (first time survival <= 0.5); handle "median not reached"
med_time <- {
  idx <- which(fit$surv <= 0.5)
  if (length(idx)) fit$time[min(idx)] else NA_real_
}

# produce the KM plot (returns a ggplot object)

# build step data from the survfit
s <- summary(fit)
df_fit <- tibble(time = c(0, s$time), surv = c(1, s$surv)) |>
  mutate(event = 1 - surv)

# plot upward curve
p_up <- ggplot(df_fit, aes(x = time, y = event)) +
  geom_step() +
  labs(
    title = "Double SWB (cumulative event probability)",
    x = "Events",
    y = "Cumulative probability"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)

# add median lines/label (same med_time as before)
if (!is.na(med_time)) {
  p_up <- p_up +
    ggplot2::geom_vline(
      xintercept = med_time,
      linetype = "dashed",
      color = "red",
      linewidth = 0.6
    ) +
    ggplot2::geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      color = "red",
      linewidth = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x = med_time,
      y = 0.52,
      label = paste0("median = ", signif(med_time, 3)),
      hjust = 0,
      vjust = 0,
      color = "red",
      size = 3
    )
} else {
  p_up <- p_up + ggplot2::labs(subtitle = "Median not reached (survival > 0.5)")
}


p_up


coxph(Surv(events, swb) ~ tq, data = id_done2)


id3 <- merge(
  id_done |> filter(swb == 1) |> select(id, events),
  id_done2 |> select(id, swb, events),
  by = "id",
  all.x = TRUE
) |>
  mutate(net = events.y - events.x)

id3 |>
  filter(swb == 1) |>
  ggplot(aes(x = events.x, y = events.y)) +
  geom_point() +
  labs(x = "Time to SWB", y = "Time to Double SWB")
