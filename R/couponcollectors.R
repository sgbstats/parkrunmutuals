library(tidyverse)

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

dist <- c(
  sample(150:199, 584, replace = T),
  sample(200:249, 340, replace = T),
  sample(250:299, 231, replace = T),
  sample(300:349, 143, replace = T),
  sample(350:399, 85, replace = T),
  sample(400:449, 52, replace = T),
  sample(450:499, 58, replace = T),
  sample(500:599, 27, replace = T),
  sample(600:699, 6, replace = T),
  sample(700:799, 1, replace = T)
)

out <- tribble(
  ~"event" , ~"time"
)
for (i in 1:63831) {
  out <- out |>
    rbind.data.frame(
      coupon_event_time(sample(0:59, sample(dist, 1), replace = T))
    )
}

x <- survival::survfit(
  Surv(time, event) ~ 1,
  data = out # |> slice_sample(n = 5e3, replace = F)
)
x


load("data/swb_id.RDa")
fit <- survival::survfit(
  Surv(events, swb) ~ 1,
  data = id_done
)
fit
library(survival)
library(ggsurvfit)
library(ggplot2)

# use existing fit if present; otherwise recreate from `out`
if (!exists("fit")) {
  fit <- survfit(Surv(events, swb) ~ 1, data = id_done)
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
    title = "SWB (cumulative event probability)",
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


coxph(Surv(events, swb) ~ tq, data = id_done)
