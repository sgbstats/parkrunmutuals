library(tidyverse)
library(parkrunfunctions)
library(hms)
library(googlesheets4)
source("R/wnr-league-helpers.R")
load("data/all_parkruns.RDa")

# Change this mapping and the year when configuring a new league season.
league_year <- 2026L
league_events <- c(
  july = "peel",
  august = "penningtonflash",
  september = "alexandra",
  october = "fletchermoss",
  november = "wythenshawe",
  december = "southmanchester"
)
month_numbers <- match(names(league_events), tolower(month.name))
if (
  anyNA(month_numbers) ||
    anyDuplicated(month_numbers) ||
    anyDuplicated(league_events)
) {
  stop("League months and parkruns must be unique and valid")
}
today <- Sys.Date()
active <- month_numbers <= as.integer(format(today, "%m")) &
  league_year <= as.integer(format(today, "%Y"))
if (league_year < as.integer(format(today, "%Y"))) {
  active[] <- TRUE
}
active[league_year > as.integer(format(today, "%Y"))] <- FALSE
active_events <- league_events[active]
active_months <- month_numbers[active]

ids <- c(
  "493595",
  "42804",
  "9334474",
  "6391679",
  "7433025",
  "4301378",
  "4051781",
  "16568",
  "16569",
  "3629365",
  "4228499",
  "10000894",
  "1166640",
  "81779",
  "8326421"
)

hc <- purrr::map_dfr(
  all_parkruns[names(all_parkruns) != "names_ids"],
  function(runner) {
    if (!as.character(runner$id) %in% ids) {
      return(NULL)
    }
    runner$results |>
      mutate(id = as.character(runner$id), name = runner$name)
  }
) |>
  mutate(
    time = as_hms(if_else(nchar(time) == 5L, paste0("00:", time), time)),
    event_date = as.Date(event_date, format = "%d/%m/%Y")
  )
runners <- hc |> distinct(name, id)

hc2 <- purrr::map2_dfr(active_events, active_months, function(event, month) {
  start <- handicap_start(league_year, month)
  end <- as.Date(sprintf("%04d-%02d-01", league_year, month))
  hc |>
    filter(event_date >= start, event_date < end, !is.na(time)) |>
    summarise(hc = min(time), .by = c("name", "id")) |>
    mutate(event = event)
})

cache_dir <- file.path("data", "wnr")
entries <- purrr::map2(active_events, active_months, function(event, month) {
  dates <- league_dates(league_year, month, today)
  fetch_league_month(event, dates, cache_dir)
})
names(entries) <- active_events

res <- purrr::imap_dfr(entries, function(month_entries, event) {
  purrr::map_dfr(month_entries, function(entry) {
    entry$result$results |>
      mutate(
        id = as.character(id),
        time = as_hms(if_else(
          nchar(as.character(time)) == 5L,
          paste0("00:", time),
          as.character(time)
        )),
        event = event,
        event_date = entry$date
      )
  })
})
volunteers <- purrr::imap_dfr(entries, function(month_entries, event) {
  purrr::map_dfr(month_entries, function(entry) {
    entry$result$volunteers |>
      transmute(id = as.character(id), event = event, event_date = entry$date)
  })
})

if (nrow(res) == 0L) {
  res <- tibble(
    id = character(),
    event = character(),
    event_date = as.Date(character()),
    time = as_hms(character())
  )
}
if (nrow(volunteers) == 0L) {
  volunteers <- tibble(
    id = character(),
    event = character(),
    event_date = as.Date(character())
  )
}

eligible_results <- res |> filter(id %in% ids)
vol_pts <- volunteers |>
  filter(id %in% ids) |>
  left_join(
    eligible_results |> distinct(id, event, event_date) |> mutate(ran = TRUE),
    by = c("id", "event", "event_date")
  ) |>
  mutate(pts = if_else(is.na(ran), 3, 1)) |>
  summarise(pts = max(pts), .by = c("id", "event"))

# Retain the existing points ranking, but only compare runners with a handicap.
time_diff <- eligible_results |>
  inner_join(hc2 |> select(id, event, hc), by = c("id", "event")) |>
  mutate(diff = time - hc) |>
  slice_min(diff, by = c("id", "event"), with_ties = FALSE) |>
  arrange(event, diff) |>
  mutate(pts = pmax(11 - row_number(), 3), .by = event) |>
  mutate(
    diff = as.character(as_hms(diff)),
    time = as.character(as_hms(time))
  ) |>
  select(id, event, diff, time, pts)

points <- bind_rows(vol_pts, time_diff |> select(id, event, pts)) |>
  summarise(total_pts = sum(pts), .by = id)
out <- runners |>
  left_join(points, by = "id") |>
  mutate(total_pts = replace_na(total_pts, 0))

for (field in c("hc", "time", "diff", "pts", "vol")) {
  values <- switch(
    field,
    hc = hc2 |> transmute(id, event, value = as.character(as_hms(hc))),
    time = time_diff |> select(id, event, value = time),
    diff = time_diff |> select(id, event, value = diff),
    pts = time_diff |> select(id, event, value = pts),
    vol = vol_pts |> select(id, event, value = pts)
  )
  if (nrow(values)) {
    out <- out |>
      left_join(
        values |>
          pivot_wider(
            names_from = event,
            values_from = value,
            names_prefix = paste0(field, "_")
          ),
        by = "id"
      )
  }
  for (event in active_events) {
    column <- paste(field, event, sep = "_")
    if (!column %in% names(out)) {
      out[[column]] <- if (field %in% c("pts", "vol")) {
        NA_real_
      } else {
        NA_character_
      }
    }
  }
}

wanted <- as.vector(outer(
  c("hc", "time", "diff", "pts", "vol"),
  active_events,
  paste,
  sep = "_"
))
out <- out |>
  select(name, id, total_pts, any_of(wanted)) |>
  arrange(desc(total_pts))

gs4_auth(path = "credentials.json")
write_sheet(
  out,
  ss = "https://docs.google.com/spreadsheets/d/1mOPeM1BA2i5gw7tNMn7geTt_yLC4ngjFw2uh6zWl4wE/edit?usp=sharing",
  sheet = "Scores"
)
