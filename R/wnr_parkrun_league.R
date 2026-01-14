library(tidyverse)
library(hms)
library(glue)
library(stringr)
library(lubridate)
library(googlesheets4)
gs4_auth()
load("data/all_parkruns.RDa")
source("R/get_results.R")
ids = tribble(
  ~id       ,
  "42804"   , #paul M
  "9334474" , #Alex beresford
  "6391679" , #kirsty
  "7433025" , #Helen
  "4301378" , #Lindsay
  "4051781" , #Callum
  "16568"   , #Paul C
  "16569"   , #Lynda
  "3629365" , #Isabel
  "1166640" , #gary
  "81779"   , #jon
  '493595'  , # Seb
)


results <- all_parkruns |>
  keep(~ .$id %in% ids$id) |>
  # Iterate over the filtered list. For each element...
  map_dfr(
    ~ {
      .x$results |>
        mutate(athlete_name = .x$name, athlete_id = .x$id)
    }
  ) |>
  mutate(
    time = as_hms(if_else(
      str_length(time) == 5,
      paste0("0:", time),
      time
    )) |>
      format("%H:%M:%S") |>
      as_hms(),
    run_date = as.Date(run_date, format = "%d/%m/%Y")
  ) |>
  select(
    athlete_name,
    athlete_id,
    short,
    run_date,
    time,
    "event_no" = run_number
  )

handicap = results |>
  summarise(
    lymepark = min(time[
      run_date >= as.Date("2025-07-01") & run_date <= as.Date("2025-12-31")
    ]),
    wilmslow = min(time[
      run_date >= as.Date("2025-07-01") & run_date <= as.Date("2026-01-31")
    ]),
    salewater = min(time[
      run_date >= as.Date("2025-07-01") & run_date <= as.Date("2026-02-28")
    ]),
    woodbank = min(time[
      run_date >= as.Date("2025-07-01") & run_date <= as.Date("2026-03-31")
    ]),
    wythenshawe = min(time[
      run_date >= as.Date("2025-07-01") & run_date <= as.Date("2026-04-30")
    ]),
    kingswaysurmston = min(time[
      run_date >= as.Date("2025-07-01") & run_date <= as.Date("2026-05-31")
    ]),
    .by = athlete_name
  ) |>
  pivot_longer(
    cols = -athlete_name,
    names_to = "month",
    values_to = "handicap"
  ) |>
  mutate(handicap = as_hms(handicap))

included_pr = tribble(
  ~"event"    , ~"event_no" , ~"short"   ,
  "Lyme Park" ,         535 , "lymepark" ,
  "Lyme Park" ,         536 , "lymepark" ,
  "Lyme Park" ,         537 , "lymepark"
)

results_filtered = results |>
  merge(included_pr)
# volunteers = tribble(
#   ~"event" , ~"event_no" , ~"barcode" , ~"short"
# )
load("data/wnr_volunteers.Rda")
included_pr = included_pr |>
  setdiff(volunteers |> distinct(event, event_no, short))
if (nrow(included_pr) > 0) {
  for (j in 1:nrow(included_pr)) {
    tryCatch(
      {
        x = get_results(
          url = glue(
            "https://www.parkrun.org.uk/{included_pr$short[j]}/results/{included_pr$event_no[j]}/"
          )
        )[["volunteers"]]
        if (!is.null(x)) {
          volunteers = volunteers |>
            rbind.data.frame(data.frame(
              barcode = x,
              event = included_pr$event[j],
              event_no = included_pr$event_no[j],
              short = included_pr$short[j]
            ))
        }
      },
      error = function(e) {
        message(conditionMessage(e))
      },
      warning = function(e) {
        message(conditionMessage(e))
      }
    )
    Sys.sleep(23)
  }
}
volunteers = volunteers |>
  distinct(event, event_no, barcode, .keep_all = T) |>
  filter(barcode %in% ids$id)
save(volunteers, file = "data/wnr_volunteers.Rda")


volunteers_pts =
  results_filtered |>
  merge(
    volunteers |> mutate(volunteer = T) |> rename("athlete_id" = "barcode"),
    all = T
  ) |>
  mutate(
    volunteer = case_when(volunteer & is.na(time) ~ 3, volunteer ~ 1, TRUE ~ 0)
  ) |>
  summarise(volunteer_pts = max(volunteer), .by = c(athlete_name, short)) |>
  rename("month" = short)


results_handicap = results_filtered |>
  select(athlete_name, time, "month" = short) |>
  merge(handicap, .by = c("athlete_name", "month")) |>
  mutate(
    diff_secs = as.numeric(time - handicap, units = "secs"),
    diff2 = paste0(
      ifelse(diff_secs < 0, "-", ""),
      floor(abs(diff_secs) / 60),
      ":",
      sprintf("%02d", floor(abs(diff_secs)) %% 60)
    )
  ) |>
  slice_min(diff_secs, by = c(athlete_name, month), with_ties = F) |>
  mutate(pts = 13 - rank(diff_secs), .by = c(month)) |>
  merge(volunteers_pts, .by = c("athlete_name", "month"))


total = results_handicap |>
  mutate(pts = pts + volunteer_pts) |>
  summarise(total_points = sum(pts), .by = athlete_name)

handicap_wide = handicap |>
  mutate(handicap = format(handicap, "%H:%M:%S")) |>
  pivot_wider(
    names_from = "month",
    values_from = "handicap",
    names_prefix = "hc_"
  )

times_wide =
  results_filtered |>
  select(month = short, athlete_name, time) |>
  mutate(time = format(time, "%H:%M:%S")) |>
  pivot_wider(
    names_from = "month",
    values_from = "time",
    names_prefix = "time_"
  )

diff = results_handicap |>
  select(month, athlete_name, diff2) |>
  pivot_wider(
    names_from = "month",
    values_from = "diff2",
    names_prefix = "diff_"
  )

pts = results_handicap |>
  select(month, athlete_name, pts) |>
  pivot_wider(names_from = "month", values_from = "pts", names_prefix = "pts_")

vol_pts = results_handicap |>
  select(month, athlete_name, volunteer_pts) |>
  pivot_wider(
    names_from = "month",
    values_from = "volunteer_pts",
    names_prefix = "vol_"
  )

output =
  results |>
  count(athlete_name) |>
  select(-n) |>
  merge(total, .by = "athlete_name", all = T) |>
  merge(handicap_wide, .by = "athlete_name", all = T) |>
  merge(times_wide, .by = "athlete_name", all = T) |>
  merge(diff, .by = "athlete_name", all = T) |>
  merge(pts, .by = "athlete_name", all = T) |>
  merge(vol_pts, .by = "athlete_name", all = T) |>
  select(any_of(c(
    "athlete_name",
    "total_points",
    c(outer(
      c("hc", "time", "diff", "pts", "vol"),
      c(
        "lymepark",
        "wilmslow",
        "salewater",
        "woodbank",
        "wythenshawe",
        "kingswaysurmston"
      ),
      FUN = paste,
      sep = "_"
    ))
  ))) |>
  select(
    -contains(c(
      "wilmslow",
      "salewater",
      "woodbank",
      "wythenshawe",
      "kingswaysurmston"
    ))
  ) |>
  arrange(-total_points)

googlesheets4::sheet_write(
  output,
  ss = "https://docs.google.com/spreadsheets/d/1tKqy3scDIttZti9yAMZbAtMcukINser_KFlXEYr-Sl8/edit?gid=0#gid=0",
  sheet = "Scores"
)
