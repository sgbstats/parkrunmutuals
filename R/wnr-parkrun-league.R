library(tidyverse)
library(parkrunfunctions)
library(stringr)
library(lubridate)
library(hms)
library(glue)
library(googlesheets4)
load("data/all_parkruns.RDa")

ids = tribble(
  ~id       ,

  '493595'  , # Seb

  "42804"   , #paul M
  "9334474" , #Alex behcford
  "6391679" , #kirsty
  "7433025" , #Helen
  "4301378" , #Lindsay
  "4051781" , #Callum
  "16568"   , #Paul C
  "16569"   , #Lynda
  "3629365" , #Isabel

  "1166640" , #gary
  "81779"   , #jon
)

col = c(names(all_parkruns[["sebastianbate"]][["results"]]), "id", "name")
hc = data.frame(matrix(ncol = length(col), nrow = 0))
names(hc) = col


for (i in names(all_parkruns)[names(all_parkruns) != "names_ids"]) {
  if (all_parkruns[[i]][["id"]] %in% ids$id) {
    hc = hc |>
      rbind.data.frame(
        all_parkruns[[i]][["results"]] |>
          mutate(
            id = all_parkruns[[i]][["id"]],
            name = all_parkruns[[i]][["name"]]
          )
      )
  }
}

hc2 = hc |>
  mutate(
    time = as_hms(if_else(
      stringr::str_length(time) == 5,
      paste0("00:", time),
      time
    )),
    event_date = as.Date(event_date, format = "%d/%m/%Y")
  ) |>
  summarise(
    lymepark = min(time[
      event_date < as.Date("2026-01-01") &
        event_date >= as.Date("2025-07-01")
    ]),
    wilmslow = min(time[
      event_date < as.Date("2026-02-01") &
        event_date >= as.Date("2025-07-01")
    ]),
    salewater = min(time[
      event_date < as.Date("2026-03-01") &
        event_date >= as.Date("2025-07-01")
    ]),
    woodbank = min(time[
      event_date < as.Date("2026-04-01") &
        event_date >= as.Date("2025-07-01")
    ]),
    wythenshawe = min(time[
      event_date < as.Date("2026-05-01") &
        event_date >= as.Date("2025-07-01")
    ]),
    kingswayurmston = min(time[
      event_date < as.Date("2026-06-01") &
        event_date >= as.Date("2025-07-01")
    ]),
    .by = c("name", "id")
  ) |>
  pivot_longer(cols = -c(name, id), values_to = "hc", names_to = "event") |>
  mutate(
    hc = as_hms(hc),
  )

events = list(
  "lymepark" = 534:538,
  "wilmslow" = 496:499,
  "salewater" = 350:353,
  "woodbank" = 779:782,
"wythenshawe"= 664:668
)
runners = hc2 |> distinct(name, id)

res = tribble(
  ~"pos" , ~"parkrunner" , ~"time" , ~"ag" , ~"id" , ~"event" , ~"event_no"
)
volunteers = tribble(
  ~"parkrunner" , ~"id" , ~"event" , ~"event_no"
)
ls = list.files("data/wnr", full.names = F)

for (i in names(events)) {
  for (j in events[[i]]) {
    cat(paste(i, j, "\n"))
    tryCatch(
      {
        if (glue("{i}{j}.RDa") %in% ls) {
          load(glue("data/wnr/{i}{j}.RDa"))
          assign("x", get(glue("{i}{j}")))
        } else {
          x = get_result(event = i, event_no = j, as_hms = T, as_Date = T)
          assign(glue("{i}{j}"), x)
          save(list = glue("{i}{j}"), file = glue("data/wnr/{i}{j}.RDa"))
          Sys.sleep(20)
        }
        res = res |>
          rbind.data.frame(
            x[["results"]] |>
              mutate(event = i, event_no = j)
          )

        volunteers = volunteers |>
          rbind.data.frame(
            x[["volunteers"]] |>
              select(id, "parkrunner") |>
              mutate(event = i, event_no = j)
          )
      },
      error = function(e) {
        warning(conditionMessage(e))
      },
      warning = function(e) {
        warning(conditionMessage(e))
      }
    )
  }
}
eligible_results = runners |>
  merge(
    res |>
      select(id, time, event, event_no),
    all.x = T
  )
vol_pts = volunteers |>
  filter(id %in% ids$id) |>
  select(id, event, event_no) |>
  merge(eligible_results |> select(id, time, event, event_no), all.x = T) |>
  mutate(pts = if_else(is.na(time), 3, 1)) |>
  summarise(pts = max(pts, na.rm = T), .by = c("id", "event"))


time_diff = eligible_results |>
  merge(
    hc2,
    all.x = T
  ) |>
  mutate(diff = time - hc) |>
  slice_min(diff, by = c("id", "event"), with_ties = F) |>
  select(id, event, diff, time) |>
  arrange(diff) |>
  mutate(pts = if_else(is.na(diff), 0, 13 - row_number()), .by = c("event")) |>
  mutate(diff = as.character(as_hms(diff)), time = as.character(as_hms(time)))

points = vol_pts |>
  rbind(time_diff |> select(id, event, pts)) |>
  summarise(total_pts = sum(pts), .by = "id")

out = runners |>
  merge(points) |>
  merge(
    hc2 |>
      mutate(hc = as.character(hc)) |>
      pivot_wider(
        names_from = "event",
        values_from = "hc",
        names_prefix = "hc_"
      ),
    all.x = T
  ) |>
  merge(
    time_diff |>
      select(id, event, diff) |>
      drop_na(event) |>
      pivot_wider(
        names_from = "event",
        values_from = "diff",
        names_prefix = "diff_"
      ),
    all.x = T
  ) |>
  merge(
    time_diff |>
      select(id, event, time) |>
      drop_na(event) |>
      pivot_wider(
        names_from = "event",
        values_from = "time",
        names_prefix = "time_"
      ),
    all.x = T
  ) |>
  merge(
    time_diff |>
      select(id, event, pts) |>
      drop_na(event) |>
      pivot_wider(
        names_from = "event",
        values_from = "pts",
        names_prefix = "pts_"
      ),
    all.x = T
  ) |>
  merge(
    vol_pts |>
      drop_na(event) |>
      pivot_wider(
        names_from = "event",
        values_from = "pts",
        names_prefix = "vol_"
      ),
    all.x = T
  ) |>
  select(
    name,
    id,
    total_pts,
    any_of(as.vector(outer(
      c("hc", "time", "diff", "pts", "vol"),
      c(
        "lymepark",
        "wilmslow",
        "salewater",
        "woodbank",
        "wythenshawe",
        "kingswayurmston"
      ),
      paste,
      sep = "_"
    )))
  ) |>
  select(
    name,
    id,
    total_pts,
    contains(c("lymepark", "wilmslow", "salewater", "woodbank", "wythenshawe"))
  ) |>
  arrange(-total_pts)


gs4_auth(
  path = "credentials.json",
)
write_sheet(
  out,
  ss = "https://docs.google.com/spreadsheets/d/1tKqy3scDIttZti9yAMZbAtMcukINser_KFlXEYr-Sl8/edit?gid=0#gid=0",
  sheet = "Scores"
)
