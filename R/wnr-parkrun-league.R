library(tidyverse)
library(parkrunfunctions)
library(stringr)
library(lubridate)
library(hms)


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
  "888332"  , #Colm
  "1166640" , #gary
  "81779"   , #jon
)

col = c(names(all_parkruns[["sebastianbate"]][["results"]]), "id", "name")
hc = data.frame(matrix(ncol = length(col), nrow = 0))
names(hc) = col

for (i in names(all_parkruns)) {
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
    time = as.hms(if_else(
      stringr::str_length(time) == 5,
      paste0("00:", time),
      time
    )),
    run_date = as.Date(run_date, format = "%d/%m/%Y")
  ) |>
  summarise(
    lymepark = min(time[
      run_date < as.Date("2026-01-01") &
        run_date >= as.Date("2025-07-01")
    ]),
    woodbank = min(time[
      run_date < as.Date("2026-02-01") &
        run_date >= as.Date("2025-07-01")
    ]),
    .by = c("name", "id")
  )
