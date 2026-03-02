library(XML)
library(tidyverse)
library(RCurl)
library(geosphere)
library(data.table)
library(RJSONIO)
library(httr)
library(rvest)

parkrunsall = fromJSON("https://images.parkrun.com/events.json")


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

library(stringi)
short = stri_trans_general(short, "Latin-ASCII")
long = stri_trans_general(long, "Latin-ASCII")
name = stri_trans_general(name, "Latin-ASCII")

parkruns = cbind.data.frame(name, countrycode, coords, short, long) |>
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
        "Nobles",
        "Gibralter Botanical Gardens"
      )
  ) |>
  arrange(name) |>
  dplyr::select(-countrycode, -long)

parkruns_list = parkruns |> dplyr::select(name, short)

parkruns2 = cross_join(
  parkruns |> dplyr::select(-short),
  parkruns |> dplyr::select(-short)
)

dist = numeric(nrow(parkruns2))
for (i in 1:nrow(parkruns2)) {
  dist[i] = distm(
    c(parkruns2$lon.x[i], parkruns2$lat.x[i]),
    c(parkruns2$lon.y[i], parkruns2$lat.y[i]),
    fun = distHaversine
  ) /
    1000
  if (i %% 10000 == 0) {
    svMisc::progress(i, nrow(parkruns2))
  }
}

distance = cbind.data.frame(parkruns2, dist) |>
  select(name.x, name.y, dist) |>
  mutate(miles = dist / 1.6) |>
  merge(parkruns_list, by.x = "name.y", by.y = "name")


save(distance, parkruns_list, file = "parkrunmutuals/distances.RDa")
