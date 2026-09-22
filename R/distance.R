library(tidyverse)
library(geosphere)
library(RJSONIO)
library(stringi)

build_distance_data <- function() {
  parkrunsall <- fromJSON("https://images.parkrun.com/events.json")
  features <- parkrunsall$events$features

  parkruns <- purrr::map_dfr(features, function(feature) {
    tibble(
      name = feature$properties$eventname,
      countrycode = feature$properties$countrycode,
      lon = feature$geometry$coordinates[[1]],
      lat = feature$geometry$coordinates[[2]],
      short = feature$properties$EventShortName,
      long = feature$properties$EventLongName
    )
  }) |>
    mutate(
      across(c(name, short, long), stri_trans_general, id = "Latin-ASCII")
    ) |>
    filter(
      countrycode == 97,
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
    select(name, lat, lon, short)

  parkruns_list <- select(parkruns, name, short)
  distance <- cross_join(select(parkruns, -short), select(parkruns, -short)) |>
    transmute(
      name.x,
      name.y,
      dist = geosphere::distHaversine(
        cbind(lon.x, lat.x),
        cbind(lon.y, lat.y)
      ) /
        1000,
      miles = dist / 1.6
    ) |>
    left_join(parkruns_list, by = c("name.y" = "name"))

  list(distance = distance, parkruns_list = parkruns_list)
}
