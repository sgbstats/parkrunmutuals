library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(stringr)

get_results = function(
  url,
  headers = c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.9",
    `Connection` = "keep-alive"
  )
) {
  response = GET(url, add_headers(.headers = headers), timeout(15))
  tryCatch(
    {
      if (status_code(response) == 408) {
        warning(sprintf(
          "Request failed [%d] for %s",
          status_code(response),
          url
        ))
      } else if (status_code(response) != 200) {
        stop(sprintf("Request failed [%d] for %s", status_code(response), url))
      }
      html <- content(response, as = "text", encoding = "UTF-8") |> read_html()
      tables <- html |> html_element("div.Results.Results")

      x = tables |>
        html_nodes("a") |>
        html_attr("href")

      x1 = x[grepl("parkrunner", x)]
      slash_pos = gregexpr("/", x1[1])[[1]][3]
      id = substr(x1, slash_pos + 1, str_length(x1) - 1) |>
        as.numeric()

      results <- tables |> html_table() |> dplyr::select(c(1, 2, 6))

      names(results) = c("pos", "parkrunner", "time")
      results = results |>
        mutate(
          parkrunner = str_extract(parkrunner, "^[^0-9]*") |> str_trim(),
          time = str_extract(time, "^[0-9:]+")
        ) |>
        drop_na(time) |>
        cbind(id)

      volunteers_p <- html |>
        html_nodes(
          xpath = "//p[contains(., 'We are very grateful to the volunteers who made this event happen:')]"
        )

      # Extract all hyperlinks from that paragraph
      volunteer_links <- volunteers_p |>
        html_nodes("a") |>
        html_attr("href")

      x2 = volunteer_links[grepl("parkrunner", volunteer_links)]
      slash_pos = gregexpr("/", x2[1])[[1]][3]
      vol_id = substr(x2, slash_pos + 1, str_length(x1) - 1) |>
        as.numeric()
      if (length(vol_id) <= 5) {
        stop("No volunteers found, event cancelled")
      }
      return(list(results = results, volunteers = vol_id))
    },
    error = function(e) {
      stop("Error: ", conditionMessage(e))
    },
    warning = function(e) {
      message(conditionMessage(e))
    }
  )
}
