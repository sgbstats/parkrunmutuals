library(tidyverse)
library(parkrunfunctions)
library(glue)
load("data/pr_done.RDa")

out = tribble(
  ~"name" , ~"all" , ~"finishes60_150" , ~"finishes150"
)
for (i in pr_done) {
  cat(crayon::red(paste0(i, "\n")))
  df <- tryCatch(
    {
      get_result(
        glue("https://www.parkrun.org.uk/{i}/results/2026-06-13/"),
        extra_data = TRUE
      )[["results"]] |>
        summarise(
          all = n(),
          finishes60_150 = sum(finishes >= 60 & finishes < 150),
          finishes150 = sum(finishes >= 150)
        ) |>
        mutate(name = i)
    },
    error = function(e) {
      data.frame(name = i, all = 0, finishes60_150 = 0, finishes150 = 0)
    }
  )
  out = out |> rbind(df)
  Sys.sleep(23)
}

save("finsiher_no" = out, file = "data/finishes.RDa")
