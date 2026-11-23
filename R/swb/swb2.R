library(tidyverse)
library(parkrunfunctions)
library(glue)
source("R/supabase-cache.R")
cache_load("data/pr_done.RDa")

out <- tribble(
  ~"name" , ~"all" , ~"finishes60_150" , ~"finishes150"
)
for (i in pr_done) {
  cat(crayon::red(paste0(i, "\n")))
  df <- tryCatch(
    {
      cache_fetch_rds(
        sprintf("data/swb/results/%s-2026-06-13.rds", i),
        function() get_result(
          glue("https://www.parkrun.org.uk/{i}/results/2026-06-13/"),
          extra_data = TRUE
        )
      )[["results"]] |>
        summarise(
          all = n(),
          finishes60_150 = sum(finishes >= 60 & finishes < 150),
          finishes150 = sum(finishes >= 150)
        ) |>
        mutate(name = i)
    },
    error = function(e) stop("Results for ", i, ": ", conditionMessage(e))
  )
  out <- out |> rbind(df)
}

cache_save(out, file = "data/finishes.RDa")
