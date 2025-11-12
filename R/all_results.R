library(tidyverse)

load("data/all_parkruns.RDa")

all_results=tribble(~"name", ~"event", ~"eventno",~"pos",~"parkrunner",~"time", ~"short" )
folder="data/results/"
for(j in names(all_parkruns)){
  cat(crayon::blue(paste(j, "\n")))
  for(i in 1:nrow(all_parkruns[[j]][["results"]])){
    event=all_parkruns[[j]][["results"]][["event"]][i]
    short=all_parkruns[[j]][["results"]][["short"]][i]
    eventno=all_parkruns[[j]][["results"]][["run_number"]][i]
    file=paste0(folder,event,eventno, ".csv")
    if(file.exists(file)){
      x=read.csv(file) %>% 
        mutate(name=all_parkruns[[j]][["name"]],
               event=event,
               eventno=eventno)
    }
    all_results=all_results %>% rbind.data.frame(x)
  }
  
}

runners=unique(all_results$name)
parkruns <- sort(unique(all_results$event))
events_done=all_results %>% count(name, event) %>% dplyr::select(-n)
date=Sys.Date()
save(all_results,runners,parkruns,date,events_done, file="data/all_results.RDa")

googledrive::drive_auth(
  email = TRUE,
  path = NULL,
  subject = NULL,
  scopes = "drive",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

googledrive::drive_update(media="data/all_results.RDa",
                          file=googledrive::as_id("11d6wDY_ryjx5sxv5sVAS1x_wbquVQqLY"),)
# save(all_results,runners,parkruns, file="parkrunmutuals/all_results.RDa")
