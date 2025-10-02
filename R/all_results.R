library(tidyverse)


load("data/all_parkruns.RDa")

all_results=tribble(~"name", ~"event", ~"eventno",~"pos",~"parkrunner",~"time" )
folder="data/results/"
for(j in names(all_parkruns)){
  cat(crayon::blue(paste(j, "\n")))
  for(i in 1:nrow(all_parkruns[[j]][["results"]])){
    event=all_parkruns[[j]][["results"]][["event"]][i]
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

all_results %>% filter(name=="Michael PETER") %>% 
  summarise(people=n_distinct(parkrunner))


all_results %>% filter(name=="Michael PETER") %>% 
  count(parkrunner) %>% 
  arrange(-n) %>% 
  head(20)


all_results %>% filter(name=="Michael PETER") %>% 
  summarise(event=n_distinct(event), .by="parkrunner") %>% 
  arrange(-event) %>% 
  head(20)
