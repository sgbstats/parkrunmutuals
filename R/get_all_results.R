library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(stringr)


get_results=function(event, eventno, headers=c(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
                                               `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
                                               `Accept-Language` = "en-US,en;q=0.9",
                                               `Connection` = "keep-alive")){
  
  response=GET(paste0("https://www.parkrun.org.uk/",str_to_lower(str_remove_all(event, "\\s")),"/results/",eventno,"/"),
               add_headers(.headers = headers),
               timeout(15))
  tryCatch({
    if(status_code(response)!=200){
      stop(sprintf("Request failed [%d] for %s", code, url))
    }
    html <- content(response, as = "text", encoding = "UTF-8") %>% read_html()
    tables <- html %>% html_element("div.Results.Results")
    results=tables%>% html_table()
    
    results=results %>% mutate(parkrunner=str_extract(parkrunner, "^[^0-9]*") %>% str_trim(),
                               time=str_extract(Time, "^[0-9:]+")) %>% 
      dplyr::select("pos"=Position,
                    parkrunner,
                    time) %>% 
      drop_na(time) 
    
    return(results)
  },
  error=function(e){
    stop("❌ Error: ", conditionMessage(e))
    
  },
  warning=function(e){
    message(conditionMessage(e))
  }
  )
}


get_all_results=function(parkrunner,folder="data/results/",...){
  if(class(parkrunner)!="parkrun_results") break
  results=parkrunner[["results"]]
  for(i in 1:nrow(results)){
    event=results$event[i]
    eventno=results$run_number[i]
    
    file=paste0(folder,event,eventno, ".csv")
    
    if(file.exists(file)) next
    
    cat(paste(event,eventno, "\n"))
    
    x=get_results(event=event, eventno = eventno)
    
    write.csv(x, file)
    Sys.sleep(25)
  }
  return(0)
}

load("data/all_parkruns.RDa")
get_all_results(all_parkruns[["sebastianbate"]])
