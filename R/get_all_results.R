library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(stringr)

get_results=function(url, headers=c(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
                                               `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
                                               `Accept-Language` = "en-US,en;q=0.9",
                                               `Connection` = "keep-alive")){
  
  response=GET(url,
               add_headers(.headers = headers),
               timeout(15))
  tryCatch({

    if(status_code(response)!=200){
      
      message(sprintf("Request failed [%d] for %s", code, url))
    }
    html <- content(response, as = "text", encoding = "UTF-8") %>% read_html()
    tables <- html %>% html_element("div.Results.Results")
  
    results <- tables %>% html_table() %>% dplyr::select(c(1, 2, 6))

    names(results)=c("pos", "parkrunner", "time")
    results=results %>% mutate(parkrunner=str_extract(parkrunner, "^[^0-9]*") %>% str_trim(),
                               time=str_extract(time, "^[0-9:]+")) %>% 
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

get_all_results=function(parkrunner,folder="data/results/",log_file="error_log.txt",...){
  if(class(parkrunner)!="parkrun_results") break
  results=parkrunner[["results"]]
  for(i in 1:nrow(results)){
    event=results$event[i]
    eventno=results$run_number[i]
    url=results$url[i]
    errors=read.csv(log_file, header = T)
    if(event %in% errors$event) next
    file=paste0(folder,event,eventno, ".csv")
    
    if(file.exists(file)) next
    
    cat(paste(event,eventno, "\n"))
    tryCatch({
      x=get_results(url=url)
      
      write.csv(x, file, row.names = F)
      Sys.sleep(25)
    },
    error=function(e){
      message("❌ Error: ", conditionMessage(e))
      write(paste(event, eventno, sep = ","), file = log_file, append = TRUE)
      
    },
    warning=function(e){
      message(conditionMessage(e))
    }
    )
    
  }
  return(0)
}


load("data/all_parkruns.RDa")

for(j in names(all_parkruns)){
  cat(crayon::blue(paste(j, "\n")))
  get_all_results(all_parkruns[[j]])
}
# ls=list.files("data/results", full.names = T)
# for(i in ls){
#   read.csv(i) %>% 
#     dplyr::select(pos,parkrunner,time) %>% 
#     write.csv(i, row.names = F)
# }
