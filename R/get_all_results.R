library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(stringr)


get_all_results=function(event, eventno, headers=c("User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/135.0.0.0 Safari/537.36")){
  
  response=GET(paste0("https://www.parkrun.org.uk/",event,"/results/",eventno,"/"),
               add_headers(.headers = headers))
  tryCatch({
    if(status_code(response)!=200){
      stop(sprintf("Request failed [%d] for %s", code, url))
    }
    html <- content(response, as = "text", encoding = "UTF-8") %>% read_html()
    tables <- html %>% html_element("div.Results.Results")
    results=tables%>% html_table()
    

    
    out=list(name=name,
             id=id,
             results=results %>% janitor::clean_names())
    
    structure(out,
              class="parkrun_results")
  },
  error=function(e){
    message("❌ Error: ", conditionMessage(e))
    NULL
  },
  warning=function(e){
    message(conditionMessage(e))
  }
  )
}
