library(tidyverse)
library(httr)
library(rvest)
library(xml2)
library(stringr)

get_all_runs=function(id, headers=c("User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/135.0.0.0 Safari/537.36")){
  id=str_remove_all(id, "\\D")
  
  response=GET(paste0("https://www.parkrun.org.uk/parkrunner/",id,"/all/"),
               add_headers(.headers = headers))
  
  tryCatch({
    if(status_code(response)!=200){
      stop(sprintf("Request failed [%d] for %s", code, url))
    }
    html <- content(response, as = "text", encoding = "UTF-8") %>% read_html()
    tables <- html %>% html_elements("table#results")
    results=tables[[3]] %>% html_table()
    
    
    h2_nodes <- html %>% html_elements("h2")
    
    # Extract plain text
    h2_text <- h2_nodes %>% html_text2()
    
    name <- str_extract(h2_text, "^[^(]+") %>% str_trim()   # everything before "("
    id   <- str_extract(h2_text, "\\(([^)]+)\\)") %>% str_remove_all("[()]") %>% str_sub(2) %>% as.numeric()
    
    out=list(name=name,
             id=id,
             results=results)
    
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

ids=tribble(~id,
'7232608', # Adam
'4087050', # Alex
'7402459', # Andy
'7072913', # Nat
'4458732', # Frankie
'593256', # Jonny
'2407768', # Luke
'582473', # Max
'7266141', # Phil
'1539187', # Rachel
'5349926', # Rob
'493595', # Seb
'7756087', # Suzy
'5243446', # Tom A
'2548951', #charlotte
'5107378', #mike
'7073694', #jgunz
#bate
'147910', #mum
'144486', #ewan
'140961', #dad

#wnr

"42804", #paul M
"9334474", #Alex beresford
"6391679", #kirsty
"7433025", #Helen
"4301378", #Lindsay
"4051781", #Callum
"16568", #Paul C
"16569", #Lynda
"3629365", #Isabel
"888332", #Colm
"1166640", #gary
"81779", #jon
)

all_parkruns=list()
for(i in ids$id){
  hold=get_all_runs(i)
  cat(paste(hold$name, "\n"))
  all_parkruns[[str_to_lower(str_remove_all(hold$name, "\\s"))]]=hold
  Sys.sleep(30)
}

save(all_parkruns, file="data/all_parkruns.RDa")
