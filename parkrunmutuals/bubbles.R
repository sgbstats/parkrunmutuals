
library(packcircles)
library(glue)
library(tidyverse)

bubble=function(var, name_in,prs, others, min=3, data_in=all_results){
  data=data_in %>% 
    filter(name==name_in,event %in% prs) %>%
    summarise(events=n_distinct(event),runs=n(),.by=c("name","parkrunner")) %>% 
    mutate(value=.data[[var]]) %>%
    arrange(-events, -runs) %>% 
    filter(row_number()<=20 | parkrunner %in% others,value>=min)
  
  packing <- circleProgressiveLayout(data$value, sizetype = 'area')
  circles <- circleLayoutVertices(packing, npoints = 50)
  data_packed <- cbind(data, packing) %>% 
    mutate(name_events = paste(gsub(" ", "\n",parkrunner), value, sep = "\n"),
           text_size = radius)
  
  ggplot() +
    geom_polygon(data = circles, aes(x, y, group = id, fill = id),
                 colour = "white", linewidth = 1.5, show.legend = FALSE) +
    geom_text(data = data_packed, aes(x, y, label = name_events, size = text_size),
              colour = "black",lineheight = 0.8) +
    scale_fill_viridis_c(option = "plasma", begin=1, end=0.5) +
    scale_size_continuous(range = c(4.5, 8.5)) +
    coord_equal() +
    theme_void() +
    guides(size = "none")
}

