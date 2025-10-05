
all_results %>% 
  summarise(people=n_distinct(parkrunner)-1, .by="name") 

distict_parkrunners=all_results %>% 
  summarise(events=n_distinct(event),
            runners=n(),
            .by=c("name","parkrunner")) %>% 
  filter(name!=parkrunner) %>% 
  arrange(-events)



gm=c("Alexandra","Bolton" ,"Bramhall","Burnage","Chadderton Hall","Cheadle Hulme","Clarence","Fletcher Moss", "Haigh Woodland","Heaton","Hyde","Marple","Oldham","Peel", "Pennington Flash", "Philips Park", "Sale Water", "South Manchester" ,"Stamford Park", "Stretford","Watergrove", "Woodbank", "Wythenshawe", "Worsley Woods")

distict_parkrunners_not_gm=all_results %>% 
  filter(!event %in% gm) %>% 
  summarise(event=n_distinct(event), .by=c("name","parkrunner")) %>% 
  filter(name!=parkrunner) %>% 
  arrange(-event)


rbenchmark::benchmark(
  "filter_first"={
    distict_parkrunners=all_results %>% 
      summarise(events=n_distinct(event),
                runners=n(),
                .by=c("name","parkrunner")) %>% 
      filter(name!=parkrunner) %>% 
      arrange(-events)
  },
  "filter_second"={
    distict_parkrunners=all_results %>% 
      filter(!event %in% gm) %>% 
      summarise(events=n_distinct(event),
                runners=n(),
                .by=c("name","parkrunner")) %>% 
      filter(name!=parkrunner) %>% 
      arrange(-events)
  },
  replications = 20
)

library(packcircles)
library(glue)

bubble=function(var, name_in){
  
  distict_parkrunners=all_results %>% 
    filter(name==name_in) %>% 
    # filter(!event %in% gm) %>% 
    summarise(events=n_distinct(event),
              runs=n(),
              .by=c("name","parkrunner")) %>% 
    mutate(value=.data[[var]])
  
  names=unique(all_results$name)
  
  data=distict_parkrunners %>% 
    arrange(-events, -runs) %>% 
    filter(row_number()<=10 | parkrunner %in% names) %>% 
    filter(value>=3)
  
  packing <- circleProgressiveLayout(data$value, sizetype = 'area')
  
  # Create data frame with circle positions
  circles <- circleLayoutVertices(packing, npoints = 50)
  # Combine with original data and calculate text size based on radius
  data_packed <- cbind(data, packing) %>% 
    mutate(
      name_events = paste(gsub(" ", "\n",parkrunner), value, sep = "\n"),
      # Scale text size proportionally to radius
      # Adjust the multiplier (1.2) to make text larger/smaller
      text_size = radius
    )
  
  # Create the plot
  p=ggplot() +
    # Draw the bubbles
    geom_polygon(data = circles, 
                 aes(x, y, group = id, fill = id),
                 colour = "white", 
                 linewidth = 1.5,
                 show.legend = FALSE) +
    # Add text labels with dynamic sizing
    geom_text(data = data_packed,
              aes(x, y, label = name_events, size = text_size),
              colour = "white") +
    # Customize appearance
    scale_fill_viridis_c(option = "plasma", begin=0, end=0.7) +
    scale_size_continuous(range = c(2, 8)) +  # Adjust min/max text size
    coord_equal() +
    theme_void() +
    guides(size = "none")  # Hide size legend
  
  return(p)
  
}

bubble("events","Sebastian BATE")
