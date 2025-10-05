input=list()
input$group="adapat"
input$name="Sebastian BATE"
input$min=3
input$eventsruns="events"
prs <- sort(unique((all_results %>% filter(name == input$name))$event))
names <- switch(input$group,
                "adapat" = c(
                  "Adam BURNETT", "Alex BUCKLEY", "Andrew CARLSON", "Charlotte TURNER",
                  "Frankie BALE", "Jonathan O'DONNELL", "Joseph GUNTRIP", "Luke DONALD",
                  "Max LETCHFIELD", "Michael PETER", "Natalie HARPER", "Philip MOYLE",
                  "Rachel BROWN", "Rob MOONEY", "Sebastian BATE", "Suzy HILL",
                  "Tom ALMOND"
                ),
                "bate" = c(
                  "Catherine BATE", "Ewan BATE", "Lawrence BATE", "Sebastian BATE"
                ),
                "wnr" = c(
                  "Alexandra BERESFORD", "Callum SHINGLER", "Colm MULHERN",
                  "Kirsty WATKINSON", "Gary SCOTT", "Helen ANDREWS",
                  "Isabel PRECIOUS-BIRDS", "Jon SHAW", "Lindsay HASTON",
                  "Lynda CLIFFORD", "Paul CLIFFORD", "Paul Thomas MULDOON",
                  "Sebastian Bate"
                )
)