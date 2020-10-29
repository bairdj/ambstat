server <- function(input, output) {
  theme_set(
    theme_fivethirtyeight(base_size=15)
  )
  
  stemi <- callModule(stemi, "stemi", ambco)
  cardiacArrest <- callModule(cardiacArrest, "cardiacArrest", ambco)
  callOutcomes <- callModule(callOutcomes, "callOutcomes", ambsys)
  responseTimes <- callModule(responseTimes, "responseTimes", ambsys, plt)
  activity <- callModule(activity, "activity", ambsys)
}