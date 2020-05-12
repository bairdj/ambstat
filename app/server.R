server <- function(input, output) {
  theme_set(
    theme_fivethirtyeight(base_size=15)
  )
  plt <- ggplot() +
    scale_colour_fivethirtyeight() +
    labs(caption = "Source: ambstat.uk, NHS England Ambulance Quality Indicators")
  
  stemi <- callModule(stemi, "stemi", ambco)
  cardiacArrest <- callModule(cardiacArrest, "cardiacArrest", ambco)
  callOutcomes <- callModule(callOutcomes, "callOutcomes", ambsys)
  responseTimes <- callModule(responseTimes, "responseTimes", ambsys, plt)
}