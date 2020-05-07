server <- function(input, output) {
  stemi <- callModule(stemi, "stemi", ambco)
  cardiacArrest <- callModule(cardiacArrest, "cardiacArrest", ambco)
  callOutcomes <- callModule(callOutcomes, "callOutcomes", ambsys)
}