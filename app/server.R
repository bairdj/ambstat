server <- function(input, output) {
  stemi <- callModule(stemi, "stemi", ambco)
  cardiacArrest <- callModule(cardiacArrest, "cardiacArrest", ambco)
  
}