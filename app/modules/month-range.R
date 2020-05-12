monthRangeUi <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("startMonth"), "Start", choices = list()),
    selectInput(ns("endMonth"), "End", choices = list())
  )
}

monthRange <- function(input, output, session, vector) {
  dates <- sort(unique(vector))
  names(dates) <- format(dates, "%B %Y")
  
  updateSelectInput(session, "startMonth", choices = dates, selected = min(dates))
  updateSelectInput(session, "endMonth", choices = dates, selected = max(dates))
  
  return(
    list(
      start=reactive({input$startMonth}),
      end=reactive({input$endMonth})
    )
  )
}