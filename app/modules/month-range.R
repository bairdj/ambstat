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
      start=reactive({
        if (str_length(input$startMonth) == 0) {
          "1900-01-01"
        } else {
          input$startMonth
        }
      }),
      end=reactive({
        if (str_length(input$endMonth) == 0) {
          "2050-01-01" 
        } else {
          input$endMonth
        }
        })
    )
  )
}