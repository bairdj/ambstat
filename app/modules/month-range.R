presetShortcuts <- list("Select" = "", "Most recent" = list("6 months"="p6", "12 months"="p12"))

monthRangeUi <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("startMonth"), "Start", choices = list()),
    selectInput(ns("endMonth"), "End", choices = list()),
    selectInput(ns("shortcut"), "Shortcuts", choices=presetShortcuts)
  )
}

monthRange <- function(input, output, session, vector) {
  dates <- sort(unique(vector))
  names(dates) <- format(dates, "%B %Y")
  
  updateSelectInput(session, "startMonth", choices = dates, selected = min(dates))
  updateSelectInput(session, "endMonth", choices = dates, selected = max(dates))
  
  # Add relevant years to shortcuts
  yearList <- sort(unique(year(dates)), decreasing = TRUE)
  presetShortcuts$`Year` <- yearList
  updateSelectInput(session, "shortcut", choices = presetShortcuts)
  
  # If shortcut used, update startMonth and endMonth, then reset shortcut
  observeEvent(input$shortcut, {
    prefix <- substr(input$shortcut, 0, 1)
    if (prefix == "p") {
      m <- as.numeric(substr(input$shortcut, 2, 4))
      dateLength <- length(dates)
      updateVarSelectInput(session, "startMonth", selected = dates[[dateLength-m]])
      updateVarSelectInput(session, "endMonth", selected = dates[[dateLength]])
    } else if (input$shortcut != "") {
      yearShortcut <- as.numeric(input$shortcut)
      startShortcut <- make_date(yearShortcut)
      if (!(startShortcut %in% dates)) {
        startShortcut <- min(dates)
      }
      updateVarSelectInput(session, "startMonth", selected = startShortcut)
      endShortcut <- make_date(yearShortcut, 12, 01)
      if (!(endShortcut %in% dates)) {
        endShortcut <- max(dates)
      }
      updateVarSelectInput(session, "endMonth", selected = endShortcut)
    }
    updateVarSelectInput(session, "shortcut", selected="")
  }, ignoreInit = TRUE)
  
  return(
    list(
      start=reactive({
        if (str_length(input$startMonth) == 0) {
          NULL
        } else {
          input$startMonth
      }}),
      end=reactive({
        if (str_length(input$endMonth) == 0) {
          NULL 
        } else {
          input$endMonth
        }
        })
    )
  )
}