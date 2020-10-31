activityUi <- function(id) {
  ns <- NS(id)
  
  tagList(
    h1("Contacts"),
    tabsetPanel(
      tabPanel("Time Series", plotlyOutput(ns("contacts"))),
      tabPanel("By Year", plotlyOutput(ns("contactsMonth")))
    ),
    h1("Incidents"),
    tabsetPanel(
      tabPanel("Time Series", plotlyOutput(ns("incidents"))),
      tabPanel("By Year", plotlyOutput(ns("incidentsMonth")))
    )
  )
  
}

activity <- function(input, output, session, ambsys) {
  output$contacts <- renderPlotly({
    ambsys %>%
      group_by(Ambulance.Service) %>%
      select(Ambulance.Service, Date, A0) %>%
      plot_ly(x=~Date, y=~A0, mode='lines', color=~Ambulance.Service, type='scatter') %>%
      layout(yaxis = list(title="Contacts"), title="Contacts per month")
  })
  
  output$contactsMonth <- renderPlotly({
    ambsys %>%
      group_by(Year, Month) %>%
      summarise(Contacts = sum(A0)) %>%
      mutate(Year = as.factor(Year), Month = factor(Month, levels=1:12, labels=month.abb)) %>%
      plot_ly(x=~Month,y=~Contacts,color=~Year,mode='lines+markers',type='scatter') %>%
      layout(title = "Contacts per year")
  })
  
  output$incidents <- renderPlotly({
    ambsys %>%
      group_by(Ambulance.Service) %>%
      select(Ambulance.Service, Date, A7) %>%
      plot_ly(x = ~Date, y=~A7, mode='lines', color=~Ambulance.Service, type='scatter') %>%
      layout(yaxis = list(title="Incidents"), title="Incidents per month")
  })
  
  output$incidentsMonth <- renderPlotly({
    ambsys %>%
      group_by(Year, Month) %>%
      summarise(Incidents = sum(A7)) %>%
      mutate(Year=as.factor(Year), Month=factor(Month, levels=1:12, labels=month.abb)) %>%
      plot_ly(x=~Month, y=~Incidents,color=~Year,mode='lines+markers',type='scatter') %>%
      layout(title = "Incidents per year")
  })
}