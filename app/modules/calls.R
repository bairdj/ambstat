callsUi <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      monthRangeUi(ns("month"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Calls", plotlyOutput(ns('contacts'))),
        tabPanel("Answer Times", plotlyOutput(ns('answer_times')))
      )
    )
  )
}

calls <- function(input, output, session, ambsys) {
  
  monthRange <- callModule(monthRange, "month", ambsys %>% drop_na(A0,A1) %>% pull(Date))
  
  data <- reactive({
    req(monthRange$start())
    req(monthRange$end())
    ambsys %>% filter(Date >= monthRange$start(), Date <= monthRange$end())
  })
  
  output$contacts <- renderPlotly({
    data() %>%
      group_by(Ambulance.Service) %>%
      drop_na(A0, A1) %>%
      summarise(Contacts = sum(A0), Answered=sum(A1)) %>%
      plot_ly(x=~Ambulance.Service,y=~Contacts,type="bar", name = "Contacts") %>%
      add_trace(y=~Answered, name="Calls Answered") %>%
      layout(
        xaxis = list(
          title = list(text = "Ambulance Service")
        )
      )
  })
  
  output$answer_times <- renderPlotly({
    data() %>%
      drop_na(A2, A1) %>%
      group_by(Ambulance.Service) %>%
      summarise(Answer = sum(A2)/sum(A1)) %>%
      plot_ly(x=~Ambulance.Service,y=~Answer,type="bar",name="Mean Answer Time") %>%
      layout(
        yaxis = list(
          title = list(text = "Mean Answer Time (s)")
        ),
        xaxis = list(
          title = list(text = "Ambulance Service")
        )
      )
  })
}