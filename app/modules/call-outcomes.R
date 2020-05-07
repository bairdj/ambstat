callOutcomesUi <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(ns("date"), "Search Period", format = "m/yyyy", start = today() - dyears())
    ),
    mainPanel(
      h1("Call outcomes by ambulance service"),
      tabsetPanel(
        tabPanel("Hear & Treat", plotlyOutput(ns("hearTreat"))),
        tabPanel("See & Treat", plotlyOutput(ns("seeTreat"))),
        tabPanel("Flow", plotlyOutput(ns("flow")))
      )
    )
  )
}

callOutcomes <- function(input, output, session, ambsys) {
  data <- reactive({
    ambsys %>% filter(Date >= input$date[1], Date <= input$date[2])
  })
  
  output$hearTreat <- renderPlotly({
    data() %>%
      group_by(Ambulance.Service) %>%
      summarise(HearTreat = sum(A17)/sum(A7)*100) %>%
      plot_ly(
        x=~Ambulance.Service,
        y=~HearTreat
      ) %>%
      layout(
        xaxis = list(
          title = list(text="Ambulance service")
        ),
        yaxis = list(
          title = list(text="%")
        )
      )
  })
  
  output$seeTreat <- renderPlotly({
    data() %>%
      group_by(Ambulance.Service) %>%
      summarise(SeeTreat = sum(A55)/sum(A7) * 100) %>%
      plot_ly(
        x=~Ambulance.Service,
        y=~SeeTreat
      ) %>%
      layout(
        xaxis = list(
          title = list(text="Ambulance Service")
        ),
        yaxis = list(
          title = list(text="%")
        )
      )
  })
  
  
  
  output$flow <- renderPlotly({
    d <- data() %>%
          drop_na(A7,A56,A17,A53,A54,A55,A18,A19,A21,A22) %>%
          summarise_at(vars(A7, A56, A17, A53, A54, A55, A18, A19, A21, A22), sum)
    plot_ly(
      type="sankey",
      node = list(
        label = c("Incidents", "Face to Face", "No Face To Face", "Transport to ED", "Transport non-ED", "No transport", "Advice", "Referred")
      ),
      link = list(
        source = c(0,0,1,1,1,2,2),
        target = c(1,2,3,4,5,6,7),
        value = c(d$A56, d$A17,d$A53,d$A54,d$A55,d$A18+d$A21,d$A19+d$A22)
      )
    )
  })
}