callOutcomesUi <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        monthRangeUi(ns("month"))
      ),
      mainPanel(
        h1("Call outcomes by ambulance service"),
        tabsetPanel(
          tabPanel("Hear & Treat", plotlyOutput(ns("hearTreat"))),
          tabPanel("See & Treat", plotlyOutput(ns("seeTreat"))),
          tabPanel("Flow", plotlyOutput(ns("flow")))
        )
      )
    ),
    fixedRow(
      column(offset=4, width=8, h1("Hear & Treat Trend"))
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("service"), "Service", service_list)
      ),
      mainPanel(
        plotlyOutput(ns("ts"))
      )
    )
  )
}

callOutcomes <- function(input, output, session, ambsys) {
  
  monthRange <- callModule(monthRange, "month", ambsys %>% drop_na(A7,A56,A17,A53,A54,A55,A18,A19,A21,A22) %>% pull(Date))
  
  data <- reactive({
    req(monthRange$start())
    req(monthRange$end())
    ambsys %>% filter(Date >= monthRange$start(), Date <= monthRange$end())
  })
  
  output$hearTreat <- renderPlotly({
    data() %>%
      drop_na(A17, A7, A19, A22, A7, A18, A21) %>%
      group_by(Ambulance.Service) %>%
      summarise(
        HearTreat = sum(A17)/sum(A7)*100,
        Referred = (sum(A19)+sum(A22))/sum(A7)*100,
        Advice = (sum(A18)+sum(A21))/sum(A7)*100,
        ) %>%
      plot_ly(
        x=~Ambulance.Service,
        y=~Advice,
        name="Closed with advice",
        type="bar"
      ) %>%
      add_trace(y=~Referred,name="Referred to other service") %>%
      layout(
        xaxis = list(
          title = list(text="Ambulance service")
        ),
        yaxis = list(
          title = list(text="%")
        ),
        barmode = "stack"
      )
  })
  
  output$seeTreat <- renderPlotly({
    data() %>%
      drop_na(A55, A7) %>%
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
  
  output$ts <- renderPlotly({
    df <- ambsys
    if (input$service != "All") {
      df <- df %>% filter(Ambulance.Service == input$service)
    }
    df %>%
      drop_na(A17,A7,A19,A22,A21) %>%
      group_by(Date) %>%
      summarise(
        HearTreat = sum(A17)/sum(A7)*100,
        Referred = (sum(A19)+sum(A22))/sum(A7)*100,
        Advice = (sum(A18)+sum(A21))/sum(A7)*100) %>%
      plot_ly(x=~Date,y=~Advice, name="Advice", type="scatter", mode="none", stackgroup="ht") %>%
      add_trace(y=~Referred, name="Referred", stackgroup="ht") %>%
      layout(
        yaxis = list(
          title = list(text="%")
        )
      )
  })
  
  
}