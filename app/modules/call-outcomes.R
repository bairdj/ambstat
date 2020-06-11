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
      column(offset=4, width=8, h1("Trend"))
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("service"), "Service", service_list)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Hear & Treat", plotlyOutput(ns("hearTreatTs"))),
          tabPanel("See & Treat", plotlyOutput(ns("seeTreatTs")))
        )
      )
    ),
    h1("Call categories"),
    plotOutput(ns("categories"))
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
  
  output$hearTreatTs <- renderPlotly({
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
  
  output$seeTreatTs <- renderPlotly({
    df <- ambsys
    if (input$service != "All") {
      df <- df %>% filter(Ambulance.Service == input$service)
    }
    df %>%
      drop_na(A55, A7) %>%
      group_by(Date) %>%
      summarise(
        SeeTreat = sum(A55)/sum(A7) * 100
      ) %>%
      plot_ly(x=~Date, y=~SeeTreat, name="See & Treat", mode="lines", type="scatter") %>%
      layout(
        yaxis = list(title = list(text="%"), rangemode="tozero")
      )
  })
  
  output$categories <- renderPlot({
    df <- ambsys %>%
      transmute(Ambulance.Service, C1 = A115, C2 = A119, C3 = A11, C4 = A12) %>%
      drop_na() %>%
      group_by(Ambulance.Service) %>%
      summarise_all(sum) %>%
      mutate(NonHCP = C1 + C2 + C3 + C4, C1 = C1 / NonHCP, C2 = C2 / NonHCP, C3 = C3 / NonHCP, C4 = C4 / NonHCP) %>%
      select(-NonHCP) %>%
      pivot_longer(c(C1, C2, C3, C4), names_to = "Category")
    plt +
      geom_col(aes(Category, value, fill = Category), position="dodge", data = df) +
      facet_wrap(vars(Ambulance.Service)) +
      labs(title = "Proportion of face to face incidents by category", subtitle="Excluding HCP/IFT and C5 incidents") +
      theme(legend.position = "none")
  })
  
  
}