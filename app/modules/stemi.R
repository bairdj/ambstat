# UI
stemiUi <- function(id) {
  ns <- NS(id)
  
  tagList(
    h1("STEMI by ambulance service"),
    sidebarLayout(
      sidebarPanel(
        monthRangeUi(ns("month"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("PPCI",
                   p("Number of patients directly admitted after transportation by an ambulance service in England, with a hospital admission date in the month in question, and an initial diagnosis of 'definite Myocardial Infarction'. Compared with number who subsequently have PPCI."),
                   plotlyOutput(ns("ppciChart"))
                   ),
          tabPanel("Call to Angiography Time",
                   p("Time from 999 call connect until catheter insertion for angiography."),
                   plotlyOutput(ns("callToBalloonChart"))),
          tabPanel("Table", tableOutput(ns("ppciTable")))
        )
      )
    ),
    h1("Trend"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("service"), "Ambulance Service", service_list)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("PPCI", plotlyOutput(ns("ppciTimeSeries"))),
          tabPanel("Call to Angiography Time", plotlyOutput(ns("callToBalloonTimeSeries")))
        )
      )
    )
  )
}

# Server
stemi <- function(input, output, session, ambco) {
  
  monthRange <- callModule(monthRange, "month", ambco %>% drop_na(M1n, M3n, M3m) %>% pull(Date))
  
  ppci <- reactive({
    ambco %>%
      drop_na(M1n,M3n,M3m) %>%
      filter(Date >= monthRange$start(), Date <= monthRange$end()) %>%
      group_by(Ambulance.Service) %>%
      summarise(
        Ppci = sum(M3n),
        NoPpci = sum(M1n)-sum(M3n),
        Total=sum(M1n),
        Rate=sum(M3n)/sum(M1n),
        CallToBalloon=mean(M3m)
        ) %>%
      arrange(-Rate)
  })
  
  timeSeries <- reactive({
    ts <- ambco %>% drop_na(M3m,M1n,M3n)
    if (input$service != "All") {
      ts <- ts %>% filter(Ambulance.Service == input$service)
    }
    ts %>% group_by(Date) %>% summarise(M3m = mean(M3m), M1n = sum(M1n), M3n = sum(M3n))
  })
  
  output$ppciChart <- renderPlotly({
    ppci() %>%
      plot_ly(type="bar", x=~Ambulance.Service, y=~Ppci, name="PPCI") %>% add_trace(y=~NoPpci, name="No PPCI") %>%
      layout(
        barmode='stack',
        yaxis = list(
          title = list(text="n")
        ),
        xaxis = list(
          title = list(text="Ambulance service"),
          type = 'category'
        )
      )
  })
  
  output$ppciTable <- renderTable({
    ppci()
  })
  
  output$callToBalloonChart <- renderPlotly({
    ppci() %>%
      plot_ly(type="bar", x=~Ambulance.Service, y=~CallToBalloon) %>%
      layout(
        yaxis = list(
          title = list(text="Time (minutes)")
        ),
        xaxis = list(
          title = list(text="Ambulance service")
        )
      )
  })
  
  output$ppciTimeSeries <- renderPlotly({
    timeSeries() %>%
      plot_ly(type="scatter", mode="lines", x=~Date, y=~M1n, name="Initial MI diagnosis") %>%
      add_trace(y=~M3n, name="Had PPCI") %>%
      layout(
        yaxis = list(
          title = list(text="n")
        )
      )
  })
  
  output$callToBalloonTimeSeries <- renderPlotly({
    timeSeries() %>%
      plot_ly(type="scatter", mode="lines", x=~Date, y=~M3m) %>%
      layout(
        yaxis = list(
          title = list(text="Minutes")
        ),
        title = list(
          text = "Mean time from 999 call connect\nto catheter insertion for angiography"
        )
      )
  })
}