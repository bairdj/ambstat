# UI
cardiacArrestUi <- function(id) {
  ns <- NS(id)
  
  tagList(
    h1("Cardiac arrest by ambulance service"),
    sidebarLayout(
      sidebarPanel(
        monthRangeUi(ns("month"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("ROSC/Survival (all)", plotlyOutput(ns("compare_rosc_all"))),
          tabPanel("ROSC/Survival (Utstein)", plotlyOutput(ns("compare_rosc_utstein"))),
          tabPanel("Outcomes", plotlyOutput(ns("rosc_sankey")), textOutput(ns('sankey_period')))
        ),
        p("Only months with complete data are included. This may not reflect the full specified search period.")
      )
    ),
    h1("ROSC Trend"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns("service"),
          "Ambulance Service",
          service_list
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotlyOutput(ns("rosc_ts"))),
          tabPanel("Table", tableOutput(ns("ts_table")))
        )
      )
    )
  )
}

# Server
cardiacArrest <- function(input, output, session, ambco) {
  
  monthRange <- callModule(monthRange, "month", ambco %>% drop_na(R1r, R1n, R2n, R2r, R3s, R3n, R4n, R4s) %>% pull(Date))
  
  data <- reactive({
      req(monthRange$start())
      req(monthRange$end())
      ambco %>% filter(Date >= monthRange$start(), Date <= monthRange$end())
  })
  
  sankey <- reactive({
    data() %>% drop_na(R0n, R1n, R1r, R3s, R3n)
  })
  
  trend <- reactive({
    df <- ambco
    if (input$service != 'All') {
      df <- ambco %>% filter(Ambulance.Service == input$service)
    }
    df %>%
      drop_na(R1r, R1n, R2n, R2r) %>%
      group_by(Date) %>%
      summarise(RoscAll = sum(R1r)/sum(R1n)*100, RoscUtstein = sum(R2r)/sum(R2n)*100) %>%
      mutate(RollingAll = rollmeanr(RoscAll, 12, fill = NA), RollingUtstein = rollmeanr(RoscUtstein, 12, fill=NA))
  })
  
  
  output$compare_rosc_all <- renderPlotly({
    rosc <- data() %>%
      group_by(Ambulance.Service) %>%
      summarise(Rosc = sum(R1r)/sum(R1n)*100, Survival = sum(R3s)/sum(R3n)*100)
    plot_ly(rosc, type="bar",x=~Ambulance.Service,y=~Rosc,name="ROSC") %>%
      add_trace(y=~Survival,name="Survival") %>%
      layout(
        xaxis = list(
          title = list(
            text="Ambulance service"
          )
        ),
        yaxis = list(
          range = c(0,100),
          title = list(
            text="%"
          )
        )
      )
  })
  output$compare_rosc_utstein <- renderPlotly({
    utstein <- data() %>% group_by(Ambulance.Service) %>% summarise(Rosc = sum(R2r)/sum(R2n)*100, Survival = sum(R4s)/sum(R4n)*100) %>%
      plot_ly(type="bar", x=~Ambulance.Service,y=~Rosc,name="ROSC") %>%
      add_trace(y=~Survival,name="Survival") %>%
      layout(
        xaxis = list(
          title = list(
            text="Ambulance service"
          )
        ),
        yaxis = list(
          title = list(
            text="%"
          ),
          range = c(0,100)
        )
      )
  })
  
  output$rosc_sankey <- renderPlotly({
    x <- sankey() %>% summarise_at(vars(R0n,R1n,R1r,R3s,R3n), sum)
    plot_ly(
      type = "sankey",
      node = list(
        label = c("Cardiac arrests", "Resuscitation attempted", "Resuscitation not attempted", "ROSC at hospital", "Deceased", "Survived to discharge", "Unknown outcome"),
        x = c(0, 0.3, 0.3, 0.6, 1, 1, 1),
        y = c(0, NA, NA, 1, NA, NA)
      ),
      link = list(
        source = c(0,0,1,1,2,3,3,3),
        target = c(1,2,3,4,4,4,5,6),
        value = c(
          x$R1n, # Arrests -> resuscitation attempted
          x$R0n-x$R1n, # Arrests -> no attempt
          x$R1r, # Attempted -> ROSC
          x$R1n-x$R1r, # Attempted -> deceased
          x$R0n-x$R1n, #No attempt -> deceased
          x$R1r-x$R3s-(x$R1n-x$R3n), # ROSC -> deceased
          x$R3s, # ROSC -> survived,
          x$R1n-x$R3n
        )
      ),
    )
  })
  
  output$sankey_period <- renderText({
    min <- min(sankey()$Date)
    max <- max(sankey()$Date)
    paste("Showing data from ", format(min, "%B %Y"), " to ", format(max, "%B %Y"))
  })
  
  output$rosc_ts <- renderPlotly({
    trend() %>%
        plot_ly(type="scatter", mode="lines", x=~Date, y=~RoscAll, name="All") %>%
        add_trace(y=~RoscUtstein, name="Utstein") %>%
        add_trace(y=~RollingAll, name="12-month (all)", visible="legendonly") %>%
        add_trace(y=~RollingUtstein, name="12-month (Utstein)", visible="legendonly") %>%
      layout(
        yaxis = list(
          range = c(0,100),
          title = list(text='ROSC %')
        )
      )
  })
  output$ts_table <- renderTable({
    trend()
  })
}