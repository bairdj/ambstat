timeLabeller <- function(breaks) {
  round_hms(breaks, 1)
}

responseTimesUi <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("category"), "Category", choices = list("Category 1" = 1, "Category 2" = 2, "Category 3" = 3, "Category 4" = 4)),
        monthRangeUi(ns("month"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Mean", plotOutput(ns("meanResponsePlot"), height=600)),
          tabPanel("90th centile", plotOutput(ns("response90Plot"), height=600)),
          tabPanel("Monthly Performance", gt_output(ns("monthlyPerformance")))
        ),
        plotOutput(ns("responsePlot"))
      )
    )
  )
}

responseTimes <- function(input, output, session, ambsys, plt) {
  
  meanResponseTs <- reactive({
    req(monthRange$start())
    req(monthRange$end())
    d <- ambsys %>% group_by(Date, Ambulance.Service) %>%
      filter(Date >= monthRange$start(), Date <= monthRange$end())
    category <- as.integer(input$category)
    if (category == 1) {
      d %>% drop_na(A24, A8) %>% summarise(Response = sum(A24)/sum(A8))
    } else if (category == 2) {
      d %>% drop_na(A30, A10) %>% summarise(Response = sum(A30)/sum(A10))
    } else if (category == 3) {
      d %>% drop_na(A33, A11) %>% summarise(Response = sum(A33)/sum(A11))
    } else if(category == 4) {
      d %>% drop_na(A36, A12) %>% summarise(Response = sum(A36)/sum(A12))
    }
  })
  
  response90Ts <- reactive({
    req(monthRange$start())
    req(monthRange$end())
    d <- ambsys %>% group_by(Date, Ambulance.Service) %>%
      filter(Date >= monthRange$start(), Date <= monthRange$end())
    category <- as.integer(input$category)
    if (category == 1) {
      d %>% drop_na(A26) %>% summarise(Response = first(A26))
    } else if (category == 2) {
      d %>% drop_na(A32) %>% summarise(Response = first(A32))
    } else if (category == 3) {
      d %>% drop_na(A35) %>% summarise(Response = first(A35))
    } else if (category == 4) {
      d %>% drop_na(A38) %>% summarise(Response = first(A38))
    }
  })
  
  meanTarget <- reactive({
    category <- as.integer(input$category)
    if (category == 1) {
      7*60
    } else if (category == 2) {
      18 * 60
    } else {
      NA
    }
  })
  
  target90 <- reactive({
    category <- as.integer(input$category)
    if (category == 1){
      15 * 60
    } else if (category == 2) {
      40 * 60
    } else if (category == 3) {
      120 * 60
    } else if (category == 4) {
      180 * 60
    } else {
      NA
    }
  })
  
  output$meanResponsePlot <- renderPlot({
    df <- meanResponseTs()
    p <- plt + 
      geom_col(aes(Date, Response, fill=Ambulance.Service), df, show.legend = FALSE) +
      scale_y_time(limits = c(0, NA), labels = timeLabeller) +
      labs(title = paste("Mean category", input$category, "response time"), y = "Response time (minutes)") +
      facet_wrap(~Ambulance.Service)
    if (!is.na(meanTarget())) {
      p <- p + geom_hline(yintercept = meanTarget(), linetype="longdash", colour="blue")
    }
    p
  })
  
  output$response90Plot <- renderPlot({
    df <- response90Ts()
    p <- plt +
      geom_col(aes(Date, Response, fill=Ambulance.Service), df, show.legend = FALSE) +
      scale_y_time(limits = c(0, NA), labels = timeLabeller) +
      labs(title = paste("Category", input$category, "90th centile response time")) +
      facet_wrap(~Ambulance.Service)
    if (!is.na(target90())) {
      p <- p + geom_hline(yintercept = target90(), linetype="longdash", colour="blue")
    }
    p
  })
  
  monthRange <- callModule(monthRange, "month", ambsys %>% drop_na(A24,A8,A30,A10,A33,A11,A36,A12) %>% pull(Date))
  
  
  output$monthlyPerformance <- render_gt({
    req(monthRange$start())
    req(monthRange$end())
    ambsys %>% drop_na(A24,A8,A30,A10, A38, A35) %>%
      filter(Date >= monthRange$start(), Date <= monthRange$end()) %>%
      group_by(Ambulance.Service, Date) %>%
      summarise(C1Mean = first(A24)/first(A8), C2Mean = first(A30)/first(A10), C390 = first(A35), C490 = first(A38)) %>%
      group_by(Ambulance.Service) %>%
      summarise(
        n = n(),
        C1 = sum(C1Mean < 7*60),
        C2 = sum(C2Mean < 18*60),
        C3 = sum(C390 < 120 * 60),
        C4 = sum(C490 < 180 *60),
        PC1 = C1/n,
        PC2 = C2/n,
        PC3 = C3/n,
        PC4 = C4/n,
        Overall = C1 + C2 + C3 + C4,
        POverall = Overall/(n*4)
        ) %>%
      gt(rowname_col = "Ambulance.Service") %>%
      tab_header('Monthly performance target achievement', subtitle = paste(format.Date(monthRange$start(), "%B %Y"), "-", format.Date(monthRange$end(), "%B %Y"))) %>%
      tab_spanner("Category 1", vars(C1, PC1)) %>%
      tab_spanner("Category 2", vars(C2, PC2)) %>%
      tab_spanner("Category 3", vars(C3, PC3)) %>%
      tab_spanner("Category 4", vars(C4, PC4)) %>%
      tab_spanner("Overall", vars(Overall, POverall)) %>%
      fmt_percent(starts_with("P"), decimals = 1) %>%
      data_color(starts_with("P"), colors = scales::col_numeric(paletteer::paletteer_d("ggsci::green_material") %>% as.character(), domain = c(0,1))) %>%
      cols_label(n = "Months", C1 = "Achieved", C2 = "Achieved", C3 = "Achieved", C4 = "Achieved", Overall = "Achieved", PC1 = "%", PC2 = "%", PC3 = "%", PC4 = "%", POverall="%") %>%
      tab_footnote("Mean response < 7 minutes", cells_column_spanners("Category 1")) %>%
      tab_footnote("Mean response < 18 minutes", cells_column_spanners("Category 2")) %>%
      tab_footnote("90th centile response < 120 minutes", cells_column_spanners("Category 3")) %>%
      tab_footnote("90th centile response < 180 minutes", cells_column_spanners("Category 4"))
  })
}