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
          tabPanel("90% percentile", plotOutput(ns("response90Plot"), height=600))
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
      scale_y_time(limits = c(0, max(df$Response))) +
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
      scale_y_time(limits = c(0, max(df$Response))) +
      labs(title = paste("Category", input$category, "90th centile response time")) +
      facet_wrap(~Ambulance.Service)
    if (!is.na(target90())) {
      p <- p + geom_hline(yintercept = target90(), linetype="longdash", colour="blue")
    }
    p
  })
  
  monthRange <- callModule(monthRange, "month", ambsys %>% drop_na(A24,A8,A30,A10,A33,A11,A36,A12) %>% pull(Date))
}