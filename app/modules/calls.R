callsUi <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Calls"),
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
    ),
    h2("Call rate"),
    plotOutput(ns('population'))
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
  
  output$population <- renderPlot({
    population <- readxl::read_excel('data/ccg-population.xlsx', sheet = "Mid-2018 Persons", skip = 6) %>%
      select(Area.Code = `Area Codes`, Population = `All Ages`) %>%
      drop_na()
    
    codes <- readr::read_csv('data/nhs-region-codes.csv', col_types = c('nccc')) %>%
      select(Area.Code = NHSER19CD, Region = NHSER19CDH, Label = NHSER19NM) %>%
      inner_join(population)
    
    call_rate <- ambsys %>%
      drop_na(A0) %>%
      filter(Year == 2018) %>%
      group_by(Region) %>%
      summarise(Calls = sum(A0)) %>%
      inner_join(codes) %>%
      mutate(CallsPerPop = Calls/Population, CallRate = CallsPerPop * 1000) %>%
      arrange(Label)
    
    call_rate
    
    ggplot(call_rate, aes(Region,CallRate)) + 
      geom_col() +
      scale_x_discrete(labels = pull(call_rate, Label)) +
      labs(title = "Calls per 1000 population by NHS Region (2018)", caption = "Source: ONS, NHS England") +
      theme_fivethirtyeight()
  })
}