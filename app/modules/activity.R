activityUi <- function(id) {
  ns <- NS(id)
  
  tagList(
    h1("Contacts"),
    tabsetPanel(
      tabPanel("Time Series", plotlyOutput(ns("contacts"))),
      tabPanel("By Year", plotlyOutput(ns("contactsMonth"))),
      tabPanel("By Population",
               gt_output(ns("contactsPopulation")),
               p("Population data source: Office for National Statistics licensed under the Open Government Licence."),
               selectInput(ns("yearPopulation"), "Year", c(2017,2018,2019), selected=2019)
      )
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
      layout(
        yaxis = list(title="Contacts"),
        title="Contacts per month",
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(count=6,label="6M",step="month",stepmode="backward"),
              list(count=12,label="12M",step="month",stepmode="backward"),
              list(step="all",label="All")
            )
          )
        )
      )
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
  
  output$contactsPopulation <- render_gt({
    # Load local authority -> ambulance mapping
    amb_lad <- read_csv('data/LAD ambulance.csv')
    # Load population data into tidy format
    readxl::read_excel('data/ukmidyearestimates20192020ladcodes.xls', sheet = 'MYE 5', skip=4) %>%
      select(Code,Name,Geography1, starts_with("Estimated")) %>%
      rename(LAD20CD = Code) %>%
      pivot_longer(starts_with("Estimated"),names_to="Year",values_to="Population") %>%
      mutate(Year = as.numeric(substr(Year, nchar(Year) - 3, nchar(Year)))) -> population
    # Join AmbSYS to aggregated population data
    ambsys %>%
      select(Year, `Org Code`, Ambulance.Service, A0) %>%
      group_by(`Org Code`, Ambulance.Service, Year) %>%
      summarise(Calls = sum(A0), .groups = "keep") %>%
      inner_join(population %>%
                   inner_join(amb_lad) %>%
                   group_by(Year, `Org Code`) %>%
                   summarise(Population = sum(Population))
                 ) %>%
      filter(Year == input$yearPopulation) %>%
      ungroup() %>%
      select(-Year,-`Org Code`) %>%
      mutate(P100 = (Calls/Population)*1e5) %>%
      arrange(Ambulance.Service) %>%
      gt() %>%
      tab_header(title = paste0("Contacts per capita - ", input$yearPopulation)) %>%
      cols_label(Ambulance.Service = "Ambulance Service", P100 = "Per 100k population") %>%
      cols_move(vars(Calls),vars(Population)) %>%
      fmt_number(vars(Calls,Population,P100),decimals=0)
    
  })
}