library(shinythemes)

stemiPeriod <- ambco %>% drop_na(M1n,M3n,M3m) %>% pull(Date)

navbarPage(fluid = TRUE, theme=shinytheme("flatly"), collapsible = TRUE,
  "AmbStat",
  tabPanel("Cardiac Arrest", cardiacArrestUi("cardiacArrest", min(ambco$Date), max(ambco$Date))),
  tabPanel("STEMI", stemiUi("stemi", min(stemiPeriod), max(stemiPeriod))),
  tabPanel("Call Outcomes", callOutcomesUi("callOutcomes")),
  tabPanel("Response Times", responseTimesUi("responseTimes")),
  footer = tagList(
    wellPanel(
      p(
        "All data sourced from ",
        a(href="https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/", "NHS England - Ambulance Quality Indicators")
      ),
      p("Created by James Baird - Paramedic"),
      a(href="https://github.com/bairdj/ambstat", "Github")
    )
  )
)