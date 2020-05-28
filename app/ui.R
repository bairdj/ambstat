library(shinythemes)

navbarPage(fluid = TRUE, theme=shinytheme("flatly"), collapsible = TRUE,
  "AmbStat",
  tabPanel("Cardiac Arrest", cardiacArrestUi("cardiacArrest")),
  tabPanel("STEMI", stemiUi("stemi")),
  tabPanel("Call Outcomes", callOutcomesUi("callOutcomes")),
  tabPanel("Response Times", responseTimesUi("responseTimes")),
  tabPanel("Calls", callsUi("calls")),
  footer = tagList(
    wellPanel(
      p(
        "All data sourced from ",
        a(target="_blank", href="https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/", "NHS England - Ambulance Quality Indicators")
      ),
      p("Created by James Baird - Paramedic"),
      a(href="https://github.com/bairdj/ambstat", target="_blank", "Github")
    )
  )
)