library(shinythemes)

analyticsTag <- function() {
  id <- Sys.getenv("GOOGLE_ANALYTICS")
  if (nchar(id) > 0) {
    return(htmlTemplate('analytics.html', ID = id, document_ = F))
  } else {
    return(tagList())
  }
}

tagList(
  tags$head(analyticsTag()),
  tags$head(includeHTML('meta.html')),
  navbarPage(fluid = TRUE, theme=shinytheme("flatly"), collapsible = TRUE,
             id = "asNavbar",
             windowTitle = "AmbStat: English ambulance statistics",
             "AmbStat",
             tabPanel("Cardiac Arrest", cardiacArrestUi("cardiacArrest")),
             tabPanel("STEMI", stemiUi("stemi")),
             tabPanel("Call Outcomes", callOutcomesUi("callOutcomes")),
             tabPanel("Response Times", responseTimesUi("responseTimes")),
             tabPanel("Activity", activityUi("activity")),
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
)
