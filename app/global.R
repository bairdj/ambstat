library(tidyr)
library(shiny)
library(lubridate)
library(zoo)
library(dplyr)
library(plotly)
library(readr)
library(stringr)
library(ggplot2)
library(ggthemes)

service_list = c("All", "EAST MIDLANDS", "EAST OF ENGLAND", "ISLE OF WIGHT", "LONDON", "NORTH EAST", "NORTH WEST", "SOUTH CENTRAL", "SOUTH WESTERN", "WEST MIDLANDS", "YORKSHIRE")

source("./loaders.R", local = TRUE)
ambsys <- load_ambsys()
ambco <- load_ambco()

plt <- ggplot() +
  scale_colour_fivethirtyeight() +
  labs(caption = "Source: ambstat.uk, NHS England Ambulance Quality Indicators")

source("./modules/stemi.R")
source("./modules/cardiac-arrest.R")
source("./modules/call-outcomes.R")
source("./modules/response-times.R")
source("./modules/month-range.R")