library(tidyr)
library(shiny)
library(lubridate)
library(zoo)
library(dplyr)
library(plotly)
library(readr)
library(stringr)

service_list = c("All", "EAST MIDLANDS", "EAST OF ENGLAND", "ISLE OF WIGHT", "LONDON", "NORTH EAST", "NORTH WEST", "SOUTH CENTRAL", "SOUTH WESTERN", "WEST MIDLANDS", "YORKSHIRE")

source("./loaders.R", local = TRUE)
ambsys <- load_ambsys()
ambco <- load_ambco()

source("./modules/stemi.R")
source("./modules/cardiac-arrest.R")