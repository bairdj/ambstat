library(tidyr)
library(shiny)
library(lubridate)
library(zoo)
library(dplyr)
library(plotly)
library(readr)
library(stringr)

service_list = c("All", "EAST MIDLANDS", "EAST OF ENGLAND", "ISLE OF WIGHT", "LONDON", "NORTH EAST", "NORTH WEST", "SOUTH CENTRAL", "SOUTH WESTERN", "WEST MIDLANDS", "YORKSHIRE")

ambco_cols <- cols_only(
  Year = col_integer(),
  Month = col_integer(),
  `Org Name` = col_character(),
  `Org Code` = col_character(),
  R0n = col_integer(),
  R1n = col_integer(),
  R1r = col_integer(),
  R3s = col_integer(),
  R3n = col_integer(),
  R2n = col_integer(),
  R2r = col_integer(),
  R4n = col_integer(),
  R4s = col_integer(),
  M1n = col_integer(),
  M3n = col_integer(),
  M3m = col_double()
)

ambco <- read_csv('AmbCO-up-to-Nov-2019-CSV.csv', na = c('.'), col_types = ambco_cols) %>%
  filter(startsWith(`Org Code`, "R")) %>% # Exclude regions (Yxx)
  mutate(`Org Name` = str_remove(`Org Name`, " (AMBULANCE SERVICE )*NHS (FOUNDATION )*TRUST")) %>% # Simplify names
  mutate(ym = Year + (Month - 1) * 1/12) %>% # For use with xts yearmon
  mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>%
  rename(Ambulance.Service = `Org Name`)

source("./modules/stemi.R")
source("./modules/cardiac-arrest.R")