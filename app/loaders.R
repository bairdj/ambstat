ambsys_cols <- cols_only(
  Year = col_integer(),
  Month = col_integer(),
  Region = col_character(),
  `Org Name` = col_character(),
  `Org Code` = col_character(),
  A53 = col_integer(),
  A54 = col_integer(),
  A55 = col_integer(),
  A56 = col_integer(),
  A17 = col_integer(),
  A18 = col_integer(),
  A19 = col_integer(),
  A21 = col_integer(),
  A22 = col_integer(),
  A0 = col_integer(),
  A7 = col_integer(),
  A8 = col_integer(),
  A9 = col_integer(),
  A10 = col_integer(),
  A11 = col_integer(),
  A12 = col_integer(),
  A24 = col_integer(),
  A27 = col_integer(),
  A30 = col_integer(),
  A33 = col_integer(),
  A36 = col_integer(),
  A26 = col_integer(),
  A32 = col_integer(),
  A35 = col_integer(),
  A38 = col_integer(),
  A115 = col_integer(),
  A119 = col_integer()
)

load_ambsys <- function() {
  return(read_csv('AmbSYS-up-to-20201130.csv', col_types = ambsys_cols, na = c('.')) %>%
           ambstatify())
}

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


load_ambco <- function() {
  return(read_csv('AmbCO-up-to-20200731.csv', na = c('.'), col_types = ambco_cols) %>%
           ambstatify())
}

ambstatify <- function(.data) {
  return(.data %>% filter(startsWith(`Org Code`, "R")) %>% # Exclude regions (Yxx)
           mutate(`Org Name` = str_remove(`Org Name`, " (AMBULANCE SERVICE )*(UNIVERSITY )*NHS (FOUNDATION )*TRUST")) %>% # Simplify names
           mutate(ym = Year + (Month - 1) * 1/12) %>% # For use with xts yearmon
           mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>%
           rename(Ambulance.Service = `Org Name`))
}

build_cache <- function() {
  ambco <- load_ambco()
  ambsys <- load_ambsys()
  save(ambco, ambsys, file = "cached.Rdata")
}
