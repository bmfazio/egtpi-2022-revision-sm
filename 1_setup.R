library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(reactablefmtr)
library(htmltools)
library(googlesheets4)
library(crosstalk)

with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
            title = tooltip, value)
}
