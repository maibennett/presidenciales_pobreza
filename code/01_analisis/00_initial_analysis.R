################################################################################
### Title: "Análisis Presidenciales y Pobreza por Comuna - Chile 2021"
### Author: Magdalena Bennett
################################################################################

rm(list = ls())
cat("\014")

library(tidyverse)
library(ggplot2)

servel <- read.csv("https://raw.githubusercontent.com/maibennett/presidenciales_pobreza/main/data/servel_county_clean.csv")

casen <- read.csv("https://raw.githubusercontent.com/maibennett/presidenciales_pobreza/main/data/casen_county_clean.csv")

casen <- casen %>% rename(comuna_id = comuna)

d <- left_join(servel, casen, by = "comuna_id")