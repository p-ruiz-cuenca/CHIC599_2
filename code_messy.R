# Load packages ----

library(dplyr)
library(ggplot2)
library(sf)

# Load data ----

## STH data ----
sth <- read.csv("raw_data/data-ET-STH-sitelevel.csv")

str(sth)

sth_1 <- sth %>% 
  filter(Georeliability %in% c(1,2,3))

## ADM data ----

ETH_adm0 <- st_read("ETH_files/ETH_adm/ETH_adm0.shp")
