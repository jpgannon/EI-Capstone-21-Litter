library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rgdal)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DT)
library(tidyverse)
library(ggthemes)
library(readr)

#library(sf)
#library(tigris)


Litterfall <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/Litterfall.csv")
SoilRespiration <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/SoilResp.csv")
StandLocations <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/StandLocations.csv")
lat_long <-
  read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/lat_long.csv")

#Filter soil resp data and converted to correct date format 
CleanSoilResp <- SoilRespiration %>% select(date, stand, flux, temperature, treatment) %>%
  mutate(date = mdy(date))
