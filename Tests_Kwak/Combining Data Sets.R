library(tidyverse)
library(shiny)
library(rgdal)
library(dplyr)

Litterfall <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/Litterfall.csv")
SoilRespiration <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/SoilResp.csv")
StandLocations <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/StandLocations.csv")
lat_long <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/lat_long.csv")
Subplot_Coord <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/Subplot_Coord.csv")
LitterBasket_Coord <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/LitterBasket_Coord.csv")




LitterBasketLoc <- select(LitterBasket_Coord, stand, basket, stake1_utm_x, stake1_utm_y)

LitterMerge <- merge(Litterfall, LitterBasketLoc)

CombinedLitLoc <- merge(LitterBasketLoc, LitterfallLocFilt)
  
#SoilRespLoc <- 

#Read in geo coordinate dataset
geo_dat <- CombinedLitLoc

#Subset data to only include UTM Easting and Northing columns
sp_dat <- geo_dat[c("stake1_utm_x", "stake1_utm_y")]

#Define UTM positioning
utm <- SpatialPoints(sp_dat, proj4string=CRS("+proj=utm +zone=19T +datum=WGS84"))

#Transform UTM data to lat/long based on projection info
CombinedLitLocLatLong <- spTransform(utm, CRS("+proj=longlat +datum=WGS84"))

CombinedLitLocLatLong <- as.data.frame(CombinedLitLocLatLong)
write_csv(CombinedLitLocLatLong, "CombinedLitLocLatLong.csv")















