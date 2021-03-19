library(tidyverse)
library(shiny)
library(rgdal)
library(dplyr)
library(sp)

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

#converting UTM to Lat Long
LitterUTM <- select(LitterBasket_Coord, stand, plot, basket, stake1_utm_x, stake1_utm_y)

utm <- SpatialPoints(LitterUTM[c("stake1_utm_x", "stake1_utm_y")], proj4string=CRS("+proj=utm +zone=19T +datum=WGS84"))

Lat_Long_Lit <- spTransform(utm, CRS("+proj=longlat +datum=WGS84"))

Lat_Long_Lit <- as.data.frame(Lat_Long_Lit)

write_csv(Lat_Long_Lit, "Lat_Long_Lit")

Litterfall_Lat_Long <- LitterUTM %>% 
  mutate(Lat = Lat_Long_Lit$stake1_utm_y) %>% 
  mutate(Long = Lat_Long_Lit$stake1_utm_x)

lat_long <- lat_long %>%
  rename(Stand = Site, Basket = Stake)

Litterfall_Lat_Long <- Litterfall_Lat_Long %>% 
  rename(Stand = stand, Plot = plot, Basket = basket)

LitterMerge <- merge(Litterfall, lat_long)

LitterMerge <- LitterMerge%>% 
  mutate(popup_info = paste("Stand:", Stand, "<br/>",
                            "Plot:", Plot, "<br/>",
                            "Basket:", Basket, "<br/>",
                            "Year:", Year, "<br/>",
                            "Treatment:", Treatment))
  









