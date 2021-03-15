library(tidyr)
library(dplyr)
library(tidyverse)

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

LitterfallLocFilt <- Litterfall %>%
                      select(Stand, Basket, Plot, Treatment, whole.mass) %>%
                      filter(Stand == c("C7", "JBO"))

CombinedLitLoc <- merge(LitterBasketLoc, LitterfallLocFilt)
  
#SoilRespLoc <- 