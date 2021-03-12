

#Load data

library(readr)
soil_resp_data <- read_csv("Litter_and_Respiration/soil.resp.va.tech.1.29.21.csv")




# convert to long and #Convert date to "month, day, year" form

library(tidyverse)
library(lubridate)

soil_resp_data2 <- soil_resp_data %>% select(date, stand, flux, treatment) %>%
  pivot_longer(cols = c("flux")) %>%
  mutate(date = mdy(date))



ggplot(soil_resp_data2, aes(x = date, y = value))+
  geom_line()+
  theme_classic()+
  ylab("Flux")
