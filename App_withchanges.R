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
library(plotly)
library(rsconnect)


#Read in data
Litterfall <-
  read_csv("Data/Litterfall.csv")
SoilRespiration <-
  read_csv("Data/SoilResp.csv")
StandLocations <-
  read_csv("Data/StandLocations.csv")
lat_long <-
  read_csv("Data/lat_long.csv")
LitterBasket_Coord <-
  read_csv("Data/LitterBasket_Coord.csv")
Subplot_Coord <-
  read_csv("Data/Subplot_Coord.csv")

#convert litter.year in Litterfall data to numeric
Litterfall$litter.year<- substr(Litterfall$litter.year, 0, 4)
Litterfall$litter.year <- as.numeric(as.character(Litterfall$litter.year))

#converting UTM to Lat Long
LitterUTM <- select(LitterBasket_Coord, stand, plot, basket, stake1_utm_x, stake1_utm_y)
utm <- SpatialPoints(LitterUTM[c("stake1_utm_x", "stake1_utm_y")], proj4string=CRS("+proj=utm +zone=19T +datum=WGS84"))
Lat_Long_Lit <- spTransform(utm, CRS("+proj=longlat +datum=WGS84"))
Lat_Long_Lit <- as.data.frame(Lat_Long_Lit)
#write_csv(Lat_Long_Lit, "Lat_Long_Lit")
#Litterfall_Lat_Long <- LitterUTM %>% 
#  mutate(Lat = Lat_Long_Lit$stake1_utm_y) %>% 
#  mutate(Long = Lat_Long_Lit$stake1_utm_x)

#renaming data to merge tables
lat_long <- lat_long %>%
  rename(Stand = Site, Basket = Stake)

#Litterfall_Lat_Long <- Litterfall_Lat_Long %>% 
#  rename(Stand = stand, Plot = plot, Basket = basket)

#Merge Litterset to include Lat and Long, also adding pop up info to litter merge
LitterMerge <-merge(Litterfall, lat_long, by = c('Stand', 'Plot', 'Basket'), all = TRUE)

LitterMerge <-  LitterMerge %>% filter(!is.na(whole.mass))

#Grouping Data
GroupedLitterMerge <- LitterMerge %>%
  group_by(litter.year, Stand, Treatment, Plot, Basket) %>%
  summarize(whole.mass = mean(whole.mass), Lat = mean(Lat), Long = mean(Long)) %>% 
  filter(!is.na(Lat)) %>% 
  ungroup()

GroupedLitterMerge <- GroupedLitterMerge %>% 
  mutate(popup_info = paste("Stand:", Stand, "<br/>",
                            "Plot:", Plot, "<br/>",
                            "Basket:", Basket, "<br/>",
                            "Year:", litter.year, "<br/>",
                            "Treatment:", Treatment, "<br/>",
                            "Average Whole Mass", whole.mass))

#Filter soil resp data and converted to correct date format 
CleanSoilResp <- SoilRespiration %>% 
  select(date, stand, flux, temperature, treatment) %>%
  mutate(date = mdy(date)) %>%
  filter(!is.na(date))

#Clean up Litterfall Data and create LitterTable
Litterfall <- Litterfall%>%mutate(Treatment = paste(Treatment))
LitterTable <- Litterfall %>% select(litter.year, Stand, Treatment, whole.mass) %>%
  rename("Mass" = 4)

#Create Litterfall Data for litterfall by species analysis
Litterfall2 <- Litterfall %>% 
  select(litter.year, Stand, Treatment, ASH, BASP, BASS, BE, HB, OAK, PC, RM, SM, STM, WB, YB, 
         QASP, GB, MM, RO, SEEDS, TWIGS, ASP, VIB, UNK, NL) %>%
  pivot_longer(cols = c(ASH, BASP, BASS, BE, HB, OAK, PC, RM, SM, STM, WB, YB, 
                        QASP, GB, MM, RO, SEEDS, TWIGS, ASP, VIB, UNK, NL))

GroupedLitter_Species <- Litterfall2 %>%
  group_by(litter.year,Treatment,name) %>%
  filter(!is.na(value)) %>%
  summarize(value = mean(value)) %>% 
  ungroup()

#Create color palet used for interactive map
colors <- c("Blues")
pal <- colorNumeric(colors, GroupedLitterMerge$whole.mass)

#Grouping Data
GroupedLitter_Specific <- LitterTable %>% 
  group_by(litter.year, Stand, Treatment) %>%
  summarize(Mass = mean(Mass, na.rm = TRUE))%>% 
  ungroup()

#Grouping Soil Respiration Data
GroupedSoilData <- CleanSoilResp %>%
  group_by(date, stand, treatment) %>%
  summarize(flux = mean(flux), temperature = mean(temperature))%>% 
  ungroup()

GroupedSoilData %>% 
  mutate(date = ymd(date))%>% 
  ungroup()

#Dataset used for Bivariate Plots
dataset <- Litterfall %>% 
  filter(!is.na(Litterfall)) %>% 
  select(litter.year, Treatment, Basket, Plot, Season, Stand, Site, whole.mass, 
         ASH, BASP, BASS, BE, HB, OAK, PC, RM, SM, STM, WB, YB, QASP, 
         GB, MM, RO, SEEDS, TWIGS, ASP, VIB, UNK, NL, N, P, mass.per.unit.area)

dataset1 <- CleanSoilResp %>%
  select(date, stand, flux, temperature, treatment) %>%
  mutate(date(ymd(date)))


#Dashboard setup
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "MELNHE", titleWidth = 250),
  dashboardSidebar(width = 263, sidebarMenu( 
    menuItem("User Guide", tabName = "Home_Page", icon = icon("home")),
    menuItem("Map", tabName = "Map", icon = icon("globe")),
    menuItem("Litterfall", icon = icon("leaf"), startExpanded = TRUE,
             menuSubItem("Mass (Multiple Stands)",
                         tabName = "Litterfall_All",
                         icon = icon("bar-chart-o")),
             menuSubItem("Mass (Compare Stands)",
                         tabName = "Litterfall",
                         icon = icon("bar-chart-o")),
             menuSubItem("Avg. Mass (Multiple Stands)",
                         tabName = "Litterfall_Grouped_All",
                         icon = icon("bar-chart-o")),
             menuSubItem("Avg. Mass (Compare Stands)",
                         tabName = "Litterfall_Grouped",
                         icon = icon("bar-chart-o")),
             menuSubItem("Mass By Species",
                         tabName = "Litterfall_Species",
                         icon = icon("bar-chart-o"))),
    menuItem("Soil Respiration", icon = icon("tint"), startExpanded = TRUE,
             menuSubItem("Flux (Multiple Stands)",
                         tabName = "Flux_AllStands",
                         icon = icon("bar-chart-o")),
             menuSubItem("Flux (Compare Stands)",
                         tabName = "Soil_Respiration",
                         icon = icon("bar-chart-o")),
             menuSubItem("Avg. Flux (Multiple Stands)",
                         tabName = "MeanFlux_AllStands",
                         icon = icon("bar-chart-o")),
             menuSubItem("Avg. Flux (Compare Stands)",
                         tabName = "SoilRespiration_FluxMean",
                         icon = icon("bar-chart-o")),
             menuSubItem("Temperature (Multiple Stands)",
                         tabName = "SoilTemp_AllStands",
                         icon = icon("bar-chart-o")),
             menuSubItem("Temperature (Compare Stands)",
                         tabName = "SoilRespiration_Temp",
                         icon = icon("bar-chart-o")),
             menuSubItem("Avg. Temperature (Multiple Stands)",
                         tabName = "TMean_AllStands",
                         icon = icon("bar-chart-o")),
             menuSubItem("Avg. Temperature (Compare Stands)",
                         tabName = "SoilRespiration_TMean",
                         icon = icon("bar-chart-o"))),
    menuItem("Bivariate Analysis", icon = icon("random"), startExpanded = TRUE,
             menuSubItem("Litterfall Bivariate",
                         tabName = "Litterfall_BiVar",
                         icon = icon("bar-chart-o")),
             menuSubItem("Soil Respiration Bivariate",
                         tabName = "SoilRespiration_BiVar",
                         icon = icon("bar-chart-o"))),
    menuItem("Explore Data", icon = icon("table"), startExpanded = TRUE,
             menuSubItem("Visualize Litterfall Data",
                         tabName = "Litterfall_Data",
                         icon = icon("book")),
             menuSubItem("Visualize Soil Respiration Data",
                         tabName = "SoilRespiration_Data",
                         icon = icon("book")))
  )),
  
  dashboardBody(tabItems(
    tabItem(tabName = "Home_Page",
            h1("User Guide"),
            fluidPage(column(tags$img(src="melnhe1.png",width="1000px",height="90px"),width=10, align = "center"),
                      column(width= 10, helpText(strong("Map Tab:"), "An interactive map that allows the user to visualize data by plotting circle markers on a world map.
                                                 Markers will show up based on user input for the stand, treatment, and year, or a combination. In addition, if you click on the points placed on the map you will see additional pop-up information.", style = "font-size:20px")
                      ),
                      column(width = 10, helpText(strong("Litterfall:"), "
                      This section uses data from the “Litterfall.csv” file. The shown “litter year” is the represents the start of a litter year range. Example: If the litter year range is “2009-2010” for an observation, the shown litter year range on the date range input and plot will be “2009”. 
                      “Multiple Stands” tabs are used to look at combined data.
                      “Compare Stands” tabs are used to look at and compare data on a stand by stand level.
                      Treatment color is fluid and will change in each tab based upon user selections in that tab", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Mass (Multiple Stands):"), "Litterfall mass is visualized across all stands. The user is able to filter by date range, treatment, and stand. 
                      Time Series Plot: A timeseries plot is generated where every dot represents an individual recorded mass observation. Dots are shown alongside a boxplot. This boxplot is generated using the dots of a specific treatment type and specific litter year. Both the dots and their correlating boxplot are colored by treatment type. 
                      Boxplot: Interactive box plots are generated summarizing litterfall mass observations by selected user inputs.", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Mass (Compare Stands):"), "Litter mass is visualized across a specific stand. The user is able to filter by date range, stand, treatment, and stand. If the user selects more than one stand, plots are faceted by stand.
                      Time Series Plot: A timeseries plot is generated where every dot represents an individual recorded mass observation in the selected stand. Dots are shown alongside a boxplot. This boxplot is generated using the dots of a specific treatment type and specific litter year. Both the dots and their correlating boxplot are colored by treatment type.
                      Boxplot: Interactive box plots are generated summarizing litterfall mass observations by selected user inputs.", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Average Mass (Multiple Stands):"), "Average litterfall mass by litter year is visualized across selected inputs. Mass recordings are separated by litter year, treatment type, stand and  then averaged. The user is able to filter by litter year, treatment, and stand. 
                      Time Series Plot: A timeseries plot is generated where every dot represents the average recorded mass observation for the litter year. A line connects the dots to help display change in average mass. Both the dots and their correlating lines are colored by treatment type. 
                      Boxplot: Interactive box plots are generated summarizing average litterfall mass observations by selected user inputs.", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Average Mass (Compare Stands):"), "Average litterfall mass is visualized across a specific stand. Mass recordings are separated by litter year, treatment type, stand and then averaged. The user is able to filter by date range, treatment, and stand. If the user selects more than one stand, plots are faceted by stand.
                      Time Series Plot: A timeseries plot is generated where every dot represents an average recorded mass observation for the litter year. A line connects the dots to help display change in average mass. Both the dots and their correlating lines are colored by treatment type. 
                      Boxplot: Interactive box plots are generated summarizing average litterfall mass observations by selected user inputs.", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Mass By Stand:"), "Average litterfall mass is visualized across a specific species. Mass recordings are separated by litter year, treatment type, stand and then averaged. The user is able to filter by litter year range, treatment, and species. If the user selects more than one species, plots are faceted by species.
                      Time Series Plot: A timeseries plot is generated where every dot represents an average recorded mass observation for the litter year. A line connects the dots to help display change in average mass. Both the dots and their correlating lines are colored by treatment type.", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Soil Respiration"), "This section uses data from the “SoilResp.csv” file.
                      “Multiple Stands” tabs are used to look at combined data. To fully compare treatments by day, a smaller date range selection is suggested.
                      “Compare Stands” tabs are used to look at and compare data on a stand by stand level.
                      Treatment color is fluid and will change in each tab based upon user selections in that tab", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Flux (Multiple Stands):"), "CO2 flux is visualized across all stands. The user is able to filter by date range, treatment, and stand. 
                      Time Series Plot: A timeseries plot is generated where every dot represents an individual recorded flux observation. Dots are shown alongside a boxplot. This boxplot is generated using the dots of a specific treatment type and specific observation date. Both the dots and their correlating boxplot are colored by treatment type. 
                      Boxplot: Interactive box plots are generated summarizing soil respiration flux observations by selected user inputs.", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Flux (Compare Stands):"), "CO2 flux is visualized across a specific stand. The user is able to filter by date range, stand, and treatment. If the user selects more than one stand, plots are faceted by stand.
                      Time Series Plot: A timeseries plot is generated where every dot represents an individual recorded flux observation. Dots are shown alongside a boxplot. This boxplot is generated using the dots of a specific treatment type and specific observation date. Both the dots and their correlating boxplot are colored by treatment type. 
                      Boxplot: Interactive box plots are generated summarizing soil respiration flux observations by selected user inputs.", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Average Flux (Multiple Stands):"), "Average CO2 flux per day is visualized across selected inputs. Flux recordings are separated by observation date, treatment type, stand and then averaged. The user is able to filter by date range, treatment, and stand.
                      Time Series Plot: A timeseries plot is generated where every dot represents an average recorded flux observation per day. A line connects the dots to help display change in average flux. Both the dots and their correlating lines are colored by treatment type.  
                      Boxplot: Interactive box plots are generated summarizing soil respiration flux observations by selected user inputs.", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Average Flux (Compare Stands):"), "Average CO2 flux per day is visualized across a specific stand. Flux recordings are separated by observation date, treatment type, stand and then averaged. The user is able to filter by date range, treatment, and stand. If the user selects more than one stand, plots are faceted by stand.
                      Time Series Plot: A timeseries plot is generated where every dot represents an average recorded flux observation per day. A line connects the dots to help display change in average flux. Both the dots and their correlating lines are colored by treatment type.  
                      Boxplot: Interactive box plots are generated summarizing soil respiration flux observations by selected user inputs.", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Temperature (Multiple Stands):"), "): Temperature is visualized across all stands. The user is able to filter by date range, treatment, and stand.
                      Time Series Plot: A timeseries plot is generated where every dot represents an individual recorded temperature observation. Dots are shown alongside a boxplot. This boxplot is generated using the dots of a specific treatment type and specific observation date. Both the dots and their correlating boxplot are colored by treatment type.
                      Boxplot: Interactive box plots are generated summarizing soil respiration flux observations by selected user inputs.", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Temperature (Compare Stands):"), "Temperature is visualized across a specific stand. The user is able to filter by date range, treatment, and stand.
                      Time Series Plot: A timeseries plot is generated where every dot represents an individual recorded temperature observation. Dots are shown alongside a boxplot. This boxplot is generated using the dots of a specific treatment type and specific observation date. Both the dots and their correlating boxplot are colored by treatment type. 
                      Boxplot: Interactive box plots are generated summarizing soil respiration flux observations by selected user inputs.", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Average Temperature (Multiple Stands):"), "Average Temperature per day is visualized across selected inputs. Temperature recordings are separated by observation date, treatment type, stand and then averaged. The user is able to filter by date range, treatment, and stand.
                      Time Series Plot: A timeseries plot is generated where every dot represents an average recorded temperature observation. A line connects the dots to help display change in average temperature. Both the dots and their correlating lines are colored by treatment type.  
                      Boxplot: Interactive box plots are generated summarizing soil respiration flux observations by selected user inputs. ", style = "font-size:20px")
                      ),
                      
                      column(width = 10, helpText(strong("Average Temperature(Compare Stands):"), "Average Temperature(Compare Stands): Average temperature per day is visualized across a specific stand. Temperature recordings are separated by observation date, treatment type, stand and then averaged. The user is able to filter by date range, treatment, and stand.
                      If the user selects more than one stand, plots are faceted by stand.
                      Time Series Plot: A timeseries plot is generated where every dot represents an average recorded temperature observation. A line connects the dots to help display change in average temperature. Both the dots and their correlating lines are colored by treatment type.  
                      Boxplot: Interactive box plots are generated summarizing soil respiration flux observations by selected user inputs.", style = "font-size:20px")
                      ),
                      
                      column(width= 10, helpText(strong("Bivariate Analysis Tab:"), "There are interactive bivariate plots for both Litterfall and Soil Respiration datasets.
                                                 The user can select variables of interest (for their desired dataset) and visualize the relationship in a scatter plot. The user is also able to color data points based on other parameters of interest.", style = "font-size:20px")
                             
                      ),
                      column(width = 10, helpText(strong("Explore Data Tab:"), "These sub tabs included in this section allow the user to explore either the litterfall dataset or the soil respiration dataset.
                                                  Variables can be filtered to view specific raw data.", style = "font-size:20px"))
                      
            )),
    
    #Creates interactive map tab with basic functions
    tabItem(tabName = "Map",
            h1("Interactive map of tree stands under study"),
            #User input for stand type
            #"HBCa" "W5", "HB", "JB" no data
            box(width = 6, selectInput("MapSite", "Select Stand :",
                                       c( "C1", "C2", "C3", "C4", 
                                          "C5", "C6", "C7", "C8",
                                          "C9", "HBO", "HBM", "JBO", 
                                          "JBM"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            #User input for treatment type             
            box(width = 3, selectInput("MapTreatment", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected = "P"),
            ),
            #Average of whole plot
            box(width = 3, title = "Plot Average:",
                checkboxInput('average', 'Average per Plot')),
            #User input for date range
            box(width = 12, sliderInput("MapYear", "Year:",
                                        min = 2011, 
                                        max = 2020,
                                        value = 2018, 
                                        sep = "")
            ),
            #Creates Box for world Map
            fluidRow(box(width = 12, leafletOutput("StandMap")))),
    
    #Litterfall Mass(All Stands)
    tabItem(tabName = "Litterfall_All",
            h1("Explore Litterfall Mass (Multiple Stands)"),
            #Input for Year 
            box(width = 3, sliderInput("Year", label = em("Select Date Range:",
                                                          style = "text-align:center;color black;font-size:100%"),
                                       min = min(Litterfall$litter.year),
                                       max = max(Litterfall$litter.year),
                                       value = c(min(Litterfall$litter.year), max(Litterfall$litter.year)),
                                       sep = "",
                                       step = 1)),
            #Input for Treatment
            box(width = 3, selectInput("Treatment", label = em("Select Treatment:",
                                                               style = "text-align:center;color black;font-size:100%"),
                                       unique(Litterfall$Treatment), multiple = TRUE, selected = c("N", "P"))),
            
            #Input for Stand (All)
            box(width = 3, selectizeInput("Stand_All", label = em("Select Stand:",
                                                                  style = "text-align:center;color black;font-size:100%"),
                                          choices = unique(Litterfall$Stand), multiple = TRUE, selected = "C1")),
            
            #Input Time Series (litterfall All)
            box(plotOutput("TS_LitterAll"), width = 12),
            #Input Box Plot (litterfall All)
            fluidRow(box(plotlyOutput("Box_LitterAll"), width = 12))), 
    
    #Litterfall mass (Specific Stand)  
    tabItem(tabName = "Litterfall",
            h1("Explore Litterfall Mass (Compare Stands)"),
            #Input for Year 
            box(width = 3, sliderInput("Year2", label = em("Select Date Range:",
                                                           style = "text-align:center;color black;font-size:100%"),
                                       min = min(Litterfall$litter.year),
                                       max = max(Litterfall$litter.year),
                                       value = c(min(Litterfall$litter.year), max(Litterfall$litter.year)),
                                       sep = "",
                                       step = 1)),
            #Input for Treatment (litterfall specific)
            box(width = 3, selectInput("Treatment_Specific", label = em("Select Treatment:",
                                                                        style = "text-align:center;color black;font-size:100%"),
                                       unique(Litterfall$Treatment), multiple = TRUE, selected = c("N", "P"))),
            #Input for Stand (litterfall specific)
            box(width = 3, selectizeInput("Stand_Specific", label = em("Select Stand:",
                                                                       style = "text-align:center;color black;font-size:100%"),
                                          choices = unique(Litterfall$Stand), multiple = TRUE, selected = "C1")),
            
            #Input Time Series (litterfall specific)
            box(plotOutput("timeseries_plot"), width = 12),
            #Input Box Plot (litterfall specific)
            fluidRow(box(plotlyOutput("litterfall_box"), width = 12))),
    
    # Litterfall by Species Visualization
    tabItem(tabName = "Litterfall_Species",
            h1("Explore Litterfall Mass by Species"),
            #Input Year
            box(width = 4, sliderInput("Species_Year", label = em("Select Date Range:",
                                                                  style = "text-align:center;color black;font-size:100%"),
                                       min = min(Litterfall$litter.year),
                                       max = max(Litterfall$litter.year),
                                       value = c(min(Litterfall$litter.year), max(Litterfall$litter.year)),
                                       sep = "",
                                       step = 1)),
            #Input for Treatment
            box(width = 4, selectInput("Species_Treatment", label = em("Select Treatment:",
                                                                       style = "text-align:center;color black;font-size:100%"),
                                       choices = unique(Litterfall2$Treatment), multiple = TRUE, selected = c("N", "P"))),
            
            #Input for Species
            box(width = 4, selectInput("Species", label = em("Select Species:",
                                                             style = "text-align:center;color black;font-size:100%"),
                                       choices = c("ASH", "BASP", "BASS", "BE", "HB", "OAK", "PC", "RM", "SM",
                                                   "STM", "WB","YB", "QASP", "GB", "MM", "RO", "SEEDS",
                                                   "TWIGS", "ASP", "VIB", "UNK", "NL"), multiple = TRUE, selected = "ASH")),
            #Input Boxplot
            box(plotlyOutput("litter_species_boxplot"), width = 12),
            
            #Input Species Key
            fluidRow(box(width = 12, helpText(strong("Species Key:"),
                                              "ASH: White Ash,
                                    BASP: Bigtooth Aspen,
                                    BASS: Basswood,
                                    BE: American Beech,
                                    HB: Hobblebush,
                                    OAK: Northern Red Oak,
                                    PC: Pin Cherry,
                                    RM: Red Maple,
                                    SM: Sugar Maple,
                                    STM: Striped Maple,
                                    WB: White Birch,
                                    YB: Yellow Birch,
                                    QASP: Quaking Aspen,
                                    GB: Gray Birch,
                                    MM: Mountain Maple,
                                    RO: Northern Red Oak
                                    SEEDS: Seeds,
                                    TWIGS: Twigs, 
                                    ASP: Bigtooth Aspen,
                                    VIB: Viburnum Lantanoides,
                                    UNK: Unknown,
                                    NL: Non Leaf")))),
    #Litterfall Grouped (All Stands)
    tabItem(tabName = "Litterfall_Grouped_All",
            h1("Explore Average Litterfall Mass (Multiple Stands)"),
            #Input for Year 
            box(width = 3, sliderInput("Grouped_Year1", label = em("Select Date Range:",
                                                                   style = "text-align:center;color black;font-size:100%"),
                                       min = min(Litterfall$litter.year),
                                       max = max(Litterfall$litter.year),
                                       value = c(min(Litterfall$litter.year), max(Litterfall$litter.year)),
                                       sep = "",
                                       step = 1)),
            #Input for Treatment (all stands)
            box(width = 3, selectInput("Grouped_Treatment", label = em("Select Treatment:",
                                                                       style = "text-align:center;color black;font-size:100%"),
                                       unique(Litterfall$Treatment), multiple = TRUE, selected = c("N", "P"))),
            
            #Input for Stand
            box(width = 3, selectizeInput("Grouped_Stand_All", label = em("Select Stand:",
                                                                          style = "text-align:center;color black;font-size:100%"),
                                          choices = unique(Litterfall$Stand), multiple = TRUE, selected = "C8")),
            
            #Grouped Litter TS plot (all stands)
            box(plotOutput("Grouped_TS_All"), width = 12),
            #Grouped Litter Boxplot (all stands)
            fluidRow(box(plotlyOutput("Grouped_Box_All"), width = 12))),
    
    #Litterfall Grouped (Specific Stand)
    tabItem(tabName = "Litterfall_Grouped",
            h1("Explore Average Litterfall Mass (Compare Stands)"),
            #Input for Year 
            box(width = 3, sliderInput("Grouped_Year2", label = em("Select Date Range:",
                                                                   style = "text-align:center;color black;font-size:100%"),
                                       min = min(Litterfall$litter.year),
                                       max = max(Litterfall$litter.year),
                                       value = c(min(Litterfall$litter.year), max(Litterfall$litter.year)),
                                       sep = "",
                                       step = 1)),
            #Input for Treatment
            box(width = 3, selectInput("Grouped_Treatment2", label = em("Select Treatment:",
                                                                        style = "text-align:center;color black;font-size:100%"),
                                       unique(Litterfall$Treatment), multiple = TRUE, selected = c("N", "P"))),
            #Input for Stand
            box(width = 3, selectizeInput("Grouped_Stand", label = em("Select Stand:",
                                                                      style = "text-align:center;color black;font-size:100%"),
                                          choices = unique(Litterfall$Stand), multiple = TRUE, selected = "C8")),
            
            #Input Time Series Plot
            box(plotOutput("grouped_timeseries_plot"), width = 12),
            #Input Box Plot
            fluidRow(box(plotlyOutput("grouped_litter_box"), width = 12))),
    
    #Input Litterfall Data Table
    tabItem(tabName = "Litterfall_Data",
            h1("Litterfall Data"),
            DT:: dataTableOutput("litterfalltable"),
    ),
    
    #Soil respiration flux (All Stands)
    tabItem(tabName = "Flux_AllStands",
            h1("Explore Soil Respiration Flux (Multiple Stands)"),
            
            #User input for date range
            box(width = 12, dateRangeInput("F_Date_All", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-11-10",
                                           min = "2008-07-01",
                                           max = "2020-11-10"),
            ),
            
            #User input for stand type (multiple stands)
            box(width = 6, selectInput("stand_all", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
                                         "JBM", "HBCa"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1")
            ),
            
            #User input for treatment type             
            box(width = 6, selectInput("F_Treatment_All", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
            ),
            
            #Flux TS plot (All Stands)
            box(plotOutput("Flux_TS_All"), width = 12),
            
            #Flux Boxplot output (All Stands)
            fluidRow(box(plotlyOutput("Flux_Box_All"), width = 12),
            )),    
    
    #Soil respiration flux (Specific Stand)
    tabItem(tabName = "Soil_Respiration",
            h1("Explore Soil Respiration Flux (Compare Stands)"),
            
            #User input for date range
            box(width = 12, dateRangeInput("date", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-11-10",
                                           min = "2008-07-01",
                                           max = "2020-11-10"),
            ),
            
            #User input for stand type 
            box(width = 6, selectInput("stand", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
                                         "JBM", "HBCa"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            
            #User input for treatment type             
            box(width = 6, selectInput("treatment", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
            ),
            
            #Making the TS plot
            box(plotOutput("flux_ts_plot"), width = 12),
            
            #Making the boxplot analysis
            fluidRow(box(plotlyOutput("soil_boxplot"), width = 12),
            )),
    #Soil Respiration Temperature (All Stands) 
    tabItem(tabName = "SoilTemp_AllStands",
            h1("Explore Soil Respiration Temperature (Multiple Stands)"),
            
            #User input for date range
            box(width = 12, dateRangeInput("T_Date_All", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-11-10",
                                           min = "2008-07-01",
                                           max = "2020-11-10"),
            ),
            
            #User input for stand type 
            box(width = 6, selectInput("temp_stand_all", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
                                         "JBM", "HBCa"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            
            #User input for treatment type             
            box(width = 6, selectInput("T_Treatment_All", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
            ),
            #Temperature TS Plot (All Stands)
            box(plotOutput("T_TS_All"), width = 12),
            
            #Temperature Boxplot (All Stands)
            fluidRow(box(plotlyOutput("T_Box_All"), width = 12),
            )),
    
    #Soil Respiration Temperature (Specific Stand)
    tabItem(tabName = "SoilRespiration_Temp",
            h1("Explore Soil Respiration Temperature (Compare Stands)"),
            
            #User input for date range
            box(width = 12, dateRangeInput("temp_date", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-11-10",
                                           min = "2008-07-01",
                                           max = "2020-11-10"),
            ),
            
            #User input for stand type 
            box(width = 6, selectInput("temp_stand", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
                                         "JBM", "HBCa"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            
            #User input for treatment type             
            box(width = 6, selectInput("temp_treatment", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
            ),
            #Temperature TS Plot (Specific Stand)
            box(plotOutput("temp_ts_plot"), width = 12),
            
            #Temperature Boxplot (Specific Stand)
            fluidRow(box(plotlyOutput("temp_boxplot"), width = 12),
            )),
    
    #Average Flux (All Stands)
    tabItem(tabName = "MeanFlux_AllStands",
            h1("Explore Average Soil Respiration Flux (Multiple Stands)"),
            
            box(width = 12, dateRangeInput("FMean_Date_All", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-11-10",
                                           min = "2008-07-01",
                                           max = "2020-11-10"),
            ),
            
            #User input for stand type 
            box(width = 6, selectInput("line_stand_all", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
                                         "JBM", "HBCa"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            
            #User input for treatment type             
            box(width = 6, selectInput("FMean_Treatment_All", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
            ),
            
            
            #Flux Mean TS Plot (All Stands)
            box(plotOutput("FMean_TS_All"), width = 12),
            #Flux Mean Boxplot (All Stands)
            fluidRow(box(plotlyOutput("FMean_Box_All"), width = 12)
            )),
    
    #Average Flux (Specific Stand)
    tabItem(tabName = "SoilRespiration_FluxMean",
            h1("Explore Average Soil Respiration (Compare Stands)"),
            
            box(width = 12, dateRangeInput("line_date", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-11-10",
                                           min = "2008-07-01",
                                           max = "2020-11-10"),
            ),
            
            #User input for stand type 
            box(width = 6, selectInput("line_stand", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
                                         "JBM", "HBCa"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            
            #User input for treatment type             
            box(width = 6, selectInput("line_treatment", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
            ),
            #Average Flux TS plot (Specific stand)
            box(plotOutput("line_ts_plot"), width = 12),
            #Average Flux Boxplot (Specific stand)
            fluidRow(box(plotlyOutput("line_boxplot"), width = 12)
            )),
    
    #Average Temperature (All Stands)
    tabItem(tabName = "TMean_AllStands",
            h1("Explore Average Soil Respiration Temperature (Multiple Stands)"),
            
            box(width = 12, dateRangeInput("TMean_Date_All", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-11-10",
                                           min = "2008-07-01",
                                           max = "2020-11-10"),
            ),
            
            #User input for stand type 
            box(width = 6, selectInput("TMean_stand_all", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
                                         "JBM", "HBCa"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            #User input for treatment type             
            box(width = 6, selectInput("TMean_Treatment_All", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
            ),
            #Making the soil resp TS plot
            box(plotOutput("TMean_TS_All"), width = 12),
            
            
            
            fluidRow(box(plotlyOutput("TMean_Box_All"), width = 12)
            )), 
    
    #Average Temperature (Specific Stand)
    tabItem(tabName = "SoilRespiration_TMean",
            h1("Explore Average Soil Respiration Temperature (Compare Stands)"),
            
            box(width = 12, dateRangeInput("TMean_date", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-11-10",
                                           min = "2008-07-01",
                                           max = "2020-11-10"),
            ),
            
            #User input for stand type 
            box(width = 6, selectInput("TMean_stand", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
                                         "JBM", "HBCa"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            
            #User input for treatment type             
            box(width = 6, selectInput("TMean_treatment", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
            ),
            #Making the soil resp TS plot
            box(plotOutput("TMean_ts_plot"), width = 12),
            
            
            
            fluidRow(box(plotlyOutput("TMean_boxplot"), width = 12)
            )),
    #Litterfall Bivariate Tab
    tabItem(tabName = "Litterfall_BiVar", 
            h1("Litterfall Bivariate Plot"),
            box(plotlyOutput('plot'), width = 12),
            hr(),
            fluidRow(column(width = 12, offset = 1,
                            box(width = 3, selectInput("x", "X-axis:",
                                                       c("litter.year", "Treatment", "Basket", "Plot", "Season", "Stand", "Site", "whole.mass", 
                                                          "ASH", "BASP", "BASS", "BE", "HB", "OAK", "PC", "RM", "SM", "STM", "WB", "YB", "QASP", 
                                                          "GB", "MM", "RO", "SEEDS", "TWIGS", "ASP", "VIB", "UNK", "NL", "N", "P", "mass.per.unit.area"),
                                                       selectize = TRUE, multiple = FALSE, selected = "litter.year")),
                            box(width = 3, selectInput('y', 'Y-axis:', 
                                        c("litter.year", "Treatment", "Basket", "Plot", "Season", "Stand", "Site", "whole.mass", 
                                          "ASH", "BASP", "BASS", "BE", "HB", "OAK", "PC", "RM", "SM", "STM", "WB", "YB", "QASP", 
                                          "GB", "MM", "RO", "SEEDS", "TWIGS", "ASP", "VIB", "UNK", "NL", "N", "P", "mass.per.unit.area"),
                                        selectize = TRUE, multiple = FALSE, selected = "whole.mass")),
                            box(width= 3, selectInput('color', 'Color:', 
                                        c("None", "litter.year", "Treatment", "Basket", "Plot", "Season", "Stand", "Site", "whole.mass", 
                                          "ASH", "BASP", "BASS", "BE", "HB", "OAK", "PC", "RM", "SM", "STM", "WB", "YB", "QASP", 
                                          "GB", "MM", "RO", "SEEDS", "TWIGS", "ASP", "VIB", "UNK", "NL", "N", "P", "mass.per.unit.area"),
                                        selectize = TRUE, multiple = FALSE, selected = "None"))
                            
                            )
            ),
            fluidRow(column(width = 12, h4("Litterfall Plot Explorer"),
                   
                   box(width = 12, sliderInput("Bivar_Ldate", "Year Range:",
                                               min = min(dataset$litter.year), 
                                               max = max(dataset$litter.year),
                                               value = c(min(dataset$litter.year), max(dataset$litter.year)), 
                                               step = 1, 
                                               sep = ""),
                       br(),
                       checkboxInput('jitter', 'Jitter'),
                       checkboxInput('smooth', 'Smooth')
                       
                   )))
            
            
            
            ),
    
    #Soil Respiration Bivariate Tab
    tabItem(tabName = "SoilRespiration_BiVar", 
            h1("Soil Respiration Bivariate Plot"),
            box(plotlyOutput('plot1'), width = 12),
            hr(),
        
            fluidRow(column(12, offset = 1,
                            box(width = 3, selectInput("x1", "X-axis:",
                                                       c("date", "stand", "flux", "temperature", "treatment"),
                                                       selectize = TRUE, multiple = FALSE, selected = "date")),
                            box(width = 3, selectInput("y1", "Y-axis:",
                                                       c("date", "stand", "flux", "temperature", "treatment"),
                                                       selectize = TRUE, multiple = FALSE, selected = "flux")),
                            box(width = 3, selectInput("color1", "Color:",
                                                       c("None", "date", "stand", "flux", "temperature", "treatment"),
                                                       selectize = TRUE, multiple = FALSE, selected = "None")),
                            
                            
            )),
            fluidRow(column(12, h4("Soil Respiration Plot Explorer"),
                            box(width = 12, sliderInput("date", "Select date range:",
                                                        min = min(dataset1$date), 
                                                        max = max(dataset1$date),
                                                        value = c(min(dataset1$date), max(dataset1$date)),
                                                        step = 1,
                                                        sep = "/"),
                            br(),
                            checkboxInput('jitter1', 'Jitter'),
                            checkboxInput('smooth1', 'Smooth')
            )))
            
            
            
            ),
    
    #Explore Soil Respiration Data
    tabItem(tabName = "SoilRespiration_Data",
            h1("Soil Respiration Data"),
            DT:: dataTableOutput("soilresptable")
    )
  ))
)


server <- function(input, output) {
  #Uses sites and treatment from input above to create cirlce markers for each specific Stands on interactive map
  output$StandMap <- renderLeaflet({
    
    StandSelect <- input$MapSite
    TreatmentSelect <- input$MapTreatment
    YearSelect <- input$MapYear
    
    GroupedLitterMerge <- GroupedLitterMerge %>%
      filter(Stand %in% StandSelect) %>% 
      filter(Treatment %in% TreatmentSelect) %>% 
      filter(litter.year %in% YearSelect) %>% 
      filter(!is.na(whole.mass))
    
    LitterAverage <- GroupedLitterMerge %>% 
      filter(Stand %in% StandSelect) %>% 
      filter(Treatment %in% TreatmentSelect) %>%
      filter(litter.year %in% YearSelect) %>% 
      filter(!is.na(whole.mass))
    
    LitterAverage <- LitterAverage %>% 
      group_by(litter.year, Stand, Treatment, Plot) %>% 
      summarize(AvgWholeMass = mean(whole.mass),
                AvgLat = mean(Lat),
                AvgLong = mean(Long)) %>% 
      mutate(popup_info = paste("Plot:", Plot, "<br/>",
                                "Year:", litter.year, "<br/>",
                                "Treatment:", Treatment, "<br/>",
                                "Average Whole Mass", AvgWholeMass))
    
    if (input$average)
      leaflet()%>% 
      addTiles()%>%
      addCircleMarkers(data = LitterAverage,
                       lat = ~AvgLat, 
                       lng = ~AvgLong,
                       radius = 3,
                       popup = ~popup_info,
                       color = ~pal(AvgWholeMass)
      ) %>% 
      addLegend(position = "bottomright",
                pal = pal,
                values = LitterMerge$whole.mass,
                bins = 6,
                title = "Average Whole Mass per Year",
                opacity = 1)
    else
      leaflet()%>% 
      addTiles()%>%
      addCircleMarkers(data = GroupedLitterMerge,
                       lat = ~Lat, 
                       lng = ~Long,
                       radius = 3,
                       popup = ~popup_info,
                       color = ~pal(whole.mass)
      ) %>% 
      addLegend(position = "bottomright",
                pal = pal,
                values = LitterMerge$whole.mass,
                bins = 6,
                title = "Average Whole Mass per Year",
                opacity = 1)
    
  })
  
  
  #Flux TS Plot (All Stands) 
  output$Flux_TS_All <- renderPlot({
    startdate <- input$F_Date_All[1]
    enddate <- input$F_Date_All[2]
    treatmentselection <- input$F_Treatment_All
    standselection <- input$stand_all
    
    CleanSoilResp %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      mutate(date = as.factor(date))%>%
      ggplot(aes(x = date, y = flux))+
      geom_boxplot(aes(x = date, y = flux, color = treatment), position = position_dodge(0.8))+
      geom_dotplot(aes(x = date, y = flux, fill = treatment), position = position_dodge(0.8), 
                   binaxis = "y")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
      labs(title = "Time Series: CO2 Flux vs. Time", 
           x = "Date", 
           y = "CO2 efflux per unit area (μg CO2/m2/s)")
    
  })
  
  #Flux Boxplot (All Stands)
  output$Flux_Box_All <- renderPlotly({
    startdate <- input$F_Date_All[1]
    enddate <- input$F_Date_All[2]
    treatmentselection <- input$F_Treatment_All
    standselection <- input$stand_all
    
    CleanSoilResp %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = flux, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 0.8)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_bw()+
      labs(title = "Boxplot: CO2 Flux vs. Treatment Type", 
           x = "Treatment", 
           y = "CO2 efflux per unit area (μg CO2/m2/s)")
  })
  
  #Flux TS Plot (Specific Stand) 
  output$flux_ts_plot <- renderPlot({
    startdate <- input$date[1]
    enddate <- input$date[2]
    standselection <- input$stand
    treatmentselection <- input$treatment
    
    CleanSoilResp %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      mutate(date = as.factor(date))%>%
      ggplot(aes(x = date, y = flux))+
      geom_boxplot(aes(x = date, y = flux, color = treatment), position = position_dodge(0.8))+
      geom_dotplot(aes(x = date, y = flux, fill = treatment), position = position_dodge(0.8), 
                   binaxis = "y")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
      labs(title = "Time Series: CO2 Flux vs. Time", 
           x = "Date", 
           y = "CO2 efflux per unit area (μg CO2/m2/s)")+
      facet_wrap(facets = "stand", scales = "free_y", ncol = 1)
  })
  
  #Flux Boxplot (Specific Stand)
  output$soil_boxplot <- renderPlotly({
    startdate <- input$date[1]
    enddate <- input$date[2]
    standselection <- input$stand
    treatmentselection <- input$treatment
    
    CleanSoilResp %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = flux, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 0.8)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_bw()+
      labs(title = "Boxplot: CO2 Flux vs. Treatment Type", 
           x = "Treatment", 
           y = "CO2 efflux per unit area (μg CO2/m2/s)")+
      facet_wrap(facets = "stand", ncol = 2)
  })
  
  output$soilresptable = DT::renderDataTable({
    CleanSoilResp
  })
  #Litterfall Mass (All Stands) time series plot
  output$TS_LitterAll <- renderPlot({
    min <- input$Year[1]
    max <- input$Year[2]
    Treatmentselection <- input$Treatment
    Standselection <- input$Stand_All
    
    
    Litterfall %>%
      filter(litter.year >= min & litter.year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
      mutate(litter.year = as.factor(litter.year)) %>%
      ggplot(aes(x = litter.year, y = whole.mass)) +
      geom_boxplot(aes(x = litter.year, y = whole.mass, color = Treatment), position = position_dodge(0.8), lwd = 1)+
      geom_dotplot(aes(x = litter.year, y = whole.mass, fill = Treatment), position = position_dodge(0.8), 
                   binaxis = "y")+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
      labs(title ="Time Series: Litterfall Mass vs. Time",
           x = "Litter Year",
           y = "Mass (g litter /m2)")
  })
  
  #Litterfall Mass (All Stands) box plot
  output$Box_LitterAll <- renderPlotly({
    min <- input$Year[1]
    max <- input$Year[2]
    Treatmentselection <- input$Treatment
    Standselection <- input$Stand_All
    
    Litterfall %>%
      filter(litter.year >= min & litter.year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
      ggplot(aes(x=Treatment, y=whole.mass, fill = Treatment)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 5, lwd = 0.8)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme_bw() +
      labs(title ="Boxplot: Litterfall Mass vs. Treatment Type",
           x = "Treatment",
           y = "Mass (g litter /m2)") 
  })
  
  #Litterfall Mass TS plot (Specific Stand)
  output$timeseries_plot <- renderPlot({
    min <- input$Year2[1]
    max <- input$Year2[2]
    Treatmentselection <- input$Treatment_Specific
    Standselection <- input$Stand_Specific
    
    
    Litterfall %>%
      filter(litter.year >= min & litter.year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
      mutate(litter.year = as.factor(litter.year)) %>%
      ggplot(aes(x = litter.year, y = whole.mass)) +
      geom_boxplot(aes(x = litter.year, y = whole.mass, color = Treatment), position = position_dodge(0.8), lwd = 1)+
      geom_dotplot(aes(x = litter.year, y = whole.mass, fill = Treatment), position = position_dodge(0.8), 
                   binaxis = "y")+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
      labs(title ="Time Series: Litterfall Mass vs. Time",
           x = "Litter Year",
           y = "Mass (g litter /m2)")+
      facet_wrap(facets = "Stand", scales = "free_y", ncol = 1)
  })
  
  #Litterfall Mass Boxplot (Specific Stand)
  output$litterfall_box <- renderPlotly({
    min <- input$Year2[1]
    max <- input$Year2[2]
    Treatmentselection <- input$Treatment_Specific
    Standselection <- input$Stand_Specific
    
    Litterfall %>%
      filter(litter.year >= min & litter.year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
      ggplot(aes(x=Treatment, y=whole.mass, fill = Treatment)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 5, lwd = 0.8)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme_bw() +
      labs(title ="Boxplot: Litterfall Mass vs. Treatment Type",
           x = "Treatment",
           y = "Mass (g litter /m2)") +
      facet_wrap(facets = "Stand", ncol = 2)
  })
  
  #Litterfall By Species BoxPlot Output
  output$litter_species_boxplot <- renderPlotly({
    min <- input$Species_Year[1]
    max <- input$Species_Year[2]
    Treatmentselection <- input$Species_Treatment
    Speciesselection <- input$Species
    
    GroupedLitter_Species %>%
      filter(litter.year >= min & litter.year <= max) %>%
      filter(name %in% Speciesselection & Treatment %in% Treatmentselection) %>%
      mutate(litter.year = as.factor(litter.year)) %>%
      ggplot(aes(x = litter.year, y = value)) +
      geom_point(aes(x = litter.year, y = value, group = Treatment, color = Treatment)) + 
      geom_line(aes(x = litter.year, y = value, color = Treatment, group = Treatment)) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
      labs(title ="Grouped Time Series: Litterfall Mass vs. Time",
           x = "Litter Year",
           y = "Mass (g litter /m2)")+
      facet_wrap(facets = "name", scales = "free_y", ncol = 1)
    
    
  })
  
  #Grouped Litterfall TS plot (All Stands)
  output$Grouped_TS_All <- renderPlot({
    min <- input$Grouped_Year1[1]
    max <- input$Grouped_Year1[2]
    Treatmentselection <- input$Grouped_Treatment
    Standselection <- input$Grouped_Stand_All
    
    GroupedLitter_Specific %>%
      filter(litter.year >= min & litter.year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
      group_by(litter.year, Treatment) %>%
      summarize(Mass = mean(Mass))%>% 
      mutate(litter.year = as.factor(litter.year)) %>%
      ggplot(aes(x = litter.year, y = Mass)) +
      geom_point(aes(x = litter.year, y = Mass, group = Treatment, color = Treatment)) + 
      geom_line(aes(x = litter.year, y = Mass, color = Treatment, group = Treatment)) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
      labs(title ="Average Litterfall Mass vs. Time",
           x = "Litter Year",
           y = "Mass (g litter /m2)")
    
  })
  
  #Grouped Litterfall Boxplot (All Stands)
  output$Grouped_Box_All<- renderPlotly({
    min <- input$Grouped_Year1[1]
    max <- input$Grouped_Year1[2]
    Treatmentselection <- input$Grouped_Treatment
    Standselection <- input$Grouped_Stand_All
    
    GroupedLitter_Specific %>%
      filter(litter.year >= min & litter.year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
      ggplot(aes(x = Treatment, y = Mass, fill = Treatment)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 0.8)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
      theme_bw() +
      labs(title ="Boxplot: Average Litterfall Mass vs. Treatment Type",
           x = "Treatment",
           y = "Mass (g litter /m2)")
  })
  #Litterfall Grouped Time series plot (Specific Stand)
  output$grouped_timeseries_plot <- renderPlot({
    min <- input$Grouped_Year2[1]
    max <- input$Grouped_Year2[2]
    Treatmentselection <- input$Grouped_Treatment2
    Standselection <- input$Grouped_Stand
    
    GroupedLitter_Specific %>%
      filter(litter.year >= min & litter.year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
      mutate(litter.year = as.factor(litter.year)) %>%
      ggplot(aes(x = litter.year, y = Mass)) +
      geom_point(aes(x = litter.year, y = Mass, group = Treatment, color = Treatment)) + 
      geom_line(aes(x = litter.year, y = Mass, color = Treatment, group = Treatment)) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
      labs(title ="Grouped Time Series: Litterfall Mass vs. Time",
           x = "Litter Year",
           y = "Mass (g litter /m2)")+
      facet_wrap(facets = "Stand", scales = "free_y", ncol = 1)
    
  })
  
  #Grouped Litterfall Boxplot(Specific Stand)
  output$grouped_litter_box<- renderPlotly({
    min <- input$Grouped_Year2[1]
    max <- input$Grouped_Year2[2]
    Treatmentselection <- input$Grouped_Treatment2
    Standselection <- input$Grouped_Stand
    
    GroupedLitter_Specific %>%
      filter(litter.year >= min & litter.year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
      ggplot(aes(x = Treatment, y = Mass, fill = Treatment)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 0.8)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
      theme_bw() +
      labs(title ="Boxplot: Average Litterfall Mass vs. Treatment Type",
           x = "Treatment",
           y = "Mass (g litter /m2)")+
      facet_wrap(facets = "Stand", ncol = 2)
  })
  
  #Litterfall data table output
  output$litterfalltable = DT::renderDataTable(
    LitterTable, options = list(pageLength = 20)
    
  )
  
  #Soil Temp TS plot (All Stands)
  output$T_TS_All <- renderPlot({
    startdate <- input$T_Date_All[1]
    enddate <- input$T_Date_All[2]
    standselection <- input$temp_stand_all
    treatmentselection <- input$T_Treatment_All
    
    CleanSoilResp %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      mutate(date = as.factor(date))%>%
      ggplot(aes(x = date, y = temperature))+
      geom_boxplot(aes(x = date, y = temperature, color = treatment), position = position_dodge(0.8))+
      geom_dotplot(aes(x = date, y = temperature, fill = treatment), position = position_dodge(0.8), 
                   binaxis = "y")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
      labs(title = "Time Series: Temperature vs Time", 
           x = "Date", 
           y = "Temperature (°C)")
    
  })  
  #Soil Temp Boxplot (All Stands)
  output$T_Box_All <- renderPlotly({
    startdate <- input$T_Date_All[1]
    enddate <- input$T_Date_All[2]
    standselection <- input$temp_stand_all
    treatmentselection <- input$T_Treatment_All
    
    CleanSoilResp %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = temperature, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 0.8)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_bw()+
      labs(title = "Boxplot: Temperature vs. Treatment Type", 
           x = "Treatment", 
           y = "Temperature (°C)")
    
  }) 
  #Soil Temp TS plot (Specific Stand)
  output$temp_ts_plot <- renderPlot({
    startdate <- input$temp_date[1]
    enddate <- input$temp_date[2]
    standselection <- input$temp_stand
    treatmentselection <- input$temp_treatment
    
    CleanSoilResp %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      mutate(date = as.factor(date))%>%
      ggplot(aes(x = date, y = temperature))+
      geom_boxplot(aes(x = date, y = temperature, color = treatment), position = position_dodge(0.8))+
      geom_dotplot(aes(x = date, y = temperature, fill = treatment), position = position_dodge(0.8), 
                   binaxis = "y")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
      labs(title = "Time Series: Temperature vs. Time", 
           x = "Date", 
           y = "Temperature (°C)")+
      facet_wrap(facets = "stand", scales = "free_y", ncol = 1)
    
  })
  #Soil Temp Boxplot (Specific Stand) 
  output$temp_boxplot <- renderPlotly({
    startdate <- input$date[1]
    enddate <- input$date[2]
    standselection <- input$temp_stand
    treatmentselection <- input$treatment
    
    CleanSoilResp %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = temperature, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 0.8)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_bw()+
      labs(title = "Time Series: Temperature vs Treatment Type", 
           x = "Treatment", 
           y = "Temperature (°C)")+
      facet_wrap(facets = "stand", ncol = 2)
    
  })
  
  #Average Flux TS Plot (All Stands)
  output$FMean_TS_All <- renderPlot({
    startdate <- input$FMean_Date_All[1]
    enddate <- input$FMean_Date_All[2]
    standselection <- input$line_stand_all
    treatmentselection <- input$FMean_Treatment_All
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = date, y = flux))+
      geom_point(aes(x = date, y = flux, group = treatment, color = treatment))+
      geom_path(aes(x = date, y = flux, group = treatment, color = treatment))+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
      labs(title = "Time Series: Average CO2 Flux vs. Time", 
           x = "Date", 
           y = "Average CO2 efflux per unit area (μg CO2/m2/s)")
    
  })
  #Average Flux Boxplot (All Stands)
  output$FMean_Box_All <- renderPlotly({
    startdate <- input$FMean_Date_All[1]
    enddate <- input$FMean_Date_All[2]
    standselection <- input$line_stand_all
    treatmentselection <- input$FMean_Treatment_All
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = temperature, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 0.8)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_bw()+
      labs(title = "Boxplot: Average CO2 Flux vs Treatment Type", 
           x = "Treatment", 
           y = "Average CO2 efflux per unit area (μg CO2/m2/s)")
    
  })
  #Average Flux TS Plot (Specific Stand) 
  output$line_ts_plot <- renderPlot({
    startdate <- input$line_date[1]
    enddate <- input$line_date[2]
    standselection <- input$line_stand
    treatmentselection <- input$line_treatment
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = date, y = flux))+
      geom_point(aes(x = date, y = flux, group = treatment, color = treatment))+
      geom_path(aes(x = date, y = flux, group = treatment, color = treatment))+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
      labs(title = "Time Series: Average CO2 Flux vs. Time", 
           x = "Date", 
           y = "Average CO2 efflux per unit area (μg CO2/m2/s)")+
      facet_wrap(facets = "stand", scales = "free_y", ncol = 1)
    
  })
  
  #Average Flux Boxplot (Specific Stand) 
  output$line_boxplot <- renderPlotly({
    startdate <- input$line_date[1]
    enddate <- input$line_date[2]
    standselection <- input$line_stand
    treatmentselection <- input$line_treatment
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = temperature, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 0.8)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_bw()+
      labs(title = "Boxplot: Average CO2 Flux vs. Treatment Type", 
           x = "Treatment", 
           y = "Average CO2 efflux per unit area (μg CO2/m2/s)")+
      facet_wrap(facets = "stand", ncol = 2)
    
  })
  
  #Average Temperature TS plot (All Stands)
  output$TMean_TS_All <- renderPlot({
    startdate <- input$TMean_Date_All[1]
    enddate <- input$TMean_Date_All[2]
    treatmentselection <- input$TMean_Treatment_All
    standselection <- input$TMean_stand_all
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = date, y = teamperature))+
      geom_point(aes(x = date, y = temperature, group = treatment, color = treatment))+
      geom_line(aes(x = date, y = temperature, group = treatment, color = treatment))+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
      labs(title = "Time Series: Average Temperature vs. Time", 
           x = "Date", 
           y = "Average Temperature (°C)")
  })
  #Average Temperature Boxplot (All Stands)
  output$TMean_Box_All <- renderPlotly({
    startdate <- input$TMean_Date_All[1]
    enddate <- input$TMean_Date_All[2]
    treatmentselection <- input$TMean_Treatment_All
    standselection <- input$TMean_stand_all
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = temperature, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 1)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 90, hjust = 0))+
      theme_bw()+
      labs(title = "Boxplot: Average Temperature vs Treatment Type", 
           x = "Treatment", 
           y = "Average Temperature (°C)")
    
  }) 
  #Average Temperature TS plot (Specific Stand)
  output$TMean_ts_plot <- renderPlot({
    startdate <- input$TMean_date[1]
    enddate <- input$TMean_date[2]
    standselection <- input$TMean_stand
    treatmentselection <- input$TMean_treatment
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = date, y = teamperature))+
      geom_point(aes(x = date, y = temperature, group = treatment, color = treatment))+
      geom_line(aes(x = date, y = temperature, group = treatment, color = treatment))+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
      labs(title = "Time Series: Average Temperature vs Time", 
           x = "Date", 
           y = "Average Temperature (°C)")+
      facet_wrap(facets = "stand", scales = "free_y", ncol = 1)
    
    
  })
  #Average Temperature Boxplot (Specific Stand) 
  output$TMean_boxplot <- renderPlotly({
    startdate <- input$TMean_date[1]
    enddate <- input$TMean_date[2]
    standselection <- input$TMean_stand
    treatmentselection <- input$TMean_treatment
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = temperature, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 1)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 90, hjust = 0))+
      theme_bw()+
      labs(title = "Boxplot: Average Temperature vs Treatment Type", 
           x = "Treatment", 
           y = "Average Temperature (°C)")+
      facet_wrap(facets = "stand", ncol = 2)
    
    
  })
  
  #Litterfall Bivariate ggplot code
  dataset <- reactive({
    dataset <- as.data.frame(Litterfall[sample(nrow(Litterfall), input$Bivar_Ldate),])
  })
  
  output$plot <- renderPlotly({
    
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    if (input$jitter)
      p <- p + geom_jitter()
    if (input$smooth)
      p <- p + geom_smooth()
    
    p
  })
  
  #Soil Respiration Bivariate ggplot code
  dataset1 <- reactive({
    CleanSoilResp[sample(nrow(CleanSoilResp), ),]
  })
  output$plot1 <- renderPlotly({
    
    p1 <- ggplot(dataset1(), aes_string(x=input$x1, y=input$y1)) + geom_point()
    
    if (input$color1 != 'None')
      p1 <- p1 + aes_string(color=input$color1)
    if (input$jitter1)
      p1 <- p1 + geom_jitter()
    if (input$smooth1)
      p1 <- p1 + geom_smooth()
    
    p1
    
  })
}


shinyApp(ui, server)