---
title: "Untitled"
runtime: shiny
output: html_document
---
  

## Import/install necessary packages:
knitr::opts_chunk$set(echo = TRUE)
library(lubridate)
library(plyr)
library(stringr)
library(sp)
require(plyr)
require(dplyr)
require(tidyr)
library(ggmap)
library(sp)
library(maps)
library(maptools)
library(neuralnet)
library(devtools)
library(plotly)
library(shiny)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
library(NeuralNetTools)
library(shiny)
library(leaflet)
library(RColorBrewer)

## Import the Terrorism and Temperature data from split csv's:

## Aggregating .csv's
read <- function(path) {
  read.csv(path,header=F,sep=",", fileEncoding="latin1")
}

## Load data from .csv's
load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read)
  rbind.fill(tables)
}

## master data tables
terrorismData <- load_data("./TerrorismData")
temperatureData <- load_data("./TemperatureData")

## make first row the header
names(terrorismData) <- as.matrix(terrorismData[1, ])
names(temperatureData) <- as.matrix(temperatureData[1, ])

terrorismData <- terrorismData[-1, ]
temperatureData <- temperatureData[-1, ]

terrorismData[] <- lapply(terrorismData, function(x) type.convert(as.character(x)))
temperatureData[] <- lapply(temperatureData, function(x) type.convert(as.character(x)))

#### Lat/Long -> Clustered Regions
#The function below takes a vector of Latitudes and a vector of Longitudes and runs Hartigan and Wong's (1979) k-means clustering algorithm to cluster the latitude and longitudes into regions. This saves us the hassle of converting cities or lat/long coordinates to US states/territories. The function is also easily scalable for non-US lat/long coordinates.

cluster <- function(latVector, longVector) {
  
  set.seed(4747)                                                ## Set seed for reproducibility
  
  lat_name <- "realLat"                                         ## Name realLat column
  long_name <- "realLong"                                       ## Name realLong column
  latlong <- data.frame(latVector, longVector)                  ## Initialize new data frame with lat and long params
  names(latlong) <- c(lat_name, long_name)                      ## Apply names to frame
  
  latlongCluster <- kmeans(latlong, centers = 20, nstart = 10)  ## Run k-means clustering with centers centroids and nstart CV's
  
  group_lat <- "GroupLat"                                       ## Name GroupLat column
  group_long <- "GroupLong"                                     ## Name GroupLong column
  latlongCenterData <- data.frame(latlongCluster$centers)       ## Initialize data frame with centers
  names(latlongCenterData) <- c(group_lat, group_long)          ## Apply names to frame
  
  latlongCenterData$clusterCenter <- 
    apply(latlongCenterData, 1, paste, collapse = ",")          ## Combine latlongCenterData columns into a comma seperated value
  
  group_name <- "GroupNumber"                                   ## Name GroupNumber column
  resultData <- data.frame(latlongCluster$cluster)              ## Initialize data frame with kmeans clusters (region values)
  names(resultData) <- c(group_name)                            ## Apply names to frame
  
  resultData <- resultData %>%
    mutate(LatLongCenter = 
             latlongCenterData$clusterCenter[GroupNumber])      ## Get the appropriate centroid location for each region number
  
  resultData$cluster <- 
    apply(resultData, 1, paste, collapse = ",")                 ## Create a new column `cluster` columns collapsed together
  
  return(resultData$cluster)                                    ## Return the comma seperated cluster data as a vector
}

## Get and clean only US terrorist attacks
usaTerrorismData <- terrorismData %>%
  filter(country == 217)  %>%                                        ## Filter for country == USA
  unite(date1, iyear,imonth,iday, sep="-") %>%                       ## Unite seperate year, month, and day columns into one (y-m-d)
  mutate(dateLub = ymd(date1)) %>%                                   ## Convert date into 'Lubridate' (YYYY-MM-DD)
  filter(!is.na(dateLub))  %>%                                       ## Filter out observations with missing dates (failed to parse)
  mutate(city_char = str_to_lower(as.character(city))) %>%           ## Create new city column as character type
  mutate(month = month(as.POSIXlt(dateLub, format="%d/%m/%Y"))) %>%  ## Create a new month column
  mutate(year = year(as.POSIXlt(dateLub, format="%d/%m/%Y"))) %>%    ## Create a new year column
  mutate(month_year = paste(month, year))                            ## Create new month, year column (M YYYY)

## Uncomment to view head and dimension of cleaned usaTerrorismData
#head(usaTerrorismData)
#dim(usaTerrorismData)


## Get and clean only US temperatures
usaTemperatureData <- temperatureData %>%
  filter(Country == "United States") %>%                             ## Filter for country == USA
  mutate(dateLub = ymd(dt)) %>%                                      ## Convert date into 'Lubridate' (YYYY-MM-DD)
  filter(!is.na(dateLub)) %>%                                        ## Filter out observations with missing dates (failed to parse)
  filter(dateLub >= as.Date("1970-01-01")) %>%                       ## Filter only for data after 1/1/1970 (when Terrorism data begins)
  mutate(city_char = str_to_lower(as.character(City))) %>%           ## Create new city column as character type
  mutate(month = month(as.POSIXlt(dateLub, format="%d/%m/%Y"))) %>%  ## Create a new month column
  mutate(year = year(as.POSIXlt(dateLub, format="%d/%m/%Y"))) %>%    ## Create a new year column
  mutate(month_year = paste(month, year)) %>%                        ## Create new month, year column (M YYYY)
  mutate(AverageTemperature = 
           as.numeric(as.character(AverageTemperature)))  %>%        ## Type of AverageTemperature is numeric
  filter(!is.na(AverageTemperature))                                 ## Remove observations with missing temperatures


## Uncomment to view head and dimension of cleaned usaTemperatureData
#head(usaTemperatureData)
#dim(usaTemperatureData)


## Merge temperature data with terrorism data by city_char and month_year
## As a result, for every city we should also have every month since 1970 (exlcuding missing data)
## (Important note: There are ~200 unique cities in our temperature data and ~700 in our terrorism data.
## As a result, when merging on cities, we lose about half of our terrorist attacks.)
mergedTempTerrorismData <- left_join(usaTemperatureData, usaTerrorismData, by=c("city_char", "month_year"))

## Uncomment to view head and dimension of mergedTempTerrorismData
#head(mergedTempTerrorismData)
#dim(mergedTempTerrorismData)


## Clean mergedTempTerrorismData 
mergedTempTerrorismData <- mergedTempTerrorismData[,c("month_year", "month.x", "year.x",
                                                      "city_char", "AverageTemperature", 
                                                      "Latitude", "Longitude", "nkill", 
                                                      "eventid")] %>%                       ## Select explanatory variables
  filter(year.x < 2016) %>%                                                                 ## Filter for a couple typos in years
  mutate(isAttack = as.integer(!is.na(eventid))) %>%                                        ## Create binary isAttack variable
  mutate(nkill = ifelse(is.na(nkill), 0, nkill)) %>%                                        ## Convert NA's to 0's in nkill
  select(-eventid) %>%                                                                      ## Remove eventid variable 
  mutate(realLong = -1* as.numeric(str_sub(Longitude, 1, -2))) %>%                          ## Parse Longitude column
  mutate(realLat = as.numeric(str_sub(Latitude, 1, -2))) %>%                                ## Parse Latitude column
  filter(!is.na(realLat)) %>%                                                               ## Remove missing Latitudes
  filter(!is.na(realLong)) %>%                                                              ## Remove missing Longitudes
  mutate(region = cluster(realLat, realLong)) %>%                                           ## Get comma seperated region cluster (r,lat,long)
  separate(region, sep=",", into=c("Region", "RegionLat", "RegionLong"), convert=TRUE) %>%  ## Seperate comma-seperated data into the 3 columns
  select(-Latitude, -Longitude)                                                             ## Remove Latitude and Longitude char vectors



mergedTempTerrorismData <- mergedTempTerrorismData %>%
  group_by(Region, month.x) %>%                                                             ## Group by clustered Region and month
  mutate(AvgTempReg = mean(AverageTemperature)) %>%                                         ## Compute average temperature per month per region
  mutate(TempDifference = abs(AvgTempReg - AverageTemperature))                             ## Compute temperature diff b/w mean and actual


mergedTempTerrorismData <- mergedTempTerrorismData %>%
  group_by(Region, year.x) %>%                                                              ## Group by clustered Region and year
  mutate(AttacksInYearInReg = sum(isAttack)) %>%                                            ## Sum the # of attacks in region in year
  mutate(nKilledInYearInReg = sum(nkill)) %>%                                               ## Sum the # of kills in region in year
  group_by(Region) %>%                                                                      ## Regroup by just Region
  mutate(AvgAttacksPerYearInReg = mean(AttacksInYearInReg)) %>%                             ## Compute mean attacks per year in every region
  mutate(AvgNKilledPerYearInReg = mean(nKilledInYearInReg)) %>%                             ## Compute mean # killed per year in every region
  group_by(city_char) %>%                                                                   ## Regroup by just city_char
  mutate(AttacksPrevYearInReg = 
           ifelse(year.x == 1970, AvgAttacksPerYearInReg, lag(AttacksInYearInReg,12))) %>%  ## Compute # of attacks in prev. year in region
  mutate(nKilledPrevYearInReg = 
           ifelse(year.x == 1970, AvgNKilledPerYearInReg, lag(nKilledInYearInReg,12))) %>%  ## Compute # killed in prev. year in region
  ungroup(city_char)                                                                        ## Ungroup city_char grouping





## Get and clean only US terrorist attacks
usaTerrorismData <- terrorismData %>%
  filter(country == 217)  %>%                                        ## Filter for country == USA
  unite(date1, iyear,imonth,iday, sep="-") %>%                       ## Unite seperate year, month, and day columns into one (y-m-d)
  mutate(dateLub = ymd(date1)) %>%                                   ## Convert date into 'Lubridate' (YYYY-MM-DD)
  filter(!is.na(dateLub))  %>%                                       ## Filter out observations with missing dates (failed to parse)
  mutate(city_char = str_to_lower(as.character(city))) %>%           ## Create new city column as character type
  mutate(month = month(as.POSIXlt(dateLub, format="%d/%m/%Y"))) %>%  ## Create a new month column
  mutate(year = year(as.POSIXlt(dateLub, format="%d/%m/%Y"))) %>%    ## Create a new year column
  mutate(month_year = paste(month, year))                            ## Create new month, year column (M YYYY)



interactiveViz <- usaTerrorismData %>%                        # Create dataset to visualize          
  filter(provstate != "Puerto Rico") %>%                     # Remove Puerto Rico
  filter(provstate != "Alaska") %>%                          # Remove Alaska
  filter(provstate != "Hawaii")  %>%                         # Remove Hawaii
  filter(longitude < 0) %>%                                  # Remove Locations East of Prime Meridien
  filter(!is.na(nkill))                                      # Get rid of NA's

interactiveViz <- interactiveViz %>%          
  group_by(provstate, year, latitude, longitude) %>%    # group attacks by year, state, lat, and long
  summarize(totalKilled = sum(nkill))               # sum the number of deaths from terrorist attacks

# set up the user interace for visualization
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Deaths", min(interactiveViz$year), max(interactiveViz$year),
                            value = range(interactiveViz$year), step = 1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

# function to create interactive map
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    interactiveViz[interactiveViz$year >= 
                     input$range[1] & interactiveViz$year <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, interactiveViz$totalKilled)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(interactiveViz) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~((50*(totalKilled + 1)) + 5), weight = 1, color = "#777777",
                 fillColor = ~pal(totalKilled), fillOpacity = 0.7, popup = ~paste(totalKilled)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = interactiveViz)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~totalKilled
      )
    }
  })
}

shinyApp(ui, server)




