library(readxl)
library(ggplot2)
library(leaflet)
options(scipen = 100)

energyData <- read_excel("egrid2018_data_v2.xlsx")

# BEGINNING OF DATA CLEANING --------------------------------------

# replace all Na entries with 0
energyData[is.na(energyData)] = 0.00000000

# replace all negatives with 0
negativeSubset <- subset(energyData, select=9:length(energyData))
negativeSubset[negativeSubset < 0] = 0.00000000
energyData[9:length(energyData)] <- negativeSubset
rm(negativeSubset)

# add total values of OTHER_FOSIL and OTHER_UNKNOWN
energyData$OTHER_TOTAL = energyData$OTHER_FOSSIL_TOTAL + energyData$OTHER_UNKNOWN_TOTAL

# add percentage values of OTHER_FOSIL and OTHER_UNKNOWN
energyData$OTHER_PERCENTAGE = energyData$OTHER_FOSSIL_PERCENTAGE + energyData$OTHER_UNKNOWN_PERCENTAGE

# remove columns OTHER_FOSSIL_TOTAL, OTHER_UNKNOWN_TOTAL, OTHER_FOSSIL_PERCENTAGE, and OTHER_UNKNOWN_PERCENTAGE
energyData = subset(energyData, select = -c(OTHER_FOSSIL_TOTAL,OTHER_UNKNOWN_TOTAL,OTHER_FOSSIL_PERCENTAGE,OTHER_UNKNOWN_PERCENTAGE))

# make sure the percentage values are correct (these values got multiplied by 10000)
divideFactor <- 10000
energyData$COAL_PERCENTAGE <- energyData$COAL_PERCENTAGE/divideFactor
energyData$OIL_PERCENTAGE <- energyData$OIL_PERCENTAGE/divideFactor
energyData$GAS_PERCENTAGE <- energyData$GAS_PERCENTAGE/divideFactor
energyData$NUCLEAR_PERCENTAGE <- energyData$NUCLEAR_PERCENTAGE/divideFactor
energyData$HYDRO_PERCENTAGE <- energyData$HYDRO_PERCENTAGE/divideFactor
energyData$BIOMASS_PERCENTAGE <- energyData$BIOMASS_PERCENTAGE/divideFactor
energyData$WIND_PERCENTAGE <- energyData$WIND_PERCENTAGE/divideFactor
energyData$SOLAR_PERCENTAGE <- energyData$SOLAR_PERCENTAGE/divideFactor
energyData$GEOTHERMAL_PERCENTAGE <- energyData$GEOTHERMAL_PERCENTAGE/divideFactor
energyData$OTHER_PERCENTAGE <- energyData$OTHER_PERCENTAGE/divideFactor
rm(divideFactor)

# END OF DATA CLEANING --------------------------------------

illinoisData <- subset(energyData, PlantState == 'IL')
# anchorXList <- list(1:length(illinoisData))
# anchorXListIndex <- 1

#test area
# illinoisData <- subset(illinoisData, (illinoisData$PlantName == 'Waterloo' ))

sourceURLs <- list(COAL = "COAL.png", OIL = "OIL.png", GAS = "GAS.png", NUCLEAR = "NUCLEAR.png",
                   HYDRO = "HYDRO.png", BIOMASS = "BIOMASS.png", WIND = "WIND.png", 
                   SOLAR = "SOLAR.png", GEOTHERMAL = "GEOTHERMAL.png", OTHER = "OTHER.png")

universalTotalGraphHeight = 5;
universalAnchorY = 0;

getIconWidth <- function(sourceIndex){
  sourceWidth <- switch(sourceIndex, illinoisData$COAL_PERCENTAGE, illinoisData$OIL_PERCENTAGE, 
                        illinoisData$GAS_PERCENTAGE, illinoisData$NUCLEAR_PERCENTAGE, 
                        illinoisData$HYDRO_PERCENTAGE, illinoisData$BIOMASS_PERCENTAGE, 
                        illinoisData$WIND_PERCENTAGE, illinoisData$SOLAR_PERCENTAGE, 
                        illinoisData$GEOTHERMAL_PERCENTAGE, illinoisData$OTHER_PERCENTAGE) 
  sourceWidth <- sourceWidth/7
  sourceWidth <- ifelse(sourceWidth < 0.00000001, 0.00000001, sourceWidth)
  sourceWidth
}

# addWithAndSum <- function(thisSourceWidth, currAnchorSum){
#   # ifelse(thisSourceWidth <= 0.00001,
#   #        currAnchorSum,
#   #        (thisSourceWidth + currAnchorSum)
#   #        )
#   
#   thisSourceWidth + currAnchorSum
# }

getIconAnchorX <- function(index, anchorSum){
  inconWidth = getIconWidth(index)
  
  ifelse(index == 1, inconWidth + anchorSum, getIconAnchorX(index - 1, anchorSum = anchorSum + inconWidth))
}

sourceGraphIcon <- function(index){
  makeIcon(
    iconUrl = sourceURLs[index], 
    iconWidth = getIconWidth(index), iconHeight = universalTotalGraphHeight, 
    iconAnchorX = getIconAnchorX(index, 0), iconAnchorY = universalAnchorY
  )
}

illinoisMap <- leaflet(illinoisData) %>%
      addTiles() %>% # Add default OpenStreetMap map titles
      addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
                 icon = sourceGraphIcon(10)) %>%
      addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
                 icon = sourceGraphIcon(9)) %>%
      addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
                 icon = sourceGraphIcon(8)) %>%
      addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
                 icon = sourceGraphIcon(7)) %>%
      addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
                 icon = sourceGraphIcon(6)) %>%
      addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
                 icon = sourceGraphIcon(5)) %>%
      addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
                 icon = sourceGraphIcon(4)) %>%
      addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
                 icon = sourceGraphIcon(3)) %>%
      addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
                 icon = sourceGraphIcon(2)) %>%
      addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
                 icon = sourceGraphIcon(1))
      





















