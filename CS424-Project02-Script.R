library(shiny)
library(readxl)
library(ggplot2)
library(leaflet)
options(scipen = 100)


# Define UI for app
ui <- fluidPage(
  # App title 
  titlePanel("Project 2 - Raw Power"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing stateDataset ----
      selectInput(inputId = "stateDataset",
                  label = "Choose a state:",
                  choices = c(
                    "Alabama",
                    "Alaska",
                    "Arizona",
                    "Arkansas",
                    "California",
                    "Colorado",
                    "Connecticut",
                    "Delaware",
                    "Florida",
                    "Georgia",
                    "Hawaii",
                    "Idaho",
                    "Illinois",
                    "Indiana",
                    "Iowa",
                    "Kansas",
                    "Kentucky",
                    "Louisiana",
                    "Maine",
                    "Maryland",
                    "Massachusetts",
                    "Michigan",
                    "Minnesota",
                    "Mississippi",
                    "Missouri",
                    "Montana",
                    "Nebraska",
                    "Nevada",
                    "New Hampshire",
                    "New Jersey",
                    "New Mexico",
                    "New York",
                    "North Carolina",
                    "North Dakota",
                    "Ohio",
                    "Oklahoma",
                    "Oregon",
                    "Pennsylvania",
                    "Rhode Island",
                    "South Carolina",
                    "South Dakota",
                    "Tennessee",
                    "Texas",
                    "Utah",
                    "Vermont",
                    "Virginia",
                    "Washington",
                    "West Virginia",
                    "Wisconsin",
                    "Wyoming"
                  )
          ), 
      leafletOutput("mymap")
      # leafletProxy("mymap")  %>% fitBounds(0, 0, 11, 11)
    ),
  
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: leaflet Map
      # TODO: add the Illinois map
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Output: Verbatim text for data summary ----
      # verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      # tableOutput("view")
      
    )
  )
)

# Define server logic required to draw a leaflet Map
server <- function(input, output) {

  energyData <- read_excel("egrid2018_data_v2.xlsx")

  # BEGINNING OF DATA CLEANING --------------------------------------

  # replace all Na entries with 0
  energyData[is.na(energyData)] = 0.00000000
  # energyData < - subset(energyData, Latitude != 0)
  # energyData < - subset(energyData, Longitude != 0)

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
  
  dataset <- energyData
  
  sourceURLs <- list(COAL = "COAL.png", OIL = "OIL.png", GAS = "GAS.png", NUCLEAR = "NUCLEAR.png",
                     HYDRO = "HYDRO.png", BIOMASS = "BIOMASS.png", WIND = "WIND.png",
                     SOLAR = "SOLAR.png", GEOTHERMAL = "GEOTHERMAL.png", OTHER = "OTHER.png")

  universalTotalGraphHeight = 5;
  universalAnchorY = 3;

  getIconWidth <- function(sourceIndex){
    sourceWidth <- switch(sourceIndex, dataset$COAL_PERCENTAGE, dataset$OIL_PERCENTAGE,
                          dataset$GAS_PERCENTAGE, dataset$NUCLEAR_PERCENTAGE,
                          dataset$HYDRO_PERCENTAGE, dataset$BIOMASS_PERCENTAGE,
                          dataset$WIND_PERCENTAGE, dataset$SOLAR_PERCENTAGE,
                          dataset$GEOTHERMAL_PERCENTAGE, dataset$OTHER_PERCENTAGE)
    sourceWidth <- sourceWidth/7
    sourceWidth <- ifelse(sourceWidth < 0.00000001, 0.00000001, sourceWidth)
    sourceWidth
  }

  getIconAnchorX <- function(index, anchorSum){
    inconWidth = getIconWidth(index)

    ifelse(index == 1, inconWidth + anchorSum, getIconAnchorX(index - 1, anchorSum = anchorSum + inconWidth))
  }

  sourceGraphIcon <- function(index){
    makeIcon(
      iconUrl = sourceURLs[index],
      iconWidth = getIconWidth(index), iconHeight = universalTotalGraphHeight,
      iconAnchorX = getIconAnchorX(index, 0),
      iconAnchorY = universalAnchorY
    )
  }
  
  # ----------------------------------------------------------------------
  
  getStateDataset <- function(selectedState) {
    subset(energyData, PlantState == selectedState)
  }
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$stateDataset, 
           "Alabama" = getStateDataset('AL'),
           "Alaska" = getStateDataset('AK'),
           "Arizona" = getStateDataset('AZ'),
           "Arkansas" = getStateDataset('AR'),
           "California" = getStateDataset('CA'),
           "Colorado" = getStateDataset('CO'),
           "Connecticut" = getStateDataset('CT'),
           "Delaware" = getStateDataset('DE'),
           "Florida" = getStateDataset('FL'),
           "Georgia" = getStateDataset('GA'),
           "Hawaii" = getStateDataset('HI'),
           "Idaho" = getStateDataset('ID'),
           "Illinois" = getStateDataset('IL'),
           "Indiana" = getStateDataset('IN'),
           "Iowa" = getStateDataset('IA'),
           "Kansas" = getStateDataset('KS'),
           "Kentucky" = getStateDataset('KY'),
           "Louisiana" = getStateDataset('LA'),
           "Maine" = getStateDataset('ME'),
           "Maryland" = getStateDataset('MD'),
           "Massachusetts" = getStateDataset('MA'),
           "Michigan" = getStateDataset('MI'),
           "Minnesota" = getStateDataset('MN'),
           "Mississippi" = getStateDataset('MS'),
           "Missouri" = getStateDataset('MO'),
           "Montana" = getStateDataset('MT'),
           "Nebraska" = getStateDataset('NE'),
           "Nevada" = getStateDataset('NV'),
           "New Hampshire" = getStateDataset('NH'),
           "New Jersey" = getStateDataset('NJ'),
           "New Mexico" = getStateDataset('NM'),
           "New York" = getStateDataset('NY'),
           "North Carolina" = getStateDataset('NC'),
           "North Dakota" = getStateDataset('ND'),
           "Ohio" = getStateDataset('OH'),
           "Oklahoma" = getStateDataset('OK'),
           "Oregon" = getStateDataset('OR'),
           "Pennsylvania" = getStateDataset('PA'),
           "Rhode Island" = getStateDataset('RI'),
           "South Carolina" = getStateDataset('SC'),
           "South Dakota" = getStateDataset('SD'),
           "Tennessee" = getStateDataset('TN'),
           "Texas" = getStateDataset('TX'),
           "Utah" = getStateDataset('UT'),
           "Vermont" = getStateDataset('VT'),
           "Virginia" = getStateDataset('VA'),
           "Washington" = getStateDataset('WA'),
           "West Virginia" = getStateDataset('WV'),
           "Wisconsin" = getStateDataset('WI'),
           "Wyoming" = getStateDataset('WY')
    )
  })
  
  # Create caption ----
  output$caption <- renderText({
    input$caption
  })
  # # Generate a summary of the dataset ----
  # output$summary <- renderPrint({
  #   dataset <- datasetInput()
  #   summary(dataset)
  # })
  
  output$mymap <- renderLeaflet({
    dataset <- datasetInput()
    leaflet(dataset) %>%
          addTiles() %>% # Add default OpenStreetMap map titles
          addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                     icon = sourceGraphIcon(10)) %>%
          addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                     icon = sourceGraphIcon(9)) %>%
          addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                     icon = sourceGraphIcon(8)) %>%
          addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                     icon = sourceGraphIcon(7)) %>%
          addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                     icon = sourceGraphIcon(6)) %>%
          addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                     icon = sourceGraphIcon(5)) %>%
          addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                     icon = sourceGraphIcon(4)) %>%
          addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                     icon = sourceGraphIcon(3)) %>%
          addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                     icon = sourceGraphIcon(2)) %>%
          addMarkers(lng = dataset$Longitude, lat=dataset$Latitude, popup=dataset$PlantName,
                     icon = sourceGraphIcon(1)) 
  })
  
  
  
  # Show the first "n" observations ----
  # output$view <- renderTable({
  #   head(datasetInput(), n = input$obs)
  # })
}

# Create Shiny app
shinyApp(ui = ui, server = server)






# #test area
# # illinoisData <- subset(illinoisData, (illinoisData$PlantName == 'Waterloo' ))
# 





# 
# illinoisMap <- leaflet(illinoisData) %>%
#       addTiles() %>% # Add default OpenStreetMap map titles
#       addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
#                  icon = sourceGraphIcon(10)) %>%
#       addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
#                  icon = sourceGraphIcon(9)) %>%
#       addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
#                  icon = sourceGraphIcon(8)) %>%
#       addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
#                  icon = sourceGraphIcon(7)) %>%
#       addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
#                  icon = sourceGraphIcon(6)) %>%
#       addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
#                  icon = sourceGraphIcon(5)) %>%
#       addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
#                  icon = sourceGraphIcon(4)) %>%
#       addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
#                  icon = sourceGraphIcon(3)) %>%
#       addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
#                  icon = sourceGraphIcon(2)) %>%
#       addMarkers(lng = illinoisData$Longitude, lat=illinoisData$Latitude, popup=illinoisData$PlantName,
#                  icon = sourceGraphIcon(1))
#       
# 
# 
# 
# 

















