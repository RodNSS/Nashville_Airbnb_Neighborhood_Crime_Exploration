library(shiny)
library(tidyverse)
library(shinyjs)
library(dplyr)
library(jsonlite)
library(glue)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(DT)
library(lubridate)
library(rgeos)
library(bslib)
library(thematic)
library(shinythemes)
library(sass)
library(plotly)
library(ggplot2)
library(htmltools)
library(htmlwidgets)
library(shinyWidgets)
library(r2d3)

source("Global.R")
# Function to fetch data from the provided URL
fetchData <- function() {
  url <- "https://data.nashville.gov/resource/2u6v-ujjs.json?$$app_token=70kFMIvD8FfF3OzDbDjCDkNJ0&$limit=15000"
  json_data <- jsonlite::fromJSON(url)
  data <- as.data.frame(json_data)
  
  # Extract date and time components
  data$incident_occurred <- as.POSIXct(data$incident_occurred, format = "%Y-%m-%dT%H:%M:%S")
  data$Date <- as.Date(data$incident_occurred)
  data$Time <- format(data$incident_occurred, format = "%I:%M %p")
  
  # Remove milliseconds from the Time column
  data$Time <- sub("\\.\\d+", "", data$Time)
  
  # Create new column "Time of Incident"
  data$`Time of Incident` <- data$Time
  
  # Remove the original "incident_occurred", "Date", and "Time" columns if desired
  data$incident_occurred <- NULL
  data$Time <- NULL
  
  # Filter out entries with "incident_status_description" as "UNFOUNDED" and "TEST ONLY"
  data <- data[data$incident_status_description != "UNFOUNDED", ]
  data <- data[data$offense_description != "TEST ONLY", ]
  
  # Remove rows with missing latitude or longitude values
  data <- data[complete.cases(data[, c("latitude", "longitude")]), ]
  
  selected_cols <- c(
    "incident_number", "incident_status_description", "investigation_status",
    "incident_location", "latitude", "longitude",
    "location_description", "offense_description", "weapon_description",
    "victim_number", "domestic_related", "victim_description",
    "victim_gender", "victim_race", "victim_ethnicity", "victim_county_resident",
    "Date", "Time of Incident"
  )
  
  data <- data[selected_cols]
  
  # Calculate the date 3 months ago from the current date
  three_months_ago <- Sys.Date() - months(3)
  
  # Filter data to exclude anything from 3 months before the current date
  data <- data[data$Date >= three_months_ago, ]
  
  # Found duplicate offenses in slightly different format
  data$offense_description <- gsub("BURGLARY- MOTOR VEHICLE", "BURGLARY - MOTOR VEHICLE", data$offense_description)
  data$offense_description <- gsub("BURGLARY- AGGRAVATED", "BURGLARY - AGGRAVATED", data$offense_description)
  
  # Separate crime categories
  data <- data %>%
    mutate(crime_category = case_when(
      offense_description %in% c("AGGRAV ASSLT - FAMILY-GUN", "AGGRAV ASSLT - FAMILY-STGARM", "AGGRAV ASSLT - FAMILY-WEAPON", "AGGRAV ASSLT - NONFAMILY-GUN", 
                                 "AGGRAV ASSLT - NONFAMILY-WEAPON", "AGGRAV ASSLT - POL OFF-GUN", "AGGRAV ASSLT - POL OFF-WEAPON", "AGGRAV ASSLT - PUB OFF-GUN", 
                                 "ARSON - BUSINESS", "ARSON - PUB-BLDG", "ARSON - RESID", "ASSAULT", "ASSAULT OF OFFICER - BODILY INJURY", "ASSAULT- FEAR OF BODILY INJURY", 
                                 "ASSAULT- OFFENSIVE OR PROVOCATIVE CONTACT", "ASSAULT, (NO INJURY, ON OFFICER/PUBLIC OFFICIAL)", "ASSAULT, AGG - DEADLY WEAPON - RECKLESS-IN CONCERT", 
                                 "Assault, Agg - Deadly Weapon- Int/Kn- Acting in Concert", "ASSAULT, AGG - SERIOUS BODILY INJURY - RECKLESS-IN CONCERT", 
                                 "Assault, Agg - Serious Bodily Injury- Int/Kn- Acting in Concert", "Assault, Agg - Strangulation- Int/Kn- Acting in Concert", "ASSAULT, AGG DEADLY WEAPON- INT/KN", 
                                 "ASSAULT, AGG., FIRST RESPONDER, DEADLY WEAPON", "ASSAULT, AGG., NURSE, STRANGULATION", "Assault, Aggravated - Deadly Weapon - Int/Kn", 
                                 "Assault, Aggravated - Deadly Weapon - Int/Kn - In Concert", "ASSAULT, AGGRAVATED - DEADLY WEAPON - INT/KN FROM MOT VEH", "Assault, 
                                 Aggravated - Deadly Weapon - Reckless", "ASSAULT, AGGRAVATED - DEADLY WEAPON - RECKLESS FROM MOT VEH", "Assault, Aggravated - Death - Int/Kn", 
                                 "Assault, Aggravated - Serious Bodily Injury - Reckless", "Assault, Aggravated - Strangulation - Int/Kn", "ASSAULT, AGGRAVATED - STRANGULATION-INT/KN", 
                                 "ASSAULT, DOMESTIC, BODILY INJURY 2ND OFFENSE", "ASSAULT, DOMESTIC, BODILY INJURY 3RD OR MORE", "ASSAULT, FIRST RESPONDER, BODILY INJURY", 
                                 "ASSAULT, FIRST RESPONDER, OFFENSIVE CONTACT", "Assault, health care provider - Bodily Injury", "Assault, health care provider - Fear of Bodily Injury", 
                                 "Assault, health care provider - Offensive Contact", "ASSAULT, NURSE, BODILY INJURY", "ASSAULT, NURSE, OFFENSIVE CONTACT", 
                                 "Assault, Officer/Responder - Agg - Serious Injury - Reckless", "Assault, Officer/Responder -Agg -Deadly Weapon - Int/Kn", 
                                 "ASSAULT, OFFICER/RESPONDER-AGG-DEAD WPN-RECK FROM MOT VEH", "ASSAULT, VEHICULAR - 1ST OFFENSE", "ASSAULT, VEHICULAR, AGGRAVATED", 
                                 "HARASSMENT- CAUSE EMOTIONAL DISTRESS, INTIMIDATE, FRIGHTEN", "HARRASSMENT (NUISANCE)", "HOMICIDE", "HOMICIDE- CRIMINAL", 
                                 "HOMICIDE, JUSTIFIABLE", "INTENTIONAL AGGRAVATED ASSAULT", "KIDNAPPING, CRIMINAL ATTEMPT", "RECKLESS ENDANGERMENT-SHOOTING FROM W/IN A VEHICLE", 
                                 "ROBBERY", "Robbery - Acting in Concert", "ROBBERY- AGG.- SERIOUS BODILY INJURY", "ROBBERY- ESP. AGG.", "ROBBERY, AGGRAVATED, HANDGUN", 
                                 "ROBBERY, AGGRAVATED, KNIFE", "ROBBERY, AGGRAVATED, LONG GUN", "ROBBERY, AGGRAVATED, OTHER", "SIMPLE ASSLT", "SIMPLE ASSLT - STRANGULATION (NO LOSS OF CONSCIOUSNESS)", 
                                 "STALKING - AGGRAVATED", "STALKING- (VALID ONLY:  7/1/92 - 6/30/95)", "STALKING- NO PRIOR CONVICTION", "THREAT OF MASS VIOLENCE IN SCHOOL", 
                                 "THREAT TO BOMB", "THREAT TO BURN", "VEHICLE OFFENSE, CRIMINAL ATTEMPT", "Weapon - Dangerous Felony - w/prior conviction", "Weapon - Felon-Poss-Firearm (Drug Offense)", 
                                 "Weapon - Felon-Poss-Firearm (Force,Violence,Deadly Weapon)", "WEAPON - FELON-POSS-FIREARM(DRUG OFFENSE)", "WEAPON - FELON-POSS-FIREARM(VIOLENCE, DEADLY WEAPON)", 
                                 "WEAPON - FIREARM OR CLUB", "WEAPON - FIREARM OR CLUB W/PRIORS", "WEAPON OFFENSE, CRIMINAL ATTEMPT", "WEAPON- CARRYING ON SCHOOL PROPERTY") ~ "Violent Crime",
      offense_description %in% c("BURGL - FORCED ENTRY-NONRESID", "BURGL - NO FORCED ENTRY-NONRESID", "BURGL - SAFE-VAULT", "BURGLARY", "BURGLARY - AGGRAVATED", 
                                 "BURGLARY - AGGRAVATED - ACTING IN CONCERT", "BURGLARY - ESPECIALLY AGGRAVATED", "BURGLARY - MOTOR VEHICLE", "BURGLARY (NON HABITATION)", 
                                 "BURGLARY, CRIMINAL ATTEMPT", "CRIMINAL TRESPASS", "CRIMINAL TRESPASS- AGGRAVATED", "CRIMINAL TRESPASS- AGGRAVATED - HOME/SCHOOL", 
                                 "CRITICAL INFRASTRUCTURE VANDALISM - $10,000 OR > BUT < $60,000", "DAMAGE PROP - BUSINESS", "DAMAGE PROP - PRIVATE", "DAMAGE PROP - PUBLIC", 
                                 "DAMAGE TO PROPERTY, CRIMINAL ATTEMPT", "LARC - BICYCLE", "LARC - FROM BANKING TYPE-INST", "LARC - FROM BLDG", "LARC - FROM COIN MACHINE", 
                                 "LARC - PARTS FROM VEH", "LARC - POSTAL", "LARC - RESID", "LARCENY - (FREE TEXT)", "LOST PROPERTY", "FOUND PROPERTY", "POSSESS STOLEN PROP", 
                                 "Reckless Endangerment-Unoccupied Habitation", "RECOVERY, STOLEN PROPERTY", "STOLEN PROPERTY", "THEFT OF PROPERTY- > $500 BUT < $1,000", 
                                 "THEFT OF PROPERTY- $1,000 OR >  BUT < $10,000", "THEFT OF PROPERTY- $10,000 OR >  BUT  < $60,000", "Theft of Property- $250,000 or more", 
                                 "THEFT OF PROPERTY- $500 OR LESS", "Theft of Property- $60,000 or > but < $250,000", "THEFT OF PROPERTY->$1,000 BUT <$2,500", 
                                 "THEFT OF PROPERTY-$1,000 OR LESS", "Theft of Vehicle- $60,000 or > but < $250,000", "THEFT OF VEHICLE->$1,000 BUT <$2,500", 
                                 "THEFT OF VEHICLE-$1,000 OR LESS", "THEFT OF VEHICLE-2,500 OR > BUT<$10,000", "VEHICLE THEFT") ~ "Property Crime",
      TRUE ~ "Other"
    ))
  
  return(data)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML('
      /* Add glow effect to the border lines */
      .glow-border {
        box-shadow: 0 0 15px #00FFFB;
      }

      /* Add glow effect to the text in the title panel */
      .glow-text {
        text-shadow: 0 0 10px #FFFFFF, 0 0 20px #FFFFFF;
      }

      /* Adjust the header title style */
      h1.title {
        text-transform: uppercase;
        letter-spacing: 1px;
        font-size: 24px;
        font-weight: bold;
      }
      
      /* Adjust the table row style */
      table.dataTable tbody tr.active td {
        color: black !important;
        box-shadow: inset 0 0 0 9999px "#3E45C490" !important;
      }
    '))
  ),
  theme = bslib::bs_add_rules(
    bslib::bs_theme(version = 5, bg = '#142730', fg = '#00FFFB', primary = "#00FFFB",
                    base_font = font_google("Nunito Sans") 
    ),
    sass::sass(
      "h1.title { text-transform: uppercase; letter-spacing: 1px; font-size: 24px; font-weight: bold; }"
    )
  ),
  titlePanel(
    tags$h1(class = "title glow-text", "Nashville Airbnb Crime Map")
  ),
  sidebarLayout(
    sidebarPanel(
      div(class = "glow-border",
          dateRangeInput("dateRange", label = "Date Range", format = "yyyy-mm-dd"),
          pickerInput(
            inputId = "crime_type",
            label = "Filter By Crime Type",
            choices = NULL,
            options = list(`actions-box` = TRUE, size = 10),
            multiple = TRUE
          ),
          plotlyOutput("sunburst"),
          d3Output("chart"),
      )
    ),
    mainPanel(
      div(class = "glow-border",
          leafletOutput("map", height = 500),
          actionButton("resetButton", "Reset"),
          DT::dataTableOutput("table")
      )
    )
  )
)

server <- function(input, output, session) {
  #bs_themer()
  
  # store click as reactiveValues
  click <- reactiveValues(clickedMarker=NULL)
  
  crimes <- reactive({
    data <- fetchData()
    data$uid <- seq.int(nrow(data))
    data
  })
  
  # Define the reactive expression to fetch data and update pickerInput choices
  unique_offense_description <- reactiveVal()
  
  # Define the reactive expression to fetch data and update pickerInput choices
  updatePickerChoices <- eventReactive(input$dateRange, {
    data <- crimes()  # Use crimes() instead of fetchData()
    filtered_data <- data[data$Date >= input$dateRange[1] & data$Date <= input$dateRange[2], ]
    unique_offense_description(sort(unique(filtered_data$offense_description)))
    updatePickerInput(
      session = session,
      inputId = "crime_type",
      choices = unique_offense_description(),
      selected = unique_offense_description(),
      options = list(`actions-box` = TRUE, size = 10)
    )
    filtered_data
  })
  
  # Define a reactive expression for filtered data
  filtered_data <- reactive({
    data <- crimes()  
    data[data$Date >= input$dateRange[1] & data$Date <= input$dateRange[2] & data$offense_description %in% input$crime_type, ]
  })
  
  # Use filtered_data in crimes_sf reactive expression
  crimes_sf <- reactive({
    data <- filtered_data() 
    filtered_data <- data[data$offense_description %in% input$crime_type, ]
    st_as_sf(filtered_data, coords = c("longitude", "latitude"), crs = 4326)
  })
  
  # Use updatePickerChoices to trigger the update of pickerInput choices
  observe({
    updatePickerChoices()
  })
  
  # Locates all crimes within a quarter mile distance of Airbnb properties and make reactive
  quarter_mile <- reactive({
    st_is_within_distance(ab_sf, crimes_sf(), dist = 402.336)
  })
  
  # Update the date range input based on the data
  observe({
    data <- crimes()
    if (!is.null(data)) {
      minDate <- min(data$Date)
      maxDate <- max(data$Date)
      updateDateRangeInput(session, "dateRange", start = minDate, end = maxDate,
                           min = minDate, max = maxDate)
    }
  })
  
  # Reset page
  observeEvent(input$resetButton, {
    updatePickerChoices()
    proxy <- DT::dataTableProxy('table')
    DT::replaceData(proxy, table_data())
    click$clickedMarker <- NULL
    
    # Reset the pickerInput selection
    updatePickerInput(
      session = session,
      inputId = "crime_type",
      selected = unique_offense_description()
    )
  })
  
  # Define quarter_mile_indices as a reactive value
  quarter_mile_indices <- reactive({
    quarter_mile()
  })
  
  # Filters the data and returns based on click id
  table_data <- reactive({
    data <- filtered_data()  
    
    if (is.null(click$clickedMarker)) {
      data
    } else {
      marker_uid <- crimes_sf()[quarter_mile_indices()[[click$clickedMarker$id]], ] %>%
        pull(uid)
      data %>%
        filter(uid %in% marker_uid) 
    }
  })
  
  rv <- reactiveValues(crime_number = NULL)
  airbnb_data <- reactiveVal(airbnb)
  
  observe({
    # Calculate the crime numbers for the filtered data
    crime_number <- sapply(quarter_mile_indices(), length)
    
    # Store the crime numbers in rv$crime_number
    rv$crime_number <- crime_number
    
    # Merge rv$crime_number with airbnb_data
    if (!is.null(rv$crime_number) && !is.null(airbnb_data())) {
      airbnb_merged <- airbnb_data() %>%
        mutate(crime_number = rv$crime_number)
      airbnb_data(airbnb_merged)
    }
  })
  
  # Define a reactive value to store the Airbnb property name
  selected_airbnb <- reactiveVal(NULL)
  
  # observer for airbnb property click
  observeEvent(input$map_shape_click, {
    click$clickedMarker <- input$map_shape_click
    
    # Store the selected Airbnb property name
    if (!is.null(input$map_shape_click$id)) {
      selected_airbnb(airbnb_data()$name[input$map_shape_click$id])
    } else {
      selected_airbnb(NULL)
    }
  })
  
  # Plot for crimes within a quarter mile of airbnb
  output$sunburst <- renderPlotly({
    if(is.null(click$clickedMarker)
    ) 
      table_data() %>%
      count(paste(nrow(table_data()),"Crimes"), 
            crime_category, 
            offense_description) %>%
      counts_to_sunburst() %>%
      layout(colorway = c("#3E45C490", "#FFA50099", "#80000090"), 
             paper_bgcolor='#142730',
             title = "All Crimes In Nashville", 
             font = list(color = "#00FFFB"),
             margin = mrg,
             showlegend = F,
             xaxis = list(showgrid = F, 
                          zeroline = F, 
                          showticklabels = F),
             yaxis = list(showgrid = F, 
                          zeroline = F, 
                          showticklabels = F))
    else
      table_data() %>%
      count(paste(nrow(table_data()),"Crimes"), 
            crime_category, 
            offense_description) %>%
      counts_to_sunburst() %>%
      layout(colorway = c("#3E45C490", "#FFA50099", "#80000090"),
             paper_bgcolor='#142730',
             title = paste(nrow(table_data()), 
                           "crimes within a quarter mile"), 
             margin = mrg, 
             showlegend = F, 
             font = list(color = "#00FFFB"),
             xaxis = list(showgrid = F, 
                          zeroline = F, 
                          showticklabels = F),
             yaxis = list(showgrid = F, 
                          zeroline = F, 
                          showticklabels = F))
    
  })
  
  
  options(r2d3.theme = list(background = "#142730", foreground = '#00FFFB'))
  
  # Barplot for locations of crime
  output$chart <- renderD3({
    if (is.null(click$clickedMarker)) {
      table_data() %>%
        group_by(location_description) %>%
        summarize(count = n()) %>%
        top_n(10, count) %>%
        arrange(desc(count)) %>%
        as.data.frame() %>%
        r2d3(
          script = "Custom_D3.js",
          data = .,  
          options = list(
            margin = 50,
            barPadding = 0.1,
            color = "#00FFFB50",
            xLabel = "Location Description",
            yLabel = "Total Incidents",
            chartTitle = "Incident Location"
          )
        )
    } else {
      table_data() %>%
        group_by(location_description) %>%
        summarize(count = n()) %>%
        top_n(10, count) %>%
        arrange(desc(count)) %>%
        as.data.frame() %>%
        r2d3(
          script = "Custom_D3.js",
          data = .,  
          options = list(
            margin = 50,
            barPadding = 0.1,
            color = "#00FFFB50",
            xLabel = "Location Description",
            yLabel = "Total Incidents",
            chartTitle = "Incident Location"
          )
        )
    }
  })
  
  color_palette <- reactive({
    if (!is.null(airbnb_data()$crime_number)) {
      max_value <- max(airbnb_data()$crime_number, na.rm = TRUE)
      
      if (max_value <= 400) {
        # Define the crime bins
        crime_bins <- c(0, 1, 5, 15, 25, 50, 75, 100, 150, 200, 300, 400)
        
        # Create the color palette with the specified bins
        colorBin(
          palette = c("#3A0889FF", "#2D3184FF", "#1E4F8EFF", "#09679AFF", 
                      "#007DA4FF", "#0A92ACFF", "#28A4B3FF", "#43B5B8FF",
                      "#5EC3BBFF", "#77CFBEFF", "#8FDAC0FF", "#A5E2C3FF"),
          bins = crime_bins
        )
      } else {
        # Define the crime bins with an additional bin for values greater than 400
        crime_bins <- c(0, 1, 5, 15, 25, 50, 75, 100, 150, 200, 300, 400, max_value)
        
        # Create the color palette with the specified bins and an extra color
        colorBin(
          palette = c("#3A0889FF", "#2D3184FF", "#1E4F8EFF", "#09679AFF", 
                      "#007DA4FF", "#0A92ACFF", "#28A4B3FF", "#43B5B8FF",
                      "#5EC3BBFF", "#77CFBEFF", "#8FDAC0FF", "#A5E2C3FF","#CBEDCAFF"),
          bins = crime_bins
        )
      }
    }
  })
  
  rv_breaks <- reactive({
    if (!is.null(airbnb_data()$crime_number)) {
      num_intervals <- 11
      colorQuantile(airbnb_data()$crime_number, n = num_intervals + 1)
    }
  })
  
  rv_labels <- reactive({
    if (!is.null(airbnb_data()$crime_number)) {
      cut(isolate(airbnb_data()$crime_number), breaks = rv_breaks(), labels = FALSE, include.lowest = TRUE)
    }
  })
  
  label_ranges <- reactive({
    if (!is.null(rv_breaks())) {
      paste0(rv_breaks()[-length(rv_breaks())], "-", rv_breaks()[-1])
    }
  })
  
  # Update leaflet map
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles(mapbox, attribution) %>%
      clearMarkers() %>%
      addCircles(data = airbnb_data(), 
                 radius = 3, 
                 weight = 2, 
                 opacity = 1, 
                 fillOpacity = 1,
                 label = paste(airbnb_data()$name, 
                               "<br>Total Crimes Within A Quarter Mile:", 
                               airbnb_data()$crime_number) %>% 
                   lapply(htmltools::HTML), 
                 group = "Airbnb", 
                 layerId = ~uid,
                 color = ~color_palette()(crime_number),
                 popup = paste("<h3>Airbnb</h3>", 
                               airbnb$popup3, "<br>", 
                               "Name:",
                               airbnb$popup2,"<br>", 
                               airbnb$popup)) %>%
      addSearchFeatures(
        targetGroups ="Airbnb", 
        options = searchFeaturesOptions(zoom =12,
                                        openPopup = TRUE, 
                                        firstTipSubmit = TRUE,
                                        autoCollapse = TRUE, 
                                        hideMarkerOnCollapse = TRUE,
                                        textPlaceholder = "Search For An Airbnb Property")) %>%
      addCircleMarkers(
        data = crimes_sf(),
        radius = 3,
        weight = 3,
        color = "red",
        layerId = as.character(crimes_sf()$id),
        label = paste(crimes_sf()$offense_description, 
                      "<br>Date:", 
                      crimes_sf()$`Date`, 
                      "<br>Time:", 
                      crimes_sf()$`Time of Incident`) %>% 
          lapply(htmltools::HTML), group = "Crime Layer",
        clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 1.5)
      ) %>% setView(lat = 36.1627, lng = -86.7816, zoom = 11) %>% 
      addResetMapButton() %>%
      addLegend(
        "bottomright",
        pal = color_palette(),
        values = airbnb_data()$crime_number,
        labels = label_ranges(),
        opacity = 1,
        title = "Crimes",
        layerId = "legend"
      ) %>%
      addLayersControl(overlayGroups =c("Airbnb",
                                        "Crime Layer"),
                       options = layersControlOptions(collapsed = TRUE))
  })
  
  # Render datatable
  output$table <- DT::renderDataTable({
    filtered_data <- table_data()[, c(
      "incident_number", "incident_status_description",
      "Date", "Time of Incident",
      "incident_location", "location_description",
      "offense_description", "weapon_description",
      "victim_number", "domestic_related",
      "victim_description", "victim_gender", "victim_race",
      "victim_ethnicity", "victim_county_resident"
    )]
    
    # Alias the column names
    colnames(filtered_data) <- c(
      "Incident Number", "Incident Status",
      "Date", "Time of Incident",
      "Incident Location", "Location Description",
      "Offense Description", "Weapon Description",
      "Victim Number", "Domestic Related",
      "Victim Description", "Victim Gender", "Victim Race",
      "Victim Ethnicity", "Victim County Resident"
    )
    
    DT::datatable(filtered_data, selection = "single",
                  options = list(
                    scrollX = TRUE,
                    scrollY = "400px"
                  )
    )
  })
  
  
}

shinyApp(ui, server)
