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

# execute global variables file first
source("global.r")

# function to fetch data from the API
fetchData <- function() {
  url <- "https://data.nashville.gov/resource/2u6v-ujjs.json?$$app_token={APP_TOKEN_HERE}$limit=15000"
  json_data <- jsonlite::fromJSON(url)
  data <- as.data.frame(json_data)
  
  # extract date and time components
  data$incident_occurred <- as.POSIXct(data$incident_occurred, format = "%Y-%m-%dT%H:%M:%S")
  data$date <- as.Date(data$incident_occurred)
  data$time <- format(data$incident_occurred, format = "%I:%M %p")
  
  # remove milliseconds from the time column
  data$time <- sub("\\.\\d+", "", data$time)
  
  # create new column "time of incident"
  data$`time of incident` <- data$time
  
  # remove the original "incident_occurred", "date", and "time" columns
  data$incident_occurred <- NULL
  data$time <- NULL
  
  # filter out entries with "incident_status_description" as "unfounded" and "test only"
  data <- data[data$incident_status_description != "unfounded", ]
  data <- data[data$offense_description != "test only", ]
  
  # remove rows with missing latitude or longitude values
  data <- data[complete.cases(data[, c("latitude", "longitude")]), ]
  
  selected_cols <- c(
    "incident_number", "incident_status_description", "investigation_status",
    "incident_location", "latitude", "longitude",
    "location_description", "offense_description", "weapon_description",
    "victim_number", "domestic_related", "victim_description",
    "victim_gender", "victim_race", "victim_ethnicity", "victim_county_resident",
    "date", "time of incident"
  )
  
  # use only selected columns
  data <- data[selected_cols]
  
  # calculate the date 3 months ago from the current date
  three_months_ago <- Sys.Date() - months(3)
  
  # filter data to exclude anything from 3 months before the current date
  data <- data[data$date >= three_months_ago, ]
  
  # found duplicate offenses in slightly different format
  data$offense_description <- gsub("burglary- motor vehicle", "burglary - motor vehicle", data$offense_description)
  data$offense_description <- gsub("burglary- aggravated", "burglary - aggravated", data$offense_description)
  
  # separate crimes into categories
  data <- data %>%
    mutate(crime_category = case_when(
      offense_description %in% c(
        "AGGRAV ASSLT - FAMILY-GUN", "AGGRAV ASSLT - FAMILY-STGARM", "AGGRAV ASSLT - FAMILY-WEAPON", "AGGRAV ASSLT - NONFAMILY-GUN", 
        "AGGRAV ASSLT - NONFAMILY-WEAPON", "AGGRAV ASSLT - POL OFF-GUN", "AGGRAV ASSLT - POL OFF-WEAPON", "AGGRAV ASSLT - PUB OFF-GUN", 
        "ARSON - BUSINESS", "ARSON - PUB-BLDG", "ARSON - RESID", "ASSAULT", "ASSAULT OF OFFICER - BODILY INJURY", "ASSAULT- FEAR OF BODILY INJURY", 
        "ASSAULT- OFFENSIVE OR PROVOCATIVE CONTACT", "ASSAULT, (NO INJURY, ON OFFICER/PUBLIC OFFICIAL)", "ASSAULT, AGG - DEADLY WEAPON - RECKLESS-IN CONCERT", 
        "Assault, Agg - Deadly Weapon- Int/Kn- Acting in Concert", "ASSAULT, AGG - SERIOUS BODILY INJURY - RECKLESS-IN CONCERT", 
        "Assault, Agg - Serious Bodily Injury- Int/Kn- Acting in Concert", "Assault, Agg - Strangulation- Int/Kn- Acting in Concert", "ASSAULT, AGG DEADLY WEAPON- INT/KN", 
        "ASSAULT, AGG., FIRST RESPONDER, DEADLY WEAPON", "ASSAULT, AGG., NURSE, STRANGULATION", "Assault, Aggravated - Deadly Weapon - Int/Kn", 
        "Assault, Aggravated - Deadly Weapon - Int/Kn - In Concert", "ASSAULT, AGGRAVATED - DEADLY WEAPON - INT/KN FROM MOT VEH", 
        "Assault, Aggravated - Deadly Weapon - Reckless", "ASSAULT, AGGRAVATED - DEADLY WEAPON - RECKLESS FROM MOT VEH", "Assault, Aggravated - Death - Int/Kn", 
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
        "WEAPON - FIREARM OR CLUB", "WEAPON - FIREARM OR CLUB W/PRIORS", "WEAPON OFFENSE, CRIMINAL ATTEMPT", "WEAPON- CARRYING ON SCHOOL PROPERTY"
      ) ~ "Violent Crime",
      offense_description %in% c(
        "BURGL - FORCED ENTRY-NONRESID", "BURGL - NO FORCED ENTRY-NONRESID", "BURGL - SAFE-VAULT", "BURGLARY", "BURGLARY - AGGRAVATED", 
        "BURGLARY - AGGRAVATED - ACTING IN CONCERT", "BURGLARY - ESPECIALLY AGGRAVATED", "BURGLARY - MOTOR VEHICLE", "BURGLARY (NON HABITATION)", 
        "BURGLARY, CRIMINAL ATTEMPT", "CRIMINAL TRESPASS", "CRIMINAL TRESPASS- AGGRAVATED", "CRIMINAL TRESPASS- AGGRAVATED - HOME/SCHOOL", 
        "CRITICAL INFRASTRUCTURE VANDALISM - $10,000 OR > BUT < $60,000", "DAMAGE PROP - BUSINESS", "DAMAGE PROP - PRIVATE", "DAMAGE PROP - PUBLIC", 
        "DAMAGE TO PROPERTY, CRIMINAL ATTEMPT", "LARC - BICYCLE", "LARC - FROM BANKING TYPE-INST", "LARC - FROM BLDG", "LARC - FROM COIN MACHINE", 
        "LARC - PARTS FROM VEH", "LARC - POSTAL", "LARC - RESID", "LARCENY - (FREE TEXT)", "LOST PROPERTY", "FOUND PROPERTY", "POSSESS STOLEN PROP", 
        "Reckless Endangerment-Unoccupied Habitation", "RECOVERY, STOLEN PROPERTY", "STOLEN PROPERTY", "THEFT OF PROPERTY- > $500 BUT < $1,000", 
        "THEFT OF PROPERTY- $1,000 OR >  BUT < $10,000", "THEFT OF PROPERTY- $10,000 OR >  BUT  < $60,000", "Theft of Property- $250,000 or more", 
        "THEFT OF PROPERTY- $500 OR LESS", "Theft of Property- $60,000 or > but < $250,000", "THEFT OF PROPERTY->$1,000 BUT <$2,500", 
        "THEFT OF PROPERTY-$1,000 OR LESS", "Theft of Vehicle- $60,000 or > but < $250,000", "THEFT OF VEHICLE->$1,000 BUT <$2,500", 
        "THEFT OF VEHICLE-$1,000 OR LESS", "THEFT OF VEHICLE-2,500 OR > BUT<$10,000", "VEHICLE THEFT"
      ) ~ "Property Crime",
      TRUE ~ "Other"
    ))
  
  return(data)
}

# define user interface
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML('
      /* add glow effect to the border lines */
      .glow-border {
        box-shadow: 0 0 15px #00FFFB;
      }

      /* add glow effect to the text in the title panel */
      .glow-text {
        text-shadow: 0 0 10px #FFFFFF, 0 0 20px #FFFFFF;
      }

      /* adjust the header title style */
      h1.title {
        text-transform: uppercase;
        letter-spacing: 1px;
        font-size: 24px;
        font-weight: bold;
      }
     
      /* adjust the table row style */
      table.dataTable tbody tr.active td {
        color: black !important;
        box-shadow: inset 0 0 0 9999px "#3E45C490" !important;
      }
     
      /* adjust the main panel style */
      .mainPanel {
        height: calc(100vh - 75px);
        display: flex;
        flex-direction: column;
        align-items: flex-start;
      }

      /* adjust the map height */
      #map {
        flex-grow: 1;
        height: 70vh !important; 
      }

      /* adjust the reset button style */
      .reset-button {
        position: absolute;
        top: 1em;
        right: 1em;
      }
     
      .mainPanel {
        position: relative;
        overflow: hidden;
      }
     
      .output-container {
        width: 100%;
        height: 100px; /* set an initial height for the output container */
        transition: height 0.3s ease; /* add smooth transition for height change */
      }
     
      .collapsed {
        height: 0 !important; /* set height to 0 when collapsed */
        overflow: hidden; /* hide content when collapsed */
      }
     
      .expanded {
        height: 200px !important; 
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
    div(
      class = "title-panel",
      tags$h1(class = "title glow-text", "Nashville Airbnb Crime Map"),
    )
  ),
  sidebarLayout(
    sidebarPanel(
      div(class = "glow-border sidebarPanel",
          dateRangeInput("dateRange", label = "Date Range", format = "yyyy-mm-dd"),
          pickerInput(
            inputId = "crime_type",
            label = "Filter By Crime Type",
            choices = NULL,
            options = list(`actions-box` = TRUE, size = 10),
            multiple = TRUE
          ),
          plotlyOutput("sunburst"),
          d3Output("chart")
      )
    ),
    mainPanel(
      div(class = "glow-border mainPanel",
          leafletOutput("map"),
          div(class = "button-container",
              actionButton('resetButton', 'Reset'),
              span(actionButton('toggleSizeButton', 'Expand Map'),
                   style = "position:absolute;right:1em;"),
          ),
          div(id = "outputContainer",
              style = "width: 100%; height: calc(100vh - 200px); overflow-y: auto;",
              DT::dataTableOutput("table"),
          )
      )
    )
  )
)

# define server logic
server <- function(input, output, session) {
  
  tableState <- reactiveVal(FALSE)  # create a reactive value to store state of table - expanded or collapsed
  
  observeEvent(tableState(), {  
    toggleLabel <- ifelse(tableState(), "Show Table", "Expand Map")  # choose label based on the current state of tableState
    updateActionButton(session, "toggleSizeButton", label = toggleLabel)  # update the label of the toggleSizeButton
  })
  
  observeEvent(input$toggleSizeButton, {  
    tableState(!tableState())  # toggle the value of tableState
    
    shinyjs::toggleClass(selector = "#outputContainer", class = "collapsed")  # toggle the 'collapsed' class of the outputContainer div
    
    toggleState <- input$toggleSizeButton %% 2 == 0  # check if the toggleSizeButton has been clicked an even number of times
    
    if (!toggleState) {  # if toggleState is FALSE
      
      shinyjs::show("outputContainer")  # expands map
      leafletProxy("map") %>%  
        setView(lat = 36.1627, lng = -86.7816, zoom = 12)  # zoom in when expanding map
      
    } else {  # if toggleState is TRUE
      
      leafletProxy("map") %>%  
        setView(lat = 36.1627, lng = -86.7816, zoom = 11)  # realign map view when table is shown
    }
    
    shinyjs::runjs("window.dispatchEvent(new Event('resize'));")  # trigger a window resize event to align viewport correctly
    
  })

  # store click as reactiveValues for leaflet map click events
  click <- reactiveValues(clickedMarker=NULL)
  
  # save the data from the API as a reactive expression
  crimes <- reactive({  
    data <- fetchData() 
    data$uid <- seq.int(nrow(data))  # add a unique identifier column to the data
    data  # return the modified data as the output of the reactive expression
  })
  
  # define a reactive expression for unique offenses to use as choices in pickerInput 
  unique_offense_description <- reactiveVal()
  
  # define the eventReactive expression to fetch data and update pickerInput choices based on the date range
  updatePickerChoices <- eventReactive(input$dateRange, {
    
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
  
  # define a reactive expression to filter data based on date range and selected crime types
  filtered_data <- reactive({
    data <- crimes()  
    data[data$Date >= input$dateRange[1] & data$Date <= input$dateRange[2] & data$offense_description %in% input$crime_type, ]
  })
  
  # define a reactive expression to convert filtered data to a spatial object
  crimes_sf <- reactive({
    data <- filtered_data() 
    filtered_data <- data[data$offense_description %in% input$crime_type, ]
    st_as_sf(filtered_data, coords = c("longitude", "latitude"), crs = 4326)
  })
  
  # update the pickerInput choices when the date range changes
  observe({
    updatePickerChoices()
  })
  
  # define a reactive expression to locate crimes within a quarter mile of Airbnb properties
  quarter_mile <- reactive({
    st_is_within_distance(ab_sf, crimes_sf(), dist = 402.336)
  })
  
  # update the date range input based on the available data
  observe({
    data <- crimes()
    if (!is.null(data)) {
      minDate <- min(data$Date)
      maxDate <- max(data$Date)
      updateDateRangeInput(session, "dateRange", start = minDate, end = maxDate,
                           min = minDate, max = maxDate)
    }
  })
  
  # reset the page when the reset button is clicked
  observeEvent(input$resetButton, {
    updatePickerChoices()
    proxy <- DT::dataTableProxy('table')
    DT::replaceData(proxy, table_data())
    click$clickedMarker <- NULL
    
    # reset the pickerInput selection
    updatePickerInput(
      session = session,
      inputId = "crime_type",
      selected = unique_offense_description()
    )
  })
  
  # define a reactive expression for the indices of crimes within a quarter mile
  quarter_mile_indices <- reactive({
    quarter_mile()
  })
  
  # filter the data based on the clicked marker ID
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
  
  # define reactive values for number of crimes within a quarter mile of each airbnb property
  rv <- reactiveValues(crime_number = NULL)
  airbnb_data <- reactiveVal(airbnb)
  
  observe({
    # calculate the crime numbers for the filtered data
    crime_number <- sapply(quarter_mile_indices(), length)
    
    # store the crime numbers in rv$crime_number
    rv$crime_number <- crime_number
    
    # merge rv$crime_number with airbnb_data
    if (!is.null(rv$crime_number) && !is.null(airbnb_data())) {
      airbnb_merged <- airbnb_data() %>%
        mutate(crime_number = rv$crime_number)
      airbnb_data(airbnb_merged)
    }
  })
  
  # define a reactive value to store the Airbnb property name - may use this later
  selected_airbnb <- reactiveVal(NULL)
  
  # observer for airbnb property click
  observeEvent(input$map_shape_click, {
    click$clickedMarker <- input$map_shape_click
    
    # store the selected Airbnb property name
    if (!is.null(input$map_shape_click$id)) {
      selected_airbnb(airbnb_data()$name[input$map_shape_click$id])
    } else {
      selected_airbnb(NULL)
    }
  })
  
  # sunburst chart for crimes within a quarter mile of Airbnb
  output$sunburst <- renderPlotly({
    if (is.null(click$clickedMarker)) {
      table_data() %>%
        count(paste(nrow(table_data()), "Crimes"), crime_category, offense_description) %>%
        counts_to_sunburst() %>%
        layout(
          colorway = c("#3E45C490", "#FFA50099", "#FF004D90"),
          paper_bgcolor = '#142730',
          title = "All Crimes In Nashville",
          font = list(color = "#00FFFB"),
          margin = mrg,
          showlegend = FALSE,
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    } else {
      table_data() %>%
        count(paste(nrow(table_data()), "Crimes"), crime_category, offense_description) %>%
        counts_to_sunburst() %>%
        layout(
          colorway = c("#3E45C490", "#FFA50099", "#FF004D90"),
          paper_bgcolor = '#142730',
          title = paste(nrow(table_data()), "crimes within a quarter mile"),
          margin = mrg,
          showlegend = FALSE,
          font = list(color = "#00FFFB"),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    }
  })
  
  # set colors for D3 barchart to match theme of app
  options(r2d3.theme = list(background = "#142730", foreground = '#00FFFB'))
  
  # D3 barplot for locations of crime incidents
  output$chart <- renderD3({
    chart_data <- table_data() %>%
      group_by(location_description) %>%
      summarize(count = n()) %>%
      top_n(10, count) %>%
      arrange(desc(count)) %>%
      as.data.frame()
    
    chart_options <- list(
      margin = 50,
      barPadding = 0.1,
      color = "#00FFFB50",
      xLabel = "Location Description",
      yLabel = "Total Incidents",
      chartTitle = "Incident Location"
    )
    
    chart_data %>%
      r2d3(
        script = "Custom_D3.js",
        data = .,
        options = chart_options
      )
  })
  
  # define reactive color pallette that reacts to date input and crime type selection
  color_palette <- reactive({
    if (!is.null(airbnb_data()$crime_number)) {
      max_value <- max(airbnb_data()$crime_number, na.rm = TRUE)
      
      if (max_value <= 400) {
        # define number of bins and range
        crime_bins <- c(0, 1, 5, 15, 25, 50, 75, 100, 150, 200, 300, 400)
        
        # create color palette with the specified bins
        colorBin(
          palette = c("#3A0889FF", "#2D3184FF", "#1E4F8EFF", "#09679AFF", 
                      "#007DA4FF", "#0A92ACFF", "#28A4B3FF", "#43B5B8FF",
                      "#5EC3BBFF", "#77CFBEFF", "#8FDAC0FF", "#A5E2C3FF"),
          bins = crime_bins
        )
      } else {
        # define the number of bins and range with an additional bin for values greater than 400
        crime_bins <- c(0, 1, 5, 15, 25, 50, 75, 100, 150, 200, 300, 400, max_value)
        
        # create color palette with the extra bin and an extra color
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
    # calculate color breaks based on the crime numbers in airbnb_data
    if (!is.null(airbnb_data()$crime_number)) {
      num_intervals <- 11
      colorQuantile(airbnb_data()$crime_number, n = num_intervals + 1)
    }
  })
  
  rv_labels <- reactive({
    # generate labels based on the crime numbers and breaks in airbnb_data
    if (!is.null(airbnb_data()$crime_number)) {
      cut(isolate(airbnb_data()$crime_number), breaks = rv_breaks(), labels = FALSE, include.lowest = TRUE)
    }
  })
  
  label_ranges <- reactive({
    # create label ranges based on the breaks in rv_breaks
    if (!is.null(rv_breaks())) {
      paste0(rv_breaks()[-length(rv_breaks())], "-", rv_breaks()[-1])
    }
  })
  
  # Leaflet map code
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(mapbox, attribution) %>%
      clearMarkers() %>%
      addCircles(
        data = airbnb_data(),
        radius = 3,
        weight = 2,
        opacity = 1,
        fillOpacity = 1,
        label = paste(
          airbnb_data()$name,
          "<br>Total Crimes Within A Quarter Mile:",
          airbnb_data()$crime_number
        ) %>% lapply(htmltools::HTML),
        group = "Airbnb",
        layerId = ~uid,
        color = ~color_palette()(crime_number),
        popup = paste(
          "<h3>Airbnb</h3>",
          airbnb$popup3,
          "<br>",
          "Name:",
          airbnb$popup2,
          "<br>",
          airbnb$popup
        )
      ) %>%
      addSearchFeatures(
        targetGroups = "Airbnb",
        options = searchFeaturesOptions(
          zoom = 12,
          openPopup = TRUE,
          firstTipSubmit = TRUE,
          autoCollapse = TRUE,
          hideMarkerOnCollapse = TRUE,
          textPlaceholder = "Search For An Airbnb Property"
        )
      ) %>%
      addCircleMarkers(
        data = crimes_sf(),
        radius = 3,
        weight = 3,
        color = "red",
        layerId = as.character(crimes_sf()$id),
        label = paste(
          crimes_sf()$offense_description,
          "<br>Date:",
          crimes_sf()$`Date`,
          "<br>Time:",
          crimes_sf()$`Time of Incident`
        ) %>% lapply(htmltools::HTML),
        group = "Crime Layer",
        clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 1.5)
      ) %>% 
      setView(lat = 36.1627, lng = -86.7816, zoom = 11) %>% 
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
      addLayersControl(
        overlayGroups = c("Airbnb", "Crime Layer"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  # render datatable with certain columns 
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
    
    # alias the column names
    colnames(filtered_data) <- c(
      "Incident Number", "Incident Status",
      "Date", "Time of Incident",
      "Incident Location", "Location Description",
      "Offense Description", "Weapon Description",
      "Victim Number", "Domestic Related",
      "Victim Description", "Victim Gender", "Victim Race",
      "Victim Ethnicity", "Victim County Resident"
    )
    
    DT::datatable(
      filtered_data,
      selection = "single",
      options = list(
        scrollX = TRUE,
        scrollY = "340px"
      )
    )
  })
  
}

shinyApp(ui, server)
