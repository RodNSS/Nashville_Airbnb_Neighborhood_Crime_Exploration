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
library(gsheet)

# execute global variables file first
source("Global.R")

# define user interface
ui <- fluidPage(
  tags$head(
    tags$script(type = "text/javascript",
                js_save_map_instance,
                js_open_popup),
    useShinyjs()
  ),
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
      height: 83vh !important; 
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
  ')),
  # overall theme colors
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
      span(style = "margin-right: 10px;"),
      tags$a(href = "https://github.com/RodNSS/Nashville_Airbnb_Neighborhood_Crime_Exploration", 
             target = "_blank", 
             icon("github", lib = "font-awesome")),
      style = "display: flex; align-items: flex-end;"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      div(
        class = "glow-border sidebarPanel",
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
      div(
        class = "glow-border mainPanel",
        leafletOutput("map"),
        div(
          class = "button-container",
          actionButton('resetButton', 'Reset'),
          span(
            actionButton('toggleSizeButton', 'Expand Map'),
            style = "position:absolute;right:1em;"
          )
        ),
        tags$style(HTML('table.dataTable tr.active td, table.dataTable td.active {background-color: #00FFFB !important;}')),
        div(
          id = "outputContainer",
          style = "width: 100%; height: calc(100vh - 100px); overflow-y: auto;",
          DT::dataTableOutput("table")
        ),
        # JS for sending click event when property is selected in search bar
        tags$script(HTML("
      $(document).on('shiny:connected', function() {
        Shiny.setInputValue('leaflet_search_control_ready', true);
      });
      
      Shiny.addCustomMessageHandler('initialize_search_control', function(message) {
        var map = $('#map').data('leaflet-map');
        if (map && map.searchControl) {
          map.searchControl.on('search:locationfound', function(e) {
            var id = e.layer && e.layer.options ? e.layer.options.layerId : null;
            Shiny.setInputValue('map_shape_click', {
              id: id
            }, {priority: 'event'});
          });
        }
      });
    "))
      )
    )
  )
)

# define server logic
server <- function(input, output, session) {

  # event handler for property selections from the search bar.
  observe({
    req(input$leaflet_search_control_ready)
    session$sendCustomMessage("initialize_search_control", list())
  })
  
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
  
  # save the data from the API as a reactive expression
  crimes <- reactive({
    data <- fetchData()
    data$uid <- seq.int(nrow(data)) # add a unique identifier column to the data for filtering
    data
  })
  
  # define a reactive expression to filter data based on date range and selected crime types
  filtered_data <- reactive({
    data <- crimes()  
    data[data$Date >= input$dateRange[1] & data$Date <= input$dateRange[2] & data$offense_description %in% input$crime_type, ]
  })
  
  # define a reactive expression to convert filtered crime data to a spatial object for mapping
  crimes_sf <- reactive({
    data <- filtered_data() 
    filtered_data <- data[data$offense_description %in% input$crime_type, ]
    st_as_sf(filtered_data, coords = c("longitude", "latitude"), crs = 4326)
  })
  
  # define a reactive expression for unique offenses to use as choices in pickerInput
  unique_offense_description <- reactiveVal()
  
  # define the eventReactive expression to fetch data and update pickerInput choices based on the date range
  updatePickerChoices <- eventReactive(input$dateRange, {
    data <- crimes()
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
  
  # update the pickerInput choices when the date range changes
  observe({
    updatePickerChoices()
  })
  
  # update the date range input based on the data
  observe({
    data <- crimes()
    if (!is.null(data)) {
      minDate <- min(data$Date)
      maxDate <- max(data$Date)
      updateDateRangeInput(session, "dateRange", start = minDate, end = maxDate,
                           min = minDate, max = maxDate)
    }
  })
  
  # define a reactive expression to locate crimes within a quarter mile of Airbnb properties
  quarter_mile <- reactive({
    st_is_within_distance(ab_sf, crimes_sf(), dist = 402.336)
  })
  
  # define a reactive expression for the indices of crimes within a quarter mile
  quarter_mile_indices <- reactive({
    quarter_mile()
  })
  
  # store click as reactiveValues for leaflet map click events
  click <- reactiveValues(clickedMarker=NULL)
  
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
  
  # reset the datatable and charts after an Airbnb property click event
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
  
  # sunburst chart for crimes within a quarter mile of airbnb
  output$sunburst <- renderPlotly({
    if(is.null(click$clickedMarker)
    ) 
      table_data() %>%
      count(paste(nrow(table_data()),"Crimes"), crime_category, offense_description) %>%
      counts_to_sunburst() %>%
      layout(#colorway = c("#3E45C490", "#FFA50099", "#FF004D90"), 
             paper_bgcolor='#142730',
             title = "All Crimes In Nashville", 
             font = list(color = "#00FFFB"),
             margin = mrg,
             showlegend = F,
             xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
             yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    else
      table_data() %>%
      count(paste(nrow(table_data()),"Crimes"), crime_category, offense_description) %>%
      counts_to_sunburst() %>%
      layout(#colorway = c("#3E45C490", "#FFA50099", "#FF004D90"),
             paper_bgcolor='#142730',
             title = paste(nrow(table_data()), "crimes within a quarter mile"), 
             margin = mrg, 
             showlegend = F, 
             font = list(color = "#00FFFB"),
             xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
             yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    
  })
  
  # set colors for D3 barchart to match theme of app
  options(r2d3.theme = list(background = "#142730", foreground = '#00FFFB'))
  
  # D3 barplot for locations of crime incidents
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
  
  # leaflet map 
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(mapbox, attribution) %>% 
      addLayersControl(
        overlayGroups = c("Airbnb", "Crime Layer"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
      setView(lat = 36.1627, lng = -86.7816, zoom = 11) %>%
      addSearchFeatures(
        targetGroups ="Airbnb", 
        options = searchFeaturesOptions(zoom = 14,
                                        openPopup = TRUE, 
                                        firstTipSubmit = TRUE,
                                        autoCollapse = TRUE, 
                                        hideMarkerOnCollapse = TRUE,
                                        textPlaceholder = "Search For An Airbnb Property")) %>%
      addResetMapButton()
  })
  
  # add Airbnb circles and crime markers to the map
  observe({
    leafletProxy("map") %>%
      clearGroup("Airbnb") %>%
      addCircles(
        data = airbnb_data(),
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
        popup = paste("<h3>Airbnb</h3>", airbnb$popup3, 
                      "<br>Name:", airbnb$popup2,
                      "<br>", airbnb$popup)) %>%
      clearGroup("Crime Layer") %>% 
      addCircleMarkers(
        data = crimes_sf(),
        radius = 3,
        weight = 3,
        color = "red",
        layerId = as.character(crimes_sf()$id),
        label = paste("<b>", crimes_sf()$offense_description, "</b>", 
                      "<br>Date:", crimes_sf()$`Date`, 
                      "<br>Time:", crimes_sf()$`Time of Incident`,
                      "<br>Incident #:", crimes_sf()$incident_number, 
                      "<br>Street:", crimes_sf()$incident_location, 
                      "<br>Location Type:", crimes_sf()$location_description,
                      "<br>Weapon:", crimes_sf()$weapon_description) %>% 
          lapply(htmltools::HTML), 
        popup = paste("<b>", crimes_sf()$offense_description, "</b>", 
                      "<br>Date:", crimes_sf()$`Date`, 
                      "<br>Time:", crimes_sf()$`Time of Incident`,
                      "<br>Incident #:", crimes_sf()$incident_number, 
                      "<br>Street:", crimes_sf()$incident_location, 
                      "<br>Location Type:", crimes_sf()$location_description,
                      "<br>Weapon:", crimes_sf()$weapon_description),
        group = "Crime Layer",
        clusterOptions = markerClusterOptions(
          spiderfyDistanceMultiplier = 1.5) 
      ) %>% 
      addLegend(
        "bottomright",
        pal = color_palette(),
        values = airbnb_data()$crime_number,
        opacity = 1,
        title = "Crimes",
        layerId = "legend"
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
    
    DT::datatable(filtered_data, 
                  selection = "single",
                  options = list(
                    scrollX = TRUE,
                    scrollY = "320px"
                  )
    )
  })
  
  # add icons to the map and open popup on datatable row click
  observeEvent(input$table_rows_selected, {
    selected_row <- table_data()[input$table_rows_selected, ]
    id <- paste0("marker", input$table_rows_selected)
    icon_url <- getIconUrl(selected_row$offense_description)
    
    leafletProxy("map") %>%
      addMarkers(
        lng = as.numeric(selected_row$longitude),
        lat = as.numeric(selected_row$latitude),
        layerId = id,
        icon = makeIcon(iconUrl = icon_url, iconWidth = 30, iconHeight = 30, iconAnchorY = 20),
        popup = paste("<b>", selected_row$offense_description, "</b>", 
                      "<br>Date:", selected_row$Date, 
                      "<br>Time:", selected_row$`Time of Incident`,
                      "<br>Incident #:", selected_row$incident_number, 
                      "<br>Street:", selected_row$incident_location, 
                      "<br>Location Type:", selected_row$location_description,
                      "<br>Weapon:", selected_row$weapon_description),
        group = "Markers"
      ) 
    
    # uses JS function from Global.R to open popup on map
    shinyjs::runjs(sprintf("setTimeout(() => open_popup('%s'), 20)", id))
  })
  
  # clear markers by clicking map
  observeEvent(input$map_click,{
    leafletProxy("map") %>% clearMarkers()
  })
}

shinyApp(ui, server)
