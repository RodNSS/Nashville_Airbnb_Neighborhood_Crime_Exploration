#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(thematic)
thematic::thematic_shiny(font = "auto")

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  titlePanel("Nashville Airbnb Crime Map"),
  
  sidebarLayout(
    
    sidebarPanel(
            dateRangeInput("crimedates",
                           label = ("Date Range"),
                           width=380,
                           start = '2022-07-01', 
                           end = '2023-01-04',
                           min = '2022-07-01', 
                           max = '2023-01-04'
                ),
          plotlyOutput("donut")
        ),
  
  mainPanel(
  leafletOutput('mymap', width = 800, height = 600),
  DT::dataTableOutput("table1")
  )
)
)
server <- function(input, output, session) {
  bs_themer()
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  #define reactives 
  filtered <- reactive({
    total
  })
  
  date_filter <- reactive({ 
    crimes %>% 
    #filter(Offense_Description %in% input$crimeselector) %>%  
    filter(Incident_Occurred >= input$crimedates[1] & Incident_Occurred <= input$crimedates[2])
  
  })
  
  output$mymap <- renderLeaflet({
    
    crimes$id <- seq.int(nrow(crimes))
    # main leaflet map
    nash %>% 
      leaflet(options = leafletOptions(minZoom = 10, preferCanvas = TRUE))  %>% 
      addProviderTiles("CartoDB.Positron")  %>% 
      addCircleMarkers(data = total, radius = 2 , weight = 1, opacity = 1, label = ~name,
                       popup = paste("<h3>Airbnb</h3>", total$popup3, "<br>", "Name:", 
                                     total$popup2,"<br>" ,total$popup),
                       color= ~pal(room_type), group = "name", layerId = ~id) %>%
      addSearchFeatures(
        targetGroups ="name", 
        options = searchFeaturesOptions(zoom =14,
                                        openPopup = TRUE, 
                                        firstTipSubmit = TRUE,
                                        autoCollapse = FALSE, 
                                        hideMarkerOnCollapse = TRUE)) %>%
      addCircleMarkers(data = date_filter(), 
                       radius = 3, 
                       weight=3, 
                       color="red", 
                       label = paste(date_filter()$Offense_Description, 
                                     "<br>Date:", date_filter()$Incident_Occurred) %>% 
                         lapply(htmltools::HTML), 
                       clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5),
                       popup = ~as.character(Offense_Description), layerId = as.character(date_filter()$uid)) %>% 
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#0bd3d3",
        completedColor = "#f890e7"
      ) %>% 
      addLegend(position ="topright", pal = pal, values = total$room_type) %>%
      setView(lat = 36.1627, lng = -86.7816, zoom = 11) %>%
      addResetMapButton()
  })
  # observer for airbnb property click
  observeEvent(input$mymap_marker_click,{
    data_of_click$clickedMarker <- input$mymap_marker_click
  })
  # plot for crimes within a quarter mile of airbnb
  output$donut <- renderPlotly({
    crimes_sf[quarter_mile[[data_of_click$clickedMarker$id]], 6] %>%
      group_by(offense) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~offense, 
              values= ~count, 
              marker = list(colors = virid,
                            line = list(color = '#FFFFFF', 
                                        width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(title = paste(nrow(crimes_sf[quarter_mile[[data_of_click$clickedMarker$id]], 6]),
                           "crimes within a quarter mile"),  showlegend = F,
             xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
             yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    
  })
  
  map_leaf <- leafletProxy("mymap") 
  # observer for drawn circle on map with quarter mile radius from center
  observeEvent(input$mymap_click,{
    click <- input$mymap_click
    f_map <- filtered()
    map_leaf %>% clearGroup("new_point") %>% 
      addCircles(click$lng, 
                 click$lat, weight = 1,
                 radius = 402.336, 
                 color = "black", 
                 fillColor = "red", 
                 fillOpacity = .2, 
                 group= "new_point",
                 highlightOptions = (bringToFront = FALSE))
    
  })
  
  # datatable output and formatting
  output$table1 <-  DT::renderDataTable({
    DT::datatable(date_filter(), 
                  selection = "single", 
                  options = list(stateSave = TRUE, 
                                 pageLength = 5,
                                 lengthMenu = c(2, 12, 18))) %>%
                  formatStyle(0, target= 'row', 
                              fontWeight ='bold', 
                              lineHeight='80%'
                             ) #%>% 
      #filter(Offense_Description %in% input$crime_selector) %>%  
      #crimes %>% filter(Incident_Occurred >= input$crimedates[1] & Incident_Occurred <= input$crimedates[2])
  })
  
  prev_row <- reactiveVal()

  # new icon style
  highlight_icon = makeAwesomeIcon(icon = 'flag', 
                                   markerColor = 'red', 
                                   iconColor = 'white')
  #observers for clicking/highlighting between map and datatable
  observeEvent(input$table1_rows_selected, {
    row_selected = date_filter()[input$table1_rows_selected, ]
    proxy <- leafletProxy('mymap')
    proxy %>%
      addAwesomeMarkers(popup = as.character(row_selected$Offense_Description),
                        layerId = as.character(row_selected$uid),
                        lng = row_selected$Longitude,
                        lat = row_selected$Latitude,
                        icon = highlight_icon) #%>%
      #flyTo(lng = date_filter()$Longitude, lat = date_filter()$Latitude, zoom = 12) 

    # Reset previously selected marker
    if(!is.null(prev_row())){
      proxy %>%
        addMarkers(popup = as.character(prev_row()$Offense_Description),
                   layerId = as.character(prev_row()$uid),
                   lng = prev_row()$Longitude,
                   lat = prev_row()$Latitude)
    }

    prev_row(row_selected)
  })

  observeEvent(input$mymap_marker_click, {
    clickId <- input$mymap_marker_click$uid
    dataTableProxy("table1") %>%
      selectRows(which(date_filter()$uid == clickId)) %>%
      selectPage(which(input$table1_rows_all == clickId) %/% input$table1_state$length + 1)
  })
  
}

shinyApp(ui, server, options = list(height = 1080))