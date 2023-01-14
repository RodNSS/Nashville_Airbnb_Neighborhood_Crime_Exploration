library(shiny)
library(bslib)
library(thematic)
thematic::thematic_shiny(font = "auto")

options(shiny.reactlog = TRUE)

pal3 <- colorNumeric(palette = as.character(paletteer_c("scico::roma", n=200)), 
                     domain  = c(min(total$crime_number), max(total$crime_number)), reverse=T)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "superhero"),
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
          plotlyOutput("donut"),
          actionButton('reset', 'Reset')
        ),
  
  mainPanel(
  leafletOutput('mymap', width = 800, height = 600),
  
  fluidRow (class="table"),
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
  
  # date_filter_copy <- reactiveValues({ 
  #   crimes %>% 
  #     #filter(Offense_Description %in% input$crimeselector) %>%  
  #     filter(Incident_Occurred >= input$crimedates[1] & Incident_Occurred <= input$crimedates[2])
  #   
  # })
  
  output$mymap <- renderLeaflet({
    
    crimes$id <- seq.int(nrow(crimes))
    # main leaflet map
    nash %>% 
      leaflet(options = leafletOptions(minZoom = 10, preferCanvas = TRUE))  %>% 
      addProviderTiles("CartoDB.DarkMatter", group = "Dark Version") %>% 
      addProviderTiles("Stamen.Toner", group = "Light Version")  %>% 
       addLayersControl(
        baseGroups = c(
          "ST", "HM"
        ),
        position = "topleft"
      ) %>%
      #addPolygons(data=nash, color ="grey", opacity=1, fillOpacity = 0) %>%
      addCircles(data = total, radius = 3, weight = 2, opacity = 1, fillOpacity = 1, 
                 label = paste(total$name, "<br>Crimes:", total$crime_number) %>% lapply(htmltools::HTML),
                 popup = paste("<h3>Airbnb</h3>", total$popup3, "<br>", "Name:",
                               total$popup2,"<br>", total$popup),
                 color= ~pal3(crime_number), group = "Airbnb", layerId = ~uid) %>%
      # addCircles(data = no_crime, radius = 3, weight = 2, opacity = 1, fillOpacity = 1, label = ~name,
      #            popup = paste("<h3>Airbnb</h3>", no_crime$popup3, "<br>", "Name:",
      #                          no_crime$popup2,"<br>", no_crime$popup),
      #            color= "#959c9e", group = "No Crime", layerId = ~uid) %>%
      # addCircles(data = el_crime, radius = 3, weight = 2, opacity = 1, fillOpacity = 1, label = ~name,
      #            popup = paste("<h3>Airbnb</h3>", el_crime$popup3, "<br>", "Name:", 
      #                          el_crime$popup2,"<br>" ,el_crime$popup),
      #            color= "#309143", group = "Extremely Low Crime", layerId = ~uid) %>%
      # addCircles(data = l_crime, radius = 3, weight = 2, opacity = 1, fillOpacity = 1, label = ~name,
      #            popup = paste("<h3>Airbnb</h3>", l_crime$popup3, "<br>", "Name:",
      #                          l_crime$popup2,"<br>" ,l_crime$popup),
      #            color= '#8ace7e', group = "Low Crime", layerId = ~uid) %>%
      # addCircles(data = ave_crime, radius = 3, weight = 2, opacity = 1, fillOpacity = 1, label = ~name,
      #            popup = paste("<h3>Airbnb</h3>", ave_crime$popup3, "<br>", "Name:",
      #                          ave_crime$popup2,"<br>" ,ave_crime$popup),
      #            color= "#ffda66", group = "Average Crime", layerId = ~uid) %>%
      # addCircles(data = high_crime, radius = 3, weight = 2, opacity = 1, fillOpacity = 1, label = ~name,
      #            popup = paste("<h3>Airbnb</h3>", high_crime$popup3, "<br>", "Name:",
      #                          high_crime$popup2,"<br>" ,high_crime$popup),
      #            color= "#ff684c", group = "High Crime", layerId = ~uid) %>%
      # addCircles(data = eh_crime, radius = 3, weight = 2, opacity = 1, fillOpacity = 1, label = ~name,
      #            popup = paste("<h3>Airbnb</h3>", eh_crime$popup3, "<br>", "Name:",
      #                          eh_crime$popup2,"<br>" ,eh_crime$popup),
      #            color= "#b60a1c", group = "Extremely High Crime", layerId = ~uid) %>%
    addLegend("bottomright", pal = pal3, values = ~crime_number,
              title = "Crimes",
              opacity = 1) %>% 
                # labels = c("No Crime",
                #            "Extremely Low Crime",
                #            "Low Crime",
                #            "Average Crime",
                #            "High Crime",
                #            "Extremely High Crime" ),
                # title = "Crime Within a Quarter Mile",
                # opacity=1) %>%
      addLayersControl(overlayGroups =c("Airbnb",
                                        "Crime Data"), baseGroups = c(
                                          "Dark Version", "Light Version"
                                        ),
                       options = layersControlOptions(collapsed = TRUE)) %>% 
      hideGroup("Crime Data") %>% 
      addSearchFeatures(
        targetGroups = "Airbnb", 
        options = searchFeaturesOptions(zoom =14,
                                        openPopup = TRUE, 
                                        firstTipSubmit = TRUE,
                                        autoCollapse = FALSE, 
                                        hideMarkerOnCollapse = TRUE,
                                        textPlaceholder = "Search For An Airbnb Property")) %>%
      addCircleMarkers(data = date_filter(), 
                       radius = 3, 
                       weight=3, 
                       color="red", 
                       label = paste(date_filter()$Offense_Description, 
                                     "<br>Date:", date_filter()$Incident_Occurred) %>% 
                         lapply(htmltools::HTML), 
                       clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5),
                       popup = ~as.character(Offense_Description),
                       group = "Crime Data",
                       layerId = as.character(date_filter()$id)) %>% 
      # addMeasure(
      #   position = "topleft",
      #   primaryLengthUnit = "meters",
      #   primaryAreaUnit = "sqmeters",
      #   activeColor = "#0bd3d3",
      #   completedColor = "#f890e7"
      # ) %>% 
      setView(lat = 36.1627, lng = -86.7816, zoom = 11) %>%
      #addMiniMap(tiles="CartoDB.DarkMatter") %>% 
      addResetMapButton()
  })
  # observer for airbnb property click
  observeEvent(input$mymap_shape_click,{
    data_of_click$clickedMarker <- input$mymap_shape_click
  })
  # plot for crimes within a quarter mile of airbnb
  output$donut <- renderPlotly({
    if(is.null(input$mymap_shape_click) #| !is.null(input$mymap_click)
       )
      crimes_sf %>%
      group_by(offense) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~offense,
              textposition = "inside",
              values= ~count, 
              marker = list(colors = virid,
                            line = list(color = '#FFFFFF', 
                                        width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(paper_bgcolor='#8C9DA6',
             title = "All Crimes In Nashville",  showlegend = F,
             xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
             yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    # else if(!is.null(input$mymap_shape_click) | !is.null(input$mymap_click))
    #   crimes_sf[quarter_mile[[data_of_click$clickedMarker$id]], 6] %>%
    #   group_by(offense) %>%
    #   summarize(count = n()) %>%
    #   plot_ly(labels = ~offense,
    #           textposition = "inside",
    #           values= ~count, 
    #           marker = list(colors = virid,
    #                         line = list(color = '#FFFFFF', 
    #                                     width = 1))) %>%
    #   add_pie(hole = 0.6) %>%
    #   layout(paper_bgcolor='#8C9DA6',
    #          title = paste(nrow(crimes_sf[quarter_mile[[data_of_click$clickedMarker$id]], 6]),
    #                        "crimes within a quarter mile"),  showlegend = F,
    #          xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    #          yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    else
    crimes_sf[quarter_mile[[data_of_click$clickedMarker$id]], 6] %>%
      group_by(offense) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~offense,
              textposition = "inside",
              values= ~count, 
              marker = list(colors = virid,
                            line = list(color = '#FFFFFF', 
                                        width = 1))) %>%
      add_pie(hole = 0.6) %>%
      layout(paper_bgcolor='#8C9DA6',
        title = paste(nrow(crimes_sf[quarter_mile[[data_of_click$clickedMarker$id]], 6]),
                           "crimes within a quarter mile"),  showlegend = F,
             xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
             yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    
  })
  
  map_leaf <- leafletProxy("mymap") 
  # observer for drawn circle on map with quarter mile radius from center
   observeEvent(input$mymap_click,{
    #data_of_click$clickedMarker <- NULL
    # click <- input$mymap_click
    # f_map <- filtered()
    # map_leaf %>% clearGroup("new_point") %>%
    #   addCircles(click$lng,
    #              click$lat, weight = 1,
    #              radius = 402.336,
    #              color = "black",
    #              fillColor = "red",
    #              fillOpacity = .2,
    #              group= "new_point",
    #              highlightOptions = (bringToFront = FALSE))
     
     proxy1 <- DT::dataTableProxy('table1')
     DT::replaceData(proxy1, date_filter())
     data_of_click$clickedMarker <- NULL
    

  })
  
  # datatable output and formatting
  output$table1 <- DT::renderDataTable({
    DT::datatable(
       if(is.null(input$mymap_shape_click) #| !is.null(input$mymap_click)
          )
        date_filter()
       # else if(is.null(input$mymap_click))
       #   date_filter()  
       else
        #crime_filter(),
        date_filter()[quarter_mile[[data_of_click$clickedMarker$id]], ], 
                  selection = "single",
                  options = list(stateSave = TRUE,
                                 pageLength = 5,
                                 lengthMenu = c(5, 10, 20),
                                 scrollX = TRUE,
                                 rownames=TRUE)) %>%
                  formatStyle(0, target= 'row', #fontWeight ='bold',
                              lineHeight='80%'
                             ) 
      
  })
  
  prev_row <- reactiveVal()

  # new icon style
  highlight_icon = makeAwesomeIcon(icon = 'flag', 
                                   markerColor = 'red', 
                                   iconColor = 'white')
  #observers for clicking/highlighting between map and datatable
  observeEvent(input$table1_rows_selected, {
    if(is.null(input$mymap_shape_click))
    {row_selected = date_filter()[input$table1_rows_selected, ]
     } #else if(!is.null(input$reset)){
    #   row_selected = date_filter()[input$table1_rows_selected, ]
    # }
    # 
    # else if(is.null(input$reset)){
    #   row_selected = date_filter()[quarter_mile[[data_of_click$clickedMarker$id]], ][input$table1_rows_selected, ]
    # }
    
     #else if(!is.null(input$reset) & !is.null(input$mymap_shape_click)){
    #   row_selected = date_filter()[quarter_mile[[data_of_click$clickedMarker$id]], ][input$table1_rows_selected, ]
    # } 
    else{
      row_selected = date_filter()[quarter_mile[[data_of_click$clickedMarker$id]], ][input$table1_rows_selected, ]
    }
    proxy <- leafletProxy('mymap') 
      proxy %>% #clearGroup("new_point") %>%
    #   addCircles(data_of_click$clickedMarker$lng,
    #              data_of_click$clickedMarker$lat, weight = 1,
    #              radius = 402.336,
    #              color = "black",
    #              fillColor = "red",
    #              fillOpacity = .2,
    #              group= "new_point",
    #              highlightOptions = (bringToFront = FALSE)) %>% 
      addAwesomeMarkers(popup = as.character(row_selected$Offense_Description),
                        layerId = as.character(row_selected$uid),
                        lng = row_selected$Longitude,
                        lat = row_selected$Latitude,
                        icon = highlight_icon) %>%
      flyTo(lng = row_selected$Longitude, lat = row_selected$Latitude, 
            if(is.null(input$mymap_shape_click))zoom = 12 else zoom=15)
  

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

  # observeEvent(input$mymap_marker_click, {
  #   clickId <- input$mymap_marker_click$id
  #   dataTableProxy("table1") %>%
  #     selectRows(which(date_filter()$id == clickId)) %>%
  #     selectPage(which(input$table1_rows_all == clickId) %/% input$table1_state$length + 1)
  
   observeEvent(input$reset, {
    proxy1 <- DT::dataTableProxy('table1')
    DT::replaceData(proxy1, date_filter())
    
   #session$reload()
   
   # if(!is.null(input$reset)){
   #   row_selected = date_filter()[input$table1_rows_selected, ]
   # }
   # 
   # else{
   #   row_selected = date_filter()[quarter_mile[[data_of_click$clickedMarker$id]], ][input$table1_rows_selected, ]
   # }
   })
  
}

shinyApp(ui, server, options = list(height = 1080))