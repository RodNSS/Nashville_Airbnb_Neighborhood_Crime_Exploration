library(shiny)
library(bslib)
library(thematic)
library(shinythemes)
thematic::thematic_shiny(font = "auto")

options(shiny.reactlog = TRUE)

pal3 <- colorNumeric(palette = as.character(paletteer_c("grDevices::RdYlBu", n=1600)), 
                     domain  = c(min(total$crime_number), max(total$crime_number)), reverse=T)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "superhero"),
  titlePanel(tagList(span("Nashville Airbnb Crime Map",
                          span(actionButton('reset', 'Reset'),
                               style = "position:absolute;right:1em;")
      )
    )
  ),
  
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
      plotlyOutput("bar")
    ),
    
    mainPanel(
      leafletOutput('mymap', width = 1245, height = 500),
      
      fluidRow (class="table"),
      DT::dataTableOutput("table1")
    )
  )
)

server <- function(input, output, session) {
  #bs_themer()
  
  click <- reactiveValues(clickedMarker=NULL)
  
  #define reactives 
  filtered <- reactive({
    total
  })
  
  date_filter <- reactive({ 
    crimes %>% 
      filter(Date >= input$crimedates[1] & Date <= input$crimedates[2])
    
  })
  
  table_data <- reactive({
    if(is.null(click$clickedMarker)){
      
      table_data <- date_filter() 
    }
    else{
      marker_uid <- crimes[quarter_mile[[click$clickedMarker$id]], ] %>% 
        pull(uid)
      table_data <- date_filter() %>% 
        filter(uid %in% marker_uid)
      
    }
    return(table_data)
  })
  
  output$mymap <- renderLeaflet({
    
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
                 label = paste(total$name, "<br>Total Crimes Within A Quarter Mile:", total$crime_number) %>% 
                   lapply(htmltools::HTML),
                 popup = paste("<h3>Airbnb</h3>", total$popup3, "<br>", "Name:",
                               total$popup2,"<br>", total$popup),
                 color= ~pal_fun2(crime_number), group = "Airbnb", layerId = ~uid, highlightOptions = (bringToFront = TRUE)) %>%
      addCircles(data = no_crime, radius = 3, weight = 2, opacity = 1, fillOpacity = 1, 
                 label = paste(no_crime$name, "<br>Crimes:", no_crime$crime_number) %>% lapply(htmltools::HTML),
                 popup = paste("<h3>Airbnb</h3>", no_crime$popup3, "<br>", "Name:",
                               no_crime$popup2,"<br>", no_crime$popup),
                 color= "#009E60", group = "No Crime", layerId = ~uid) %>%
      addLegend("bottomright", pal= pal_fun2, values = ~crime_number,
                title = "Crimes",
                opacity = 1) %>% 
      addLayersControl(overlayGroups =c("Airbnb",
                                        "Crime Layer",
                                        "No Crime"), 
                       baseGroups = c("Dark Version", 
                                      "Light Version"),
                       options = layersControlOptions(collapsed = TRUE)) %>% 
      hideGroup(c("Crime Layer", "No Crime")) %>%
      addSearchFeatures(
        targetGroups = "Airbnb", 
        options = searchFeaturesOptions(zoom = 15,
                                        openPopup = TRUE, 
                                        firstTipSubmit = TRUE,
                                        autoCollapse = TRUE, 
                                        hideMarkerOnCollapse = TRUE,
                                        textPlaceholder = "Search For An Airbnb Property")) %>%
      addCircleMarkers(data = date_filter(),
                       radius = 3,
                       weight=3,
                       color="red",
                       label = ~paste("<b>", Offense,"</b>", "<br>Weapon:", Weapon,
                                      "<br>Date:", Date, "<br>Incident #:", Incident, "<br>Street:",
                                      Street, "<br>Location Type:", Location) %>% 
                         lapply(htmltools::HTML), 
                       clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5),
                       popup = ~paste("<b>",Offense,"</b>", "<br>Weapon:", Weapon,
                                      "<br>Date:", Date, "<br>Incident #:", Incident, "<br>Street:",
                                      Street, "<br>Location Type:", Location),
                       group = "Crime Layer",
                       layerId = as.character(date_filter()$id)) %>% 
      setView(lat = 36.1627, lng = -86.7816, zoom = 11) %>%
      #addMiniMap(tiles="CartoDB.DarkMatter") %>% 
      addResetMapButton() 
  })
  # observer for airbnb property click
  observeEvent(input$mymap_shape_click,{
    click$clickedMarker <- input$mymap_shape_click
  })
  # plot for crimes within a quarter mile of airbnb
  output$donut <- renderPlotly({
    if(is.null(click$clickedMarker)
    )
      crime_plot
    else
      table_data() %>% 
      group_by(Offense) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~Offense,
              textposition = "inside",
              values= ~count, 
              marker = list(colors = virid,
                            line = list(color = '#FFFFFF', 
                                        width = 1))) %>%
      add_pie(hole = 0.5) %>%
      layout(colorway = c("#376597FF", "#AF6458FF", "#A89985FF", "#6B452BFF", "#3A3E3FFF", 
                                     "#FFFEEAFF", "#855C75FF", "#D9AF6BFF", "#736F4CFF", "#526A83FF", 
                                     "#625377FF", "#68855CFF", "#9C9C5EFF", "#A06177FF", "#8C785DFF", 
                                     "#467378FF", "#7C7C7CFF", "#002b36", "#EFE1C6FF", "#839496", 
                                     "#D9B196FF", "#657b83", "#93a1a1", "#104A8AFF", "#537270FF", 
                                     "#556246FF", "#2B323FFF", "#928F6BFF", "#CCAF69FF", "#eee8d5", 
                                     "#C5AC8EFF", "#889F95FF", "#48211AFF", "#00295DFF"),
                                     paper_bgcolor='#8C9DA6',
             title = paste(nrow(table_data()),
                           "crimes within a quarter mile"), margin = mrg, showlegend = F,
             xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
             yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    
  })
  # barplot for locations of crime
  output$bar <- renderPlotly({
    if(is.null(click$clickedMarker))
      barplot
    else
      table_data() %>%
      group_by(Location) %>%
      summarize(count = n()) %>%
      plot_ly(x = ~Location,
              y = ~count,
              name = "Location",
              type = "bar",
              #orientation = 'h',
              marker = list(color = bar_pal,
                            line = list(color = '#8C9DA6', 
                                        width = 1))) %>% 
      layout(paper_bgcolor='#8C9DA6', plot_bgcolor ='#8C9DA6',
             xaxis = list(title = "Location Description", showgrid = T, zeroline = F, showticklabels = T),
             yaxis  = list(title = "Count", showgrid = T, zeroline = F, showticklabels = T))
  })
  
  map_leaf <- leafletProxy("mymap") 
  # observer for clearing markers by clicking map
  observeEvent(input$mymap_click,{
    map_leaf %>% clearMarkers() 
    
  })
  
  # datatable output and formatting
  output$table1 <- DT::renderDataTable({
    DT::datatable(
      table_data(), 
      selection = "single",
      options = list(stateSave = TRUE,
                     pageLength = 5,
                     lengthMenu = c(5, 25, 50, 100),
                     scrollX = TRUE,
                     scrollY = "250px",
                     rownames=TRUE)) %>%
      formatStyle(0, target= 'row', #fontWeight ='bold',
                  lineHeight='80%'
      ) 
    
  })
  
  #icon for highlighting specific crime when clicking on datatable
  highlight_icon = makeAwesomeIcon(icon = 'flag', 
                                   markerColor = 'red', 
                                   iconColor = 'white')
  #observers for clicking/highlighting between map and datatable
  observeEvent(input$table1_rows_selected, {
    row_selected = table_data()[input$table1_rows_selected, ]
    
    proxy <- leafletProxy('mymap') 
    proxy %>% clearMarkers() %>%
      addAwesomeMarkers(popup = paste("<b>",row_selected$Offense,"</b>", "<br>Weapon:", row_selected$Weapon,
                                      "<br>Date:", row_selected$Date, "<br>Incident #:", row_selected$Incident, 
                                      "<br>Street:", row_selected$Street, "<br>Location Type:", row_selected$Location),
                        layerId = as.character(row_selected$uid),
                        lng = row_selected$Longitude,
                        lat = row_selected$Latitude,
                        icon = highlight_icon,
                        group = "markers") %>%
      flyTo(lng = row_selected$Longitude, lat = row_selected$Latitude, 
            if(is.null(input$mymap_shape_click))zoom = 12 else zoom=15)
    
  })
  # reset page
  observeEvent(input$reset, {
    proxy1 <- DT::dataTableProxy('table1')
    DT::replaceData(proxy1, date_filter())
    click$clickedMarker <- NULL
    map_leaf %>% clearMarkers()
    #session$reload()
  })
  
}

shinyApp(ui, server, options = list(height = 1080))