library(shiny)
library(bslib)
library(thematic)
library(shinythemes)
library(sass)
library(plotly)
library(dplyr)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(RColorBrewer)
library(htmltools)
library(ggplot2)
library(DT)
library(glue)
library(plotly)
library(viridis)
library(mapproj)
library(pals)
library(colourvalues)
library(paletteer)
library(lubridate)
library(shinyWidgets)
#thematic::thematic_shiny(font = "auto")

options(shiny.reactlog = TRUE)

theme <- bslib::bs_theme(version = 5, bootswatch = "superhero")

ui <- fluidPage(
  theme = theme,
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
                     start = '2022-07-10', 
                     end = '2023-01-20',
                     min = '2022-07-10', 
                     max = '2023-01-20'
      ), 
      pickerInput(
        inputId = "crime_type",
        label = "Filter By Crime Type",
        choices = sort(unique(crimes$Offense)),
        selected = crimes$Offense,
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      ),
      plotlyOutput("donut"),
      plotlyOutput("bar")
    ),
    
    mainPanel(
      leafletOutput('mymap', width = 1100, height = 500),
      
      fluidRow (class="table"),
      DT::dataTableOutput("table1")
    )
  )
)

server <- function(input, output, session) {
  #bs_themer()
  
  click <- reactiveValues(clickedMarker=NULL)
  
  #define reactives 
  date_filter <- reactive({ 
    crimes %>% 
      filter(Date >= input$crimedates[1] & Date <= input$crimedates[2]) %>% 
      filter(Offense %in% input$crime_type)
    
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
      addProviderTiles("CartoDB.DarkMatter", group = "Dark Theme") %>% 
      addProviderTiles("Stamen.Toner", group = "Light Theme")  %>% 
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
      addLegend("bottomright", colors = c("#3A0889FF", "#2D3184FF", "#1E4F8EFF", "#09679AFF", 
                                          "#007DA4FF", "#0A92ACFF", "#28A4B3FF", "#43B5B8FF", 
                                          "#5EC3BBFF", "#77CFBEFF", "#8FDAC0FF", "#A5E2C3FF", 
                                          "#CBEDCAFF", "#E5E598FF", "#C5AD40FF", "#A4661EFF", "#7E1900FF"), 
                                          labels = c("0", "1-5", "5-10", "11-15", "16-25", "26-50", "51-75", 
                                                     "76-100", "101-150", "151-200", "201-300", "301-400", 
                                                     "401-700", "701-1400", "1401-1500", "1501-1600", "1601-1700"), 
                values = ~crime_number,
                title = "Crimes",
                opacity = 1) %>%
      addLayersControl(overlayGroups =c("Airbnb",
                                        "Crime Layer",
                                        "No Crime"), 
                       baseGroups = c("Dark Theme", 
                                      "Light Theme"),
                       options = layersControlOptions(collapsed = TRUE)) %>% 
      hideGroup("No Crime") %>%
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
             xaxis = list(title = "Location Description", showgrid = T, zeroline = F, 
                          showticklabels = T, categoryorder = "total descending"),
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
    updatePickerInput(
      session,
      inputId = "crime_type",
      choices = sort(unique(crimes$Offense)),
      selected = crimes$Offense
    )
    proxy1 <- DT::dataTableProxy('table1')
    DT::replaceData(proxy1, date_filter())
    click$clickedMarker <- NULL
    map_leaf %>% clearMarkers()
    #session$reload()
  })
  
}

shinyApp(ui, server, options = list(height = 1080))