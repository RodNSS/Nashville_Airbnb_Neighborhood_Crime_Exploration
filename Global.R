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
library(httr)

airbnb <- read_csv("listings.csv")

mapbox <- Sys.getenv("MAPBOX_API_KEY")
attribution <- "© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> <strong><a"

# pull in stored shots fired and shooting data from google sheets
shots <- gsheet2tbl('google_sheets_link')

# drop rows with any NA values
shots <- shots[complete.cases(shots), ]

# format to "year-month-day"
shots$Date <- mdy(shots$Date)  
#shots$Date <- as.Date(shots$Date, format = "%Y-%m-%d")

# parse and format the time
shots$Time <- strftime(strptime(shots$Time, format = "%H:%M:%S"), format = "%l:%M %p")

# filter data to exclude anything from 3 months before the current date
three_months_ago <- Sys.Date() - months(3)
shots <- shots[shots$Date >= three_months_ago, ]

# define the threshold for coordinate variation to avoid duplicate calls for same incident
threshold <- 0.1 

# filter out incidents where the coordinates vary by more than one-tenth of a degree
filtered_results <- shots %>%
  group_by(Date, Time, City) %>%
  filter(max(Lat) - min(Lat) > threshold | max(Long) - min(Long) > threshold) %>%
  ungroup()

shots <- shots %>%
  mutate(
    Incident_Type = case_when(
      Incident_Type == "SHOOTING IN PROGRESS JUVENILE" ~ "SHOOTING",
      Incident_Type == "SHOTS FIRED-JUVENILE" ~ "SHOTS FIRED",
      TRUE ~ Incident_Type
    )
  ) %>%
  # filter out duplicate Shooting/Shots Fired incidents based on conditions
  group_by(Date, Time, Address) %>%
  filter(!(Incident_Type == "SHOTS FIRED" & "SHOOTING" %in% Incident_Type)) %>%
  ungroup() %>%
  # keep only unique combinations of Date, Time, and City
  distinct(Date, Time, City, .keep_all = TRUE)

# combine the distinct and filtered results
shots <- bind_rows(filtered_results, shots)

# specify column mapping for join
column_mapping <- c("Time" = "Time of Incident",
                    "Date" = "Date",
                    "Lat" = "Latitude",
                    "Long" = "Longitude",
                    "Incident_Type" = "Offense_Description",
                    "Full_Address" = "Incident_Location")

# subset and rename columns in shots based on the column_mapping
shots <- shots %>%
  select(all_of(names(column_mapping))) %>%
  rename_with(~ column_mapping[.], everything())

# function to fetch data with offset pagination
fetchData <- function(url, max_records = 2000, total_records = 16000) {
  url <- "https://services2.arcgis.com/HdTo6HJqh92wn4D8/arcgis/rest/services/Metro_Nashville_Police_Department_Incidents_view/FeatureServer/0/query"
  all_data <- list()
  offset <- 0
  records_fetched <- 0
  more_records <- TRUE
  
  while(more_records && records_fetched < total_records) {
    
    # define query parameters with pagination
    query_params <- list(
      outFields = "Incident_Number,Investigation_Status,Incident_Location,Latitude,Longitude,Location_Description,Offense_Description,Weapon_Description,Victim_Number,Domestic_Related,Victim_Description,Victim_Gender,Victim_Race,Victim_Ethnicity,Victim_County_Resident,Incident_Occurred,Incident_Status_Description",
      where = "Incident_Occurred > CURRENT_TIMESTAMP - INTERVAL '90' DAY",
      f = "geojson",
      resultOffset = offset,
      resultRecordCount = max_records
    )
    
    # make GET request
    response <- GET(url, query = query_params)
    if (http_error(response)) {
      stop("HTTP request failed: ", http_status(response)$reason)
    }
    
    # parse JSON response content
    content <- content(response, "text", encoding = "UTF-8")
    geojson_content <- st_read(content, quiet = TRUE)
    
    # append data to list
    all_data <- append(all_data, list(geojson_content))
    
    # update offset and records fetched
    records_fetched <- records_fetched + nrow(geojson_content)
    offset <- offset + max_records
    
    # check if there are more records to fetch
    if (nrow(geojson_content) < max_records) {
      more_records <- FALSE
    }
  }
  
  # combine all fetched data and remove duplicates
  all_data_combined <- do.call(rbind, all_data)
  all_data_combined <- distinct(all_data_combined)  
  data <- st_drop_geometry(all_data_combined)
  
  # convert milliseconds to Central US Time
  data$Incident_Occurred <- as.POSIXct(data$Incident_Occurred / 1000, origin = "1970-01-01", tz = "UTC")
  data$Incident_Occurred <- format(data$Incident_Occurred, tz = "America/Chicago", usetz = TRUE)
  data$Incident_Occurred <- format(as.POSIXct(data$Incident_Occurred, tz = "America/Chicago"), "%m-%d-%Y %l:%M %p")
  
  # create separate Date and Time column and remove Incident_Occurred column
  data$Date <- as.Date(data$Incident_Occurred, format = "%m-%d-%Y %l:%M %p")
  data$`Time of Incident` <- format(as.POSIXct(data$Incident_Occurred, format = "%m-%d-%Y %l:%M %p"), format = "%l:%M %p")
  data$Incident_Occurred <- NULL
  
  # filter out entries with "Incident_Status_Description" as "UNFOUNDED"
  data <- data[data$Incident_Status_Description != "UNFOUNDED", ]
  data <- data[data$Offense_Description != "TEST ONLY", ]
  
  # remove rows with missing latitude or longitude values
  data <- data[complete.cases(data[, c("Latitude", "Longitude")]), ]
  
  selected_cols <- c(
    "Incident_Number", "Incident_Status_Description", "Investigation_Status",
    "Incident_Location", "Latitude", "Longitude",
    "Location_Description", "Offense_Description", "Weapon_Description",
    "Victim_Number", "Domestic_Related", "Victim_Description",
    "Victim_Gender", "Victim_Race", "Victim_Ethnicity", "Victim_County_Resident",
    "Date", "Time of Incident"
  )
  
  data <- data[selected_cols]
  
  # found duplicate offenses in slightly different format
  data$Offense_Description <- gsub("BURGLARY- MOTOR VEHICLE", "BURGLARY - MOTOR VEHICLE", data$Offense_Description)
  data$Offense_Description <- gsub("BURGLARY- AGGRAVATED", "BURGLARY - AGGRAVATED", data$Offense_Description)
  
  # combine dataframes and lowercase column names
  data <- bind_rows(data, shots)
  names(data) <- tolower(names(data))
  
  # separate crimes into categories
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
                                 "WEAPON - FIREARM OR CLUB", "WEAPON - FIREARM OR CLUB W/PRIORS", "WEAPON OFFENSE, CRIMINAL ATTEMPT", "WEAPON- CARRYING ON SCHOOL PROPERTY",
                                 "SHOOTING", "SHOTS FIRED") ~ "Violent Crime",
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

# add Google Street View links
airbnb$coord<-paste(airbnb$latitude,airbnb$longitude,sep=",")
airbnb$links <- glue("http://maps.google.com/maps?q=&layer=c&cbll={airbnb$coord}")

# format labels
airbnb <- airbnb %>% 
  mutate(
    popup2 = paste0('<a target=_blank href =', listing_url, '>', name, '</a>'),
    popup = paste0('<a target=_blank href =', links, '>Street View</a>'),
    popup3 = paste0("<img src='", picture_url, "' width='200px' height='100px'>")
  )

# add unique id to each row for filtering
airbnb$uid <- seq.int(nrow(airbnb))

# convert airbnb dataframe to sf object
ab_sf <- st_as_sf(airbnb, coords =c("longitude","latitude"), crs =4326)

# custom margin for Plotly title
mrg <- list(l = 50, r = 50,
            b = 50, t = 50)

# javascript code for enabling popup when datatable is clicked, used this - https://stackoverflow.com/questions/66240941/r-leaflet-add-a-new-marker-on-the-map-with-the-popup-already-opened
js_save_map_instance <- HTML(
  paste(
    "var mapsPlaceholder = [];",
    "L.Map.addInitHook(function () {",
    "   mapsPlaceholder.push(this); // Use whatever global scope variable you like.",
    "});", sep = "\n"
  )
)

js_open_popup <- HTML(
  paste("function open_popup(id) {",
        "   console.log('open popup for ' + id);",
        "   mapsPlaceholder[0].eachLayer(function(l) {",
        "      if (l.options && l.options.layerId == id) {",
        "         l.openPopup();",
        "      }",
        "   });",
        "}", sep = "\n"
  )
)

# function for getting custom icons - can expand on this later
getIconUrl <- function(offense_description) {
  if (grepl("HOMICIDE", offense_description)) {
    "https://www.svgrepo.com/show/286584/weapon-target.svg"
  } else if (offense_description == "BURGLARY - MOTOR VEHICLE" || offense_description == "VEHICLE THEFT") {
    "https://www.svgrepo.com/show/284078/police-car-car.svg"
  } else {
    "https://www.svgrepo.com/show/402496/police-car-light.svg"
  }
}

# function for sunburst chart. Slightly tweaked this https://github.com/yogevherz/plotme/blob/master/R/count_to_sunburst_treemap.R
counts_to_sunburst <- function(count_data, fill_by_n = FALSE, sort_by_n = FALSE){
  
  params <- create_all_col_params(count_data, fill_by_n, sort_by_n)

  # create custom color mapping function so colors remain the same for each crime category
  get_color <- function(category) {
    if (grepl("Violent Crime", category)) "#FF004D90"
    else if (grepl("Property Crime", category)) "#FFA50099"
    else if (grepl("Other", category)) "#3E45C490"
    else NA  # make the root (center) colorless
  }
  
  # assign colors based on the level and category
  color_vector <- sapply(seq_along(params$ids), function(i) {
    id_parts <- strsplit(params$ids[i], "\\.->.") %>% unlist()
    if (length(id_parts) == 1) {
      NA  # root (center) remains colorless
    } else if (length(id_parts) == 2) {
      get_color(id_parts[2])  # second level (main categories)
    } else {
      get_color(id_parts[2])  # offenses inherit parent category color
    }
  })
  
  purrr::exec(plotly::plot_ly,
              !!!params,
              type = "sunburst",
              branchvalues = "total",
              hoverinfo = "text",
              textinfo = "label+percent parent",
              textfont = list(color = "#00FFFB"),
              marker = list(
                colors = color_vector,
                line = list(color = "#00FFFB", width = 1)
              )
  )
}

#' @export
#' @rdname count_to_sunburst

create_all_col_params <- function(count_data, fill_by_n, sort_by_n){
  
  assert_count_df(count_data)
  assertthat::assert_that(is.logical(fill_by_n),
                          length(fill_by_n) == 1,
                          msg = "fill_by_n must be either TRUE or FALSE")
  assertthat::assert_that(is.logical(sort_by_n),
                          length(sort_by_n) == 1,
                          msg = "sort_by_n must be either TRUE or FALSE")
  
  count_data <- all_non_n_cols_to_char(count_data)
  
  category_num <- ncol(count_data) - 1
  
  params <- purrr::map(1:category_num,
                       create_one_col_params,
                       df = count_data,
                       root = "") %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(sort = sort_by_n)
  
  if(fill_by_n){
    params <- params %>%
      dplyr::mutate(marker = list(
        colorbar = list(
          bgcolor = ""
        )
      ))
  }
  params
}

create_one_col_params <- function(df,
                                  col_num,
                                  root){
  col_name <- names(df)[col_num]
  
  df %>%
    dplyr::group_by(dplyr::across(1:dplyr::all_of(col_num))) %>%
    dplyr::summarise(values = sum(.data$n), .groups = "drop") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ids = paste(dplyr::c_across(1: !!col_num),
                  collapse = ".->."),
      parents = ifelse(!!col_num > 1,
                       paste(dplyr::c_across(1 :(!!col_num - 1)),
                             collapse = ".->."),
                       root)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(labels = .[[!!col_num]],
                  hovertext = stringr::str_glue(
                    "{labels}\nTotal Incidents: {values}")
    ) %>%
    dplyr::select(ids, parents, labels, values, hovertext)
}

assert_count_df <- function(var){
  msg <- paste(substitute(var), "must be a count dataframe (output of dplyr::count)")
  assertthat::assert_that(is.data.frame(var),
                          assertthat::has_name(var, "n"),
                          msg = msg)
  
  n_col <- var$n
  assertthat::assert_that(is.numeric(n_col), msg = msg)
}

all_non_n_cols_to_char <- function(df){
  df %>%
    dplyr::mutate(dplyr::across(!matches("^n$"), as.character))
}
