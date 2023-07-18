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

airbnb <- read_csv("listings.csv")

mapbox <- Sys.getenv("MAPBOX_API_KEY")
attribution <- "© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> <strong><a"

# function to fetch data from the API
fetchData <- function() {
  url <- "https://data.nashville.gov/resource/2u6v-ujjs.json?$$app_token={APP TOKEN GOES HERE}$limit=15000"
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
  
  purrr::exec(plotly::plot_ly,
              !!!params,
              type = "sunburst",
              branchvalues = "total",
              hoverinfo = "text",
              textinfo = "label+percent parent",
              textfont = list(color = "#00FFFB"),
              marker = list(#colors = virid,
                line = list(color = "#00FFFB", 
                            width = 1)))
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