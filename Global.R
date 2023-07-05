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

# Add Google Street View links
airbnb$coord<-paste(airbnb$latitude,airbnb$longitude,sep=",")
airbnb$links <- glue("http://maps.google.com/maps?q=&layer=c&cbll={airbnb$coord}")

# Format labels
airbnb <- airbnb %>% 
  mutate(
    popup2 = paste0('<a target=_blank href =', listing_url, '>', name, '</a>'),
    popup = paste0('<a target=_blank href =', links, '>Street View</a>'),
    popup3 = paste0("<img src='", picture_url, "' width='200px' height='100px'>")
  )

# Add unique id to each row for filtering
airbnb$uid <- seq.int(nrow(airbnb))

# Convert airbnb dataframe to sf object
ab_sf <- st_as_sf(airbnb, coords =c("longitude","latitude"), crs =4326)

# Custom margin for Plotly title
mrg <- list(l = 50, r = 50,
            b = 50, t = 50)

# Function for sunburst chart. Slightly tweaked this https://github.com/yogevherz/plotme/blob/master/R/count_to_sunburst_treemap.R
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