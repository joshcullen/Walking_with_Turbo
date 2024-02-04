
### Process GPX data and extract elevation for tracks ###

library(XML)
library(tidyverse)
library(sf)
library(elevatr)
library(sfarrow)

source("utils.R")


#################
### Load data ###
#################

# Previously processed tracks
tracks_old <- st_read_parquet("Data_processed/tracks.parquet") |> 
  nest(.by = date)


# Parse GPX files (from Data folder)
tracks_all <- parse_gpx() |> 
  bind_rows() |> 
  mutate(date = as_date(datetime),
         time = str_extract(datetime, "[0-9]{2}:[0-9]{2}:[0-9]{2}")) |> 
  nest(.by = date)

# Filter out previously processed data
tracks_new <- tracks_all |> 
  filter(!date %in% tracks_old$date)


### Only continue w/ data processing if new tracks exist
if (nrow(tracks_new) != 0) {
  
  # Convert tracks to sf objects
  tracks_new_sf <- tracks_new |> 
    unnest(cols = data) |> 
    st_as_sf(coords = c('lon','lat'), crs = 4326, remove = FALSE)
  
  
  
  
  #########################################
  ### Wrangle data for use in dashboard ###
  #########################################
  
  # Get elevation for points
  tracks_new_sf2 <- get_elev_point(locations = tracks_new_sf)
  
  # Convert to LINESTRING object
  tracks_new_sf3 <- tracks_new_sf2 |> 
    mutate(geom_lead = lead(geometry)) |> 
    slice(-n()) |> 
    rowwise() |> 
    mutate(line_seg = st_union(geometry, geom_lead) |> 
             st_cast("LINESTRING")) |> 
    ungroup() |> 
    st_set_geometry("line_seg") |> 
    dplyr::select(datetime, date, time, elevation)
  
  
  ###################
  ### Export data ###
  ###################
  
  # Merge w/ tracks_old
  tracks_update <- rbind(unnest(tracks_old, cols = data) |> 
                           st_as_sf(crs = 4326, sf_column_name = "line_seg") |> 
                           relocate(datetime, date, time, elevation),
                         tracks_new_sf3)
  
  st_write_parquet(tracks_update, "Data_processed/tracks.parquet")
  
}

### Remove all intermediate objects
rm(tracks_all, tracks_new, tracks_old, parse_gpx)

