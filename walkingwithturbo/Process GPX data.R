
### Process GPX data and extract elevation for tracks ###

library(tidyverse)
library(sf)
library(elevatr)
library(sfarrow)

source("utils.R")


#################
### Load data ###
#################

# Select all .gpx files
gpx_files <- dir(path = "Data", full.names = TRUE)

# Read in GPX files
tracks_all <- map(gpx_files, st_read, layer = "track_points") |> 
  bind_rows() |> 
  dplyr::select(time) |> 
  mutate(datetime = time,
         date = as_date(time),
         time = str_extract(time, "[0-9]{2}:[0-9]{2}:[0-9]{2}")) |> 
  nest(.by = date)

# Previously processed tracks
tracks_old <- st_read_parquet("Data_processed/tracks.parquet") |> 
  nest(.by = date)




# Filter out previously processed data
tracks_new <- tracks_all |> 
  filter(!date %in% tracks_old$date)


### Only continue w/ data processing if new tracks exist
if (nrow(tracks_new) != 0) {
  
  # Convert tracks to sf objects
  tracks_new_sf <- tracks_new |> 
    unnest(cols = data) |> 
    st_as_sf(crs = 4326, remove = FALSE)
  
  
  
  
  #########################################
  ### Wrangle data for use in dashboard ###
  #########################################
  
  # Get elevation for points
  tracks_new_sf2 <- get_elev_point(locations = tracks_new_sf)
  
  # Convert to LINESTRING object
  tracks_new_sf3 <- tracks_new_sf2 |> 
    group_by(date) |> 
    mutate(geom_lead = lead(geometry)) |> 
    slice(-n()) |> 
    mutate(dist = as.numeric(st_distance(geometry, geom_lead, by_element = TRUE))) |>  #calc step length
    filter(dist != 0) |>  #remove obs where location hasn't changed
    rowwise() |> 
    mutate(geometry = st_union(geometry, geom_lead) |> 
             st_cast("LINESTRING")) |> 
    ungroup() |> 
    dplyr::select(datetime, date, time, elevation)
  
  
  ###################
  ### Export data ###
  ###################
  
  # Merge w/ tracks_old
  tracks_update <- rbind(unnest(tracks_old, cols = data) |> 
                           st_as_sf(crs = 4326) |> 
                           relocate(datetime, date, time, elevation),
                         tracks_new_sf3)
  
  st_write_parquet(tracks_update, "Data_processed/tracks.parquet")
  
}

### Remove all intermediate objects
rm(tracks_all, tracks_new, tracks_old, parse_gpx)

