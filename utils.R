
# Parse the GPX file

parse_gpx = function() {
  
  # Interactively select .gpx file to load
  gpx_files <- dir(path = "Data", full.names = TRUE)
  
  # Begin file parsing
  parsed_files <- map(gpx_files, ~htmlTreeParse(.x, error = function (...) {}, useInternalNodes = T))
  
  # Create list of tracks
  track_list <- map(parsed_files,
                    ~{
                      times <- xpathSApply(.x, path = "//trkpt/time", xmlValue)
                      coords <- xpathSApply(.x, path = "//trkpt", xmlAttrs)
                      
                      track_df <- data.frame(lat = as.numeric(coords["lat",]),
                                             lon = as.numeric(coords["lon",]),
                                             datetime = times) |> 
                        mutate(datetime = as_datetime(datetime, tz = "America/New_York"))
                    })

  
  return(track_list)
  
}
