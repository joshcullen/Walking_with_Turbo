
# Refer to https://repo.r-wasm.org for packages supported by shinylive/webR

library(tidyverse)
library(sf)
library(leaflet)
library(sfarrow)
library(dygraphs)
library(shiny)
library(bslib)
library(duckplyr)
# library(ggiraph)
library(reactable)



# source("Process GPX data.R")
# source("Convert images to jpeg.R")
source("utils.R")


## Load data
tracks_fine <- st_read_parquet("https://github.com/joshcullen/Walking_with_Turbo/raw/main/walkingwithturbo/Data_processed/tracks.parquet") |> 
  split(~date) |> 
  map(~mutate(.x,
              sl = as.numeric(st_length(.x)),
              cumul_dist = cumsum(sl) * 0.000621371,  #convert from m to miles
              elevation = round(elevation / 3.28084, 2))  #convert from m to ft
  ) |> 
  bind_rows()

tracks_summary <- tracks_fine |> 
  summarize(.by = date,
            across(geometry, st_union),
            duration = difftime(last(datetime), first(datetime), units = "mins") |> 
              round(2),
            distance = max(cumul_dist, na.rm = TRUE) |> 
              round(2),  #m to miles
            elev_gain = max(elevation, na.rm = TRUE) - min(elevation, na.rm = TRUE)#,
            # do_union = FALSE
  )

# Starting coordinates for map view
start_coords <- tracks_summary[nrow(tracks_summary),] |> 
  st_centroid() |> 
  st_coordinates()


# track.pal <- colorFactor(palette = "Spectral", domain = unique(tracks_summary$date))



## UI
ui <- page_sidebar(
  tags$head(tags$style(HTML('.bslib-page-title {font-size: 3rem;
                            background-color: #006D6F;
                            color: #FADA5E;}',
                            '.bslib-value-box .value-box-title {font-size: 1.3rem}'))),
  
  theme = bs_theme(bootswatch = "materia",
                   version = 5,
                   fg = "#141635",
                   bg = "#FFF",
                   base_font = font_google("Love Ya Like A Sister")),
  title = "Walks with Turbo",
  sidebar = sidebar(
    width = 350,
    # class = "bg-secondary",
    selectInput('track_date', 'Choose a track:', 
                tracks_summary$date, tracks_summary$date[nrow(tracks_summary)]),
    tags$iframe(
      style="border-radius:12px", 
      src="https://open.spotify.com/embed/playlist/1Y9ZG5Au7ujuE143KIdXQl?utm_source=generator&theme=0", 
      width="100%", 
      height="152", 
      frameBorder="0", 
      allowfullscreen="", 
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture", 
      loading="lazy"),
    # imageOutput("photo"),
    htmlOutput("picture", fill = "container"),
    value_box(
      title = "Number of walks",
      nrow(tracks_summary),
      showcase = icon("paw", class = 'fa-3x'),
      # showcase_layout = "top right",
      theme = value_box_theme(bg = "#006D6F", fg = "#FADA5E"),
      height = "100px"
    )
  ),
  card(leafletOutput("map"), full_screen = TRUE),
  layout_column_wrap(card(reactableOutput("tbl")), card(dygraphOutput("elev_prof")))
)


## Server
server <- function(input, output, session) {
  
  # bs_themer()
  
  # Update photo based on selection
  # output$photo <- renderUI({
  #   # value <- isolate(input$dynamic)
  #   img(src = paste0(input$track_date, ".jpeg"), align = "center")
  # })
  # output$photo <- renderImage({
  #   filename <- paste0("www/", input$track_date, ".jpeg")
  #   list(src = filename,
  #        width = 300,
  #        height = 400,
  #        align = "center")
  # }, deleteFile = FALSE)
  
  output$picture <- renderText({
    src <- paste0("https://github.com/joshcullen/Walking_with_Turbo/blob/main/walkingwithturbo/www/",
                  input$track_date,
                  ".jpeg?raw=true")
    
    c('<img src="',src,'">')
    })
  
  
  # Create reactive objects
  tracks_fine_r <- reactive({
    tracks_fine |> 
      duckplyr::filter(date == input$track_date) |> 
      drop_na(elevation)
  })
  
  # Remove selected track from summary df
  tracks_summary2 <- reactive({
    tracks_summary |> 
      duckplyr::filter(date != input$track_date)
  })
  
  
  
  # Plot Leaflet map
  basemap <- leaflet() |>
    setView(lng = as.numeric(start_coords[,'X']),
            lat = as.numeric(start_coords[,'Y']),
            zoom = 14) |> 
    addProviderTiles(provider = providers$Esri.WorldImagery, group = "World Imagery",
                     options = tileOptions(zIndex = -10)) |>
    addProviderTiles(provider = providers$OpenStreetMap, group = "Open Street Map",
                     options = tileOptions(zIndex = -10)) |>
    addLayersControl(baseGroups = c("World Imagery", "Open Street Map"),
                     overlayGroups = c("Tracks","Selected Track"),
                     options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) |>
    addScaleBar(position = "bottomleft")
  
  output$map <- renderLeaflet(basemap)
  
  
  
  
  observe({
    # req(tracks_fine_r()) # Do this if tracks_fine_r() is not null
    
    
    elev.pal <- colorNumeric(palette = "viridis",
                             domain = range(tracks_fine_r()$elevation))
    
    leafletProxy("map") |> 
      clearControls() |>
      clearShapes() |>
      fitBounds(min(st_coordinates(tracks_fine_r())[,1]),
                min(st_coordinates(tracks_fine_r())[,2]),
                max(st_coordinates(tracks_fine_r())[,1]),
                max(st_coordinates(tracks_fine_r())[,2])
      ) |>
      addPolylines(data = tracks_summary2(),
                   color = "lightgrey",
                   opacity = 0.7,
                   weight = 3,
                   label = ~paste0("<b>Date:</b> ", date,
                                   "<br> <b>Duration (min):</b> ", duration,
                                   "<br> <b>Distance (miles):</b> ", distance,
                                   "<br> <b>Elevation gain (ft):</b> ", elev_gain) |>
                     lapply(htmltools::HTML),
                   labelOptions = labelOptions(style = list(
                     "font-size" = "14px")),
                   highlightOptions = highlightOptions(opacity = 1,
                                                       bringToFront = TRUE,
                                                       weight = 5),
                   group = "Tracks") |> 
      addPolylines(data = tracks_fine_r(),
                   color = ~elev.pal(elevation),
                   opacity = 0.5,
                   weight = 3,
                   label = ~paste0("<b>Date:</b> ", date,
                                   "<br> <b>Time:</b> ", time,
                                   "<br> <b>Elevation (ft):</b> ", elevation) |>
                     lapply(htmltools::HTML),
                   labelOptions = labelOptions(style = list(
                     "font-size" = "14px")),
                   highlightOptions = highlightOptions(opacity = 1,
                                                       bringToFront = TRUE,
                                                       weight = 4),
                   group = "Selected Track") |>
      addLegend_decreasing(title = "Elevation (ft)",
                pal = elev.pal,
                values = range(tracks_fine_r()$elevation),
                decreasing = TRUE)
  })
  
  
  
  
  
  
  observe({
    # Elevation profile
    # elev_plot <- highchart() |> 
    #   hc_add_series(data = tracks_fine_r(),
    #                 hcaes(x = round(cumul_dist, 2), y = elevation),
    #                 type = "spline",
    #                 # name = "<b>Elevation (ft)</b>",
    #                 color = "firebrick",
    #                 lineWidth = 5,
    #                 tooltip = list(pointFormat = "<b>Elevation (ft):</b> {point.elevation}")
    #   ) |> 
    #   hc_xAxis(title = list(text = "Distance (miles)",
    #                         style = list(fontSize = 20)),
    #            labels = list(style = list(fontSize = 15))) |> 
    #   hc_yAxis(title = list(text = "Elevation (ft)",
    #                         style = list(fontSize = 20)),
    #            labels = list(style = list(fontSize = 15))) |> 
    #   hc_legend(enabled = FALSE) |> 
    #   hc_tooltip(crosshairs = TRUE,
    #              headerFormat = "<b>Distance (miles):</b> {point.key}<br>"
    #   )
    elev_plot <- dygraph(tracks_fine_r() |> 
                           st_drop_geometry() |> 
                           duckplyr::select(cumul_dist, elevation) |> 
                           duckplyr::mutate(cumul_dist = round(cumul_dist, 2))) |> 
      dySeries(strokeWidth = 4, color = "firebrick", label = "Elevation (ft)") |> 
      dyAxis("y", label = "Elevation (ft)", axisLabelFontSize = 15, axisLabelWidth = 50,
             labelWidth = 25) |> 
      dyAxis("x", label = "Distance (miles)", axisLabelFontSize = 15, axisLabelWidth = 50,
             labelHeight = 25) |> 
      dyOptions(axisLineWidth = 2, drawGrid = FALSE) |> 
      dyHighlight(highlightCircleSize = 5,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE) |> 
      dyCrosshair(direction = "vertical") |> 
      dyLegend(width = 150, show = "always", labelsSeparateLines = TRUE)
    
    
    # output$elev_prof <- renderHighchart(elev_plot)
    output$elev_prof <- renderDygraph(elev_plot)
  })
  
  
  
  # Summary table
  output$tbl <- renderReactable({
    reactable(data = st_drop_geometry(tracks_summary),
              defaultColDef = colDef(
                # header = function(value) gsub(".", " ", value, fixed = TRUE),
                # cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 70,
                headerStyle = list(background = "#f7f7f8")
              ),
              defaultSorted = list(date = "desc"),
              filterable = TRUE,
              defaultPageSize = 6,
              paginationType = "jump",
              columns = list(
                date = colDef(name = "Date", align = "left", style = list(fontWeight = 800)),
                duration = colDef(name = "Duration (min)"),
                distance = colDef(name = "Distance (mi)"),
                elev_gain = colDef(name = "Elevation Gain (ft)")
              )
    )
  })
  
  
  
}



# Create Shiny app
shinyApp(ui = ui, server = server)
