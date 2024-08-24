# Load libraries.
library(XML)
library(dplyr)
library(sf)
library(leaflet)

# Repeat steps in activities_explore.R.
gpx <- htmlTreeParse(file = "data/4812281446.gpx", useInternalNodes = TRUE)
name <- xpathSApply(doc = gpx, path = "//trk/name", fun = xmlValue)
type <- xpathSApply(doc = gpx, path = "//trk/type", fun = xmlValue)
coords <- xpathSApply(doc = gpx, path = "//trkpt", fun = xmlAttrs)
elevation <- xpathSApply(doc = gpx, path = "//trkpt/ele", fun = xmlValue)
time <- xpathSApply(doc = gpx, path = "//trkpt/time", fun = xmlValue)

# Extract information into a dataframe.
gpx_sf <- data.frame(
  act_name    = name,
  act_type    = type,
  timestamps  = time,
  lat         = coords["lat", ],
  lon         = coords["lon", ],
  ele         = as.numeric(elevation)
) %>% 
  st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) 

# Create a line out of the points.
gpx_line_sf <- gpx_sf %>% 
  group_by(act_name) %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING") 

# Interactive map.
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron , group = "Positron (default)") %>%
  addProviderTiles(providers$OpenStreetMap    , group = "Open Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>% 
  addPolylines(data = gpx_line_sf, color = "#fc4c02") %>% 
  addLayersControl(
    baseGroups = c(
      "Positron (default)",
      "Open Street Map",
      "World Imagery (satellite)"
    ))
