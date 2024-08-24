# Load libraries.
library(XML)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)

# Parse a single gpx file.
gpx <- htmlTreeParse(file = "data/4812281446.gpx", useInternalNodes = TRUE)

# Extract name.
name <- xpathSApply(doc = gpx, path = "//trk/name", fun = xmlValue)

# Extract type.
type <- xpathSApply(doc = gpx, path = "//trk/type", fun = xmlValue)

# Extract coords.
coords <- xpathSApply(doc = gpx, path = "//trkpt", fun = xmlAttrs)

# Extract elevation.
elevation <- xpathSApply(doc = gpx, path = "//trkpt/ele", fun = xmlValue)

# Extract time.
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
  mutate(timestamps = ymd_hms(timestamps),
         ele_r = round(ele/0.5)*0.5,   # round to nearest 0.5.
         secs  = 1:nrow(.),            # create seconds from zero.
         mins  = round(secs/60,0)) %>% # same but for minutes.
  st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) 

# Single activity elevation.
ggplot(data = gpx_sf) +
  geom_line(mapping = aes(x = secs/60, y = ele, group = 1),
            colour = "#fc4c02", linewidth = 2) +
  theme_minimal() +
  labs(y = "Elevation", x = "Minutes")

# Create a line out of the points.
gpx_line_sf <- gpx_sf %>% 
  group_by(act_name) %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING") 

# Single activity map.
ggplot(data = gpx_line_sf) +
  geom_sf(colour = "#fc4c02", linewidth = 1) +
  theme_minimal()

# Activity-level stats.
stats_df <- data.frame(
  act_name    = name,
  act_type    = type,
  total_mins  = as.numeric(max(gpx_sf$timestamps)-min(gpx_sf$timestamps)),
  total_km    = as.numeric(st_length(gpx_line_sf)/1000),
  ele_gain    = sum(diff(gpx_sf$ele)[diff(gpx_sf$ele) > 0])
) %>% 
  mutate(
    av_km_time     = total_mins/total_km
  ) %>% 
  mutate_if(is.numeric, function(x)round(x, 2))
