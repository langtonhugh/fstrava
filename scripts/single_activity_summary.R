# Load libraries.
library(pbapply)
library(XML)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)
library(maptiles)
library(tidyterra)

# Create list of all the gpx files that we have.
file_names <- paste0(
  "data/",
  list.files("data", pattern = glob2rx("*.gpx"))
)

# Read them all into a list.
raw_list <- pblapply(file_names, function(x){
  htmlTreeParse(file = x, useInternalNodes = TRUE)
}
)

# How many activities do we have?
length(raw_list)

# Function for extracting the relevant information.
acts_clean <- list()

for (i in seq_along(raw_list)){
  
# Extract name.
name <- xpathSApply(doc = raw_list[[i]], path = "//trk/name", fun = xmlValue)

# Extract type.
type <- xpathSApply(doc = raw_list[[i]], path = "//trk/type", fun = xmlValue)

# Extract coords.
coords <- xpathSApply(doc = raw_list[[i]], path = "//trkpt", fun = xmlAttrs)

# Extract elevation.
elevation <- xpathSApply(doc = raw_list[[i]], path = "//trkpt/ele", fun = xmlValue)

# Extract time.
time <- xpathSApply(doc = raw_list[[i]], path = "//trkpt/time", fun = xmlValue)

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
         week_lub   = week(timestamps),
         year_lub   = year(timestamps)) %>% 
  st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) 

# Insert each into the list.
acts_clean[[i]] <- gpx_sf

}

# Bind together for broad summaries, then filter for runs only.
acts_sf <- bind_rows(acts_clean, .id = "act_id") %>% 
  filter(act_type == "running")

# Convert coords to lines.
acts_lines_sf <- acts_sf %>% 
  group_by(act_id) %>% 
  summarize(do_union=FALSE) %>% 
  st_cast("LINESTRING") %>% 
  ungroup() 

# Create df of the distances.
acts_dist_df <- acts_lines_sf %>% 
  mutate(total_km = round(as.numeric(st_length(.)/1000), 2)) %>% 
  as_tibble() %>% 
  select(-geometry) 

# Activity-level stats.
stats_df <- acts_sf %>% 
  as_tibble() %>% 
  group_by(act_id) %>% 
  summarize(
  total_mins  = as.numeric(max(timestamps)-min(timestamps)),
  ele_gain    = sum(diff(ele)[diff(ele) > 0])
) %>% 
  left_join(acts_dist_df) %>%
  mutate(av_km_time = total_mins/total_km,
         act_id     = as.numeric(act_id)) %>% 
  mutate_if(is.numeric, function(x)round(x, 2))

# Single activity elevation.
acts_sf %>% 
  filter(act_id == 1) %>% 
  ggplot(data = .) +
  geom_line(mapping = aes(x = timestamps, y = ele, group = 1),
            colour = "#fc4c02", linewidth = 2) +
  theme_minimal() +
  labs(y = "Elevation", x = "Time")

# Single activity map.
act1_sf <- acts_sf %>% 
  filter(act_id == 1)

# Obtain tiles.
osm_posit <- get_tiles(act1_sf, provider = "CartoDB.Positron",
                       crop = FALSE)

# Plot.
ggplot() +
  geom_spatraster_rgb(data = osm_posit) +
  geom_sf(data = act1_sf,
          colour = "#fc4c02", linewidth = 0.1) +
  theme_void()



