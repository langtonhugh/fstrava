# Load libraries.
library(pbapply)
library(XML)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)

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

# Distances per week.
acts_weeks_df <- acts_sf %>% 
  as_tibble() %>% 
  select(act_id, week_lub, year_lub) %>% 
  right_join(acts_dist_df) %>% 
  distinct() %>% 
  group_by(week_lub, year_lub) %>% 
  summarize(weekly_km = sum(total_km)) %>% 
  ungroup()

# Distribution of distances.
ggplot(data = stats_df) +
  geom_histogram(mapping = aes(x = total_km), bins = 16, fill = "#fc4c02") +
  theme_minimal() +
  labs(y = NULL, x = "Km")

# Distribution of time running.
ggplot(data = stats_df) +
  geom_histogram(mapping = aes(x = total_mins), bins = 20, fill = "#fc4c02") +
  theme_minimal() +
  labs(y = NULL, x = "Minutes")

# Distribution of speed.
ggplot(data = stats_df) +
  geom_histogram(mapping = aes(x = av_km_time), bins = 30, fill = "#fc4c02") +
  theme_minimal() +
  labs(y = NULL, x = "Km pace (minutes)")

# Distribution of elevation gains.
ggplot(data = stats_df) +
  geom_histogram(mapping = aes(x = ele_gain), bins = 20, fill = "#fc4c02") +
  theme_minimal() +
  labs(y = NULL, x = "Metres")

# Weekly distance summary.
acts_weeks_df %>% 
  unite(col = "week_year", week_lub, year_lub) %>%
  ggplot(data = .) +
  geom_col(mapping = aes(x = week_year, y = weekly_km, group = 1),
           fill = "#fc4c02") +
  theme_minimal() +
  labs(y = "Km", x = "Weeks") 
  







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


