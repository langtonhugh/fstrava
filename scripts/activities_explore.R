# Load libraries.
library(pbapply)
library(XML)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)

# Settings.
theme_set(theme_minimal())

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

# Ping-level data for every activity. 
pings_df <- acts_sf %>% 
  as_tibble() %>% 
  group_by(act_id) %>% 
  mutate(
  act_time   = max(timestamps)-min(timestamps),
  act_mins   = as.numeric(act_time, units = "mins"),
  ele_gain   = sum(diff(ele)[diff(ele) > 0])
) %>% 
  ungroup() %>% 
  left_join(acts_dist_df) %>%
  mutate(av_km_time = act_mins/total_km,
         act_id     = as.numeric(act_id))

# Summary table example.
sum_table_df <- pings_df %>% 
  mutate(av_km_time = round(av_km_time, 2),
         act_mins   = round(act_mins, 2),
         ele_gain   = round(ele_gain, 0),
         act_date = format(date(timestamps), "%d.%m.%y")) %>% 
  select(act_id, act_date, act_name, act_mins, total_km, ele_gain, av_km_time) %>% 
  distinct(act_id, .keep_all = TRUE) %>% 
  arrange(act_id) 

# Visuals to go alongside the table.
# Handling.
sum_visuals_df <- sum_table_df %>% 
  select(-act_date, -act_name) %>% 
  rename(`Time (mins)`   = act_mins,
         `Distance (km)` = total_km,
         `Elevation gain (metres)` = ele_gain,
         `Km pace (mins)`          = av_km_time) %>% 
  pivot_longer(cols = -act_id,
               names_to = "measure",
               values_to = "value")

# Histograms.
ggplot(data = sum_visuals_df) +
  geom_histogram(mapping = aes(x = value), bins = 20, fill = "#fc4c02") +
  facet_wrap(~measure, scales = "free", ncol = 4) +
  labs(y = NULL, x = NULL) +
  theme(
    axis.text.y = element_blank()
  )

# # Scatter plot of individual runs.
# ggplot(data = sum_visuals_df) +
#   geom_jitter(mapping = aes(x = value, y = 0),
#                colour = "#fc4c02", alpha = 0.5) +
#   facet_wrap(~measure, scales = "free", nrow = 4) +
#   labs(y = NULL, x = NULL) +
#   theme(
#     axis.text.y = element_blank(),
#     panel.grid.major.y = element_blank()
#   ) 

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


