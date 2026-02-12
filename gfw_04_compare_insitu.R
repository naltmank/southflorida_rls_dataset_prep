rm(list = ls())
# install.packages("librarian")
librarian::shelf(here, sf, tidyverse, lubridate, dplyr)

#### READ DATA ####
# read gfw fishing pressure data 
gfw <- read.csv(here::here("global_fishing_watch_data", "annual_detections_standardized_20260127.csv"))

gfw_sub <- subset(gfw, year == 2024 | year == 2025)
gfw_mean <- gfw_sub %>%
  group_by(year, region, site_code, site_name, habitat) %>%
  summarise(fishing_pressure_mean = mean(fishing_pressure, na.rm = T))

# read dani's human use data
aerial <- read.csv(here::here("global_fishing_watch_data", "clean_aerial_survey_data_2022_2025.csv"))

# filter to 2024/2025 to align with gfw
aerial_sub <- aerial %>%
  filter(year %in% c(2024, 2025))

# remove NAs
aerial_sub <- aerial_sub[!is.na(aerial_sub$Latitude),]


# make a shapefile
aerial_sf <- st_as_sf(aerial_sub, coords = c("Longitude", "Latitude"), crs = 4326)

# Project to a metric CRS 
aerial_sf <- st_transform(aerial_sf, 3857)

# read metadata
site_locations <- read.csv(here::here("metadata_files",
                                      "site_transect_metadata.csv"))

#### AGGREGATE AERIAL DATA ####
# convert to shapefile
site_locations_sf <- st_as_sf(site_locations, coords = c("longitude", "latitude"), crs = 4326)

# Project to a metric CRS 
site_locations_sf <- st_transform(site_locations_sf, 3857)

# set 5km radius
radius <- 5000
sites_buffer <- st_buffer(site_locations_sf, dist = radius) 


# join radius and aerial df - filter out detections that aren't within our radius
aerial_near_sites <- st_join(aerial_sf, sites_buffer, join = st_within, left = FALSE)
# observations count for multiple sites due to buffer overlapping buffers and how close the boats sometimes are


# aggregate boats - just total boats for now
aerial_agg <- aerial_near_sites %>%
  # filter out cruising vessels
  filter(Vessel.Activity != "cruising") %>%
  group_by(year, region, site_code, site_name) %>%
  summarise(vessels_total = sum(Number.of.Vessels.of.This.Type, na.rm = T))

# comparison with fishing pressure
vessels_combined <- gfw_mean %>%
  select(year, region, site_code, site_name, fishing_pressure_mean) %>%
  merge(aerial_agg, by = c("year", "region", "site_code", "site_name"))

# z-score function
z_standard <- function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

cor(z_standard(vessels_combined$fishing_pressure_mean), z_standard(vessels_combined$vessels_total))

(observation_comp_plot <- ggplot(data = vessels_combined) +
    geom_point(aes(x = fishing_pressure_mean, y = vessels_total, shape = as.factor(year), colour = region)) +
    labs(x = "GFW Detections", y = "Plane Detections") + 
    theme_classic() +
    theme(axis.title.x = element_text(size=18), 
          axis.title.y = element_text(size=18), 
          strip.text.x = element_text(size = 25),
          panel.grid.major = element_blank(),  #remove major-grid labels
          panel.grid.minor = element_blank(),  #remove minor-grid labels
          legend.position = 'bottom', # move legend to bottom
          legend.box.background = element_rect(colour = "black"))
)

mod <- lm(vessels_total ~ fishing_pressure_mean, data = vessels_combined)
performance::r2(mod)
