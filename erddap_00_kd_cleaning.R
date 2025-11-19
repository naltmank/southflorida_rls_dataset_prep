rm(list = ls())
# install.packages("librarian")
librarian::shelf(here, sf, tidyverse, lubridate)

#### READ AND PREP DATA ####
# read metadata
site_locations <- read.csv(here::here("metadata_files",
                                      "site_transect_metadata.csv"))

# read light attenuation data
# UPDATE HERE AS MORE DATA COME IN #
kdl <- read.csv(here::here("erddap_data",
                           "kd_lightattenuation.csv"))

# convert to spatial object on same coordinate system
kdl_sf <- st_as_sf(kdl, coords = c("longitude", "latitude"), crs = 4326)
site_locations_sf <- st_as_sf(site_locations, coords = c("longitude", "latitude"), crs = 4326)

# Project to a metric CRS 
kdl_sf <- st_transform(kdl_sf, 3857)
site_locations_sf <- st_transform(site_locations_sf, 3857)

# convert timestamps to date and time, then extract year and month
kdl_sf <- kdl_sf %>%
  mutate(
    time = ymd_hms(time, tz = "UTC"),
    year = year(time),
    month = month(time)
  )


#### FILTER KDL TO RELEVANT LOCATIONS ####
# set 5km radius
radius <- 5000
sites_buffer <- st_buffer(site_locations_sf, dist = radius) 

# join radius and kdl df 
kdl_near_sites <- st_join(kdl_sf, sites_buffer, join = st_within, left = FALSE) 


# aggregate to site level
kdl_annual <- kdl_near_sites %>%
  group_by(year, region, site_code, site_name, habitat, t1_depth, t2_depth) %>%
  summarise(
    turbidity_mean = mean(Kd_490_median, na.rm = T),
    .groups = "drop"
  )


write.csv(kdl_annual, here::here("erddap_data", "kdl_annual_490med.csv"), row.names = F)
