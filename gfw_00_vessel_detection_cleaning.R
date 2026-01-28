rm(list = ls())
# install.packages("librarian")
librarian::shelf(here, sf, tidyverse, lubridate)

# read GFW vessel detection data
vessel_detections_path <- here::here("global_fishing_watch_data", "vessel_detections/current")

vessel_detections <- list.files(
  path = vessel_detections_path,
  pattern = "\\.csv$",       # only CSV files
  full.names = TRUE
) %>%
  map_dfr(read_csv)          # read and bind all into one dataframe

# read metadata
site_locations <- read.csv(here::here("metadata_files",
                                      "site_transect_metadata.csv"))

# convert to spatial object on same coordinate system
vessel_detections_sf <- st_as_sf(vessel_detections, coords = c("lon", "lat"), crs = 4326)
site_locations_sf <- st_as_sf(site_locations, coords = c("longitude", "latitude"), crs = 4326)

# Project to a metric CRS 
vessel_detections_sf <- st_transform(vessel_detections_sf, 3857)
site_locations_sf <- st_transform(site_locations_sf, 3857)

# convert detection timestamps to date and time, then extract year and month
vessel_detections_sf <- vessel_detections_sf %>%
  mutate(
    detect_timestamp = ymd_hms(detect_timestamp, tz = "UTC"),
    year = year(detect_timestamp),
    month = month(detect_timestamp)
  )

# set 5km radius
radius <- 5000
sites_buffer <- st_buffer(site_locations_sf, dist = radius) 

# join radius and detections df - filter out detections that aren't within our radius
detections_near_sites <- st_join(vessel_detections_sf, sites_buffer, join = st_within, left = FALSE) %>%
  # filter out detections with low presence score or likely infrastucture
  filter(presence_score >= 0.5,
         likely_infrastructure != "true",
         length_m_inferred <= 40,
         speed_kn_inferred < 10)

# some duplicates in df - not unsurprising due to overlapping satellites or grid cells per gfw
# also, detect_id and geometry causes issue on write because of the comma --> remove dupes and then detect_id

# detections_near_sites_nodupes <- detections_near_sites[!duplicated(detections_near_sites$detect_id), ] 
# current method removes sites
# upon closer examination it looks like detection IDs are sometimes associated with multiple sites.
# not sure how this is possible but if we change the way we filter to focus on full row matches we retain all sites

detections_near_sites_nodupes <- detections_near_sites %>%
  dplyr::distinct(
    detect_id,
    site_code,
    geometry,
    .keep_all = TRUE
  )

# going with this option as it seems more likely to be correct - there might be an issue with unique_id

remove_cols <- c("detect_id", "geometry")

detections_near_sites_nodupes <- detections_near_sites_nodupes[,-which(names(detections_near_sites_nodupes) %in%
                                                                         remove_cols)]
present_sites <- unique(detections_near_sites_nodupes$site_code) # all sites present now

write.csv(detections_near_sites_nodupes, here::here("global_fishing_watch_data", "detections_near_sites_20260127.csv"),
          row.names = F)

