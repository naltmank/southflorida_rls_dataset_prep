rm(list = ls())
librarian::shelf(here, sf, dplyr, lwgeom)

# read metadata
site_locations <- read.csv(here::here("metadata_files",
                                      "site_transect_metadata.csv"))

sites_sf <- st_as_sf(site_locations, coords = c("longitude", "latitude"), crs = 4326)

# read and flatten the geoJSON data
sf_use_s2(FALSE)
reef_habitat <- st_read(here::here("areal_extent", "Unified Reef Map - Reef Habitat.geojson"))
reef_habitat <- st_make_valid(reef_habitat)

# reproject to be in area instead of latlon
reef_habitat <- st_transform(reef_habitat, crs = 3857)

# calculate area
reef_habitat$area_m2  <- st_area(reef_habitat)

# make sites match geoJSON projection
sites_proj <- st_transform(sites_sf, st_crs(reef_habitat))

# merge sites with reef map
sites_with_reef <- st_join(sites_proj, reef_habitat, left = FALSE)

# some sites area read in across multiple layers and habitat types - subset only the coral reef and hardbottom ones
sites_with_reef_sub <- subset(sites_with_reef, FIRST_ClassLv0 == "Coral Reef and Hardbottom") %>%
  st_drop_geometry() %>%
  select(region, site_code, site_name, habitat, area_m2)

# convert area_m2 column to numeric instead of geometry
sites_with_reef_sub$area_m2 <- as.numeric(sites_with_reef_sub$area_m2)

# take log for binning
sites_with_reef_sub$area_log <- log(sites_with_reef_sub$area_m2)

# create bins
sites_with_reef_sub$area_bin <- as.factor(cut(
  sites_with_reef_sub$area_log,
  breaks = 12,
  labels = 1:12,
  include.lowest = TRUE
))

# some sites were projected to be on land or seagrass - add those back in
final_sites <- site_locations %>%
  left_join(
    sites_with_reef_sub %>%
      st_drop_geometry() %>%
      select(site_name, area_m2, area_log, area_bin),
    by = "site_name"
  )

# Snap City fell on two polygons, one of which is definitely wrong
# filter out wrong one
final_sites <- final_sites %>%
  dplyr::filter(!(site_name == "Snap City" & area_bin == 9))

# select the relevant columns - LKH will make judgments for missing site size bins
sites_for_write <- final_sites %>%
  select(region, site_code, site_name, habitat, area_m2, area_log, area_bin)

write.csv(sites_for_write, "areal_extent/habitat_area.csv", row.names = F)
