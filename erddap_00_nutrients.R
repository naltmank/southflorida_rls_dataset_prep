rm(list = ls())
# install.packages("librarian")
librarian::shelf(here, sf, tidyverse, lubridate)

#### READ AND PREP DATA ####
# read metadata
site_locations <- read.csv(here::here("metadata_files",
                                      "site_transect_metadata.csv"))

# read light attenuation data
# UPDATE HERE AS MORE DATA COME IN #
nutrients <- read.csv(here::here("erddap_data",
                           "nutrients_2021sd.csv"))

# convert to spatial object on same coordinate system
nutrients_sf <- st_as_sf(nutrients, coords = c("longitude", "latitude"), crs = 4326)
site_locations_sf <- st_as_sf(site_locations, coords = c("longitude", "latitude"), crs = 4326)

# Project to a metric CRS 
nutrients_sf <- st_transform(nutrients_sf, 3857)
site_locations_sf <- st_transform(site_locations_sf, 3857)

# convert timestamps to date and time, then extract year and month
nutrients_sf <- nutrients_sf %>%
  mutate(
    time = ymd_hms(time, tz = "UTC"),
    year = year(time),
    month = month(time)
  )


#### FILTER NUTRIENTS TO RELEVANT LOCATIONS ####
# nutrients data has lower sampling resolution - increase radius to 10k
radius <- 15000
sites_buffer <- st_buffer(site_locations_sf, dist = radius) 

# join radius and nutrients df 
nutrients_near_sites <- st_join(nutrients_sf, sites_buffer, join = st_within, left = FALSE) 


# aggregate to site level - maintaining depth here for merge at specific sites later
nutrients_annual <- nutrients_near_sites %>%
  group_by(year, depth, region, site_code, site_name, habitat, t1_depth, t2_depth) %>%
  summarise(
    fe_mean = mean(fe, na.rm = T),
    no3_mean = mean(no3, na.rm = T),
    po4_mean = mean(po4, na.rm = T),
    si_mean = mean(si, na.rm = T),
    .groups = "drop"
  ) %>%
  st_drop_geometry() # remove geometry 

# rename the nutrient depth column to avoid confusion
names(nutrients_annual)[names(nutrients_annual) == "depth"] <- "nutrient_depth"

# get mean depth of transects
nutrients_annual$mean_transect_depth <- rowMeans(nutrients_annual[,c("t1_depth", 't2_depth')], na.rm=TRUE)

# subset nutrients based on measurement depth closest to mean_transect depth
nutrients_closest <- nutrients_annual %>%
  mutate(depth_diff = abs(nutrient_depth - mean_transect_depth)) %>%
  group_by(year, region, site_code, site_name, habitat, t1_depth, t2_depth) %>% 
  slice_min(order_by = depth_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

#### INFER MISSING NUTRIENT VALUES ####
# not all sites were included in merge and some sites have NAs
# infer the nturient values based on the mean values from the other sites in that region

# Identify nutrient columns
nutrient_vars <- c("fe_mean", "no3_mean", "po4_mean", "si_mean")

# get regional means per year
region_means <- nutrients_closest %>%
  group_by(year, region) %>%
  summarize(across(all_of(nutrient_vars), ~ mean(.x, na.rm = TRUE)))

# split the nutrients dataset into rows that have data and the NAs rows from USEC sites with missing depth
na_depth_rows <- nutrients_closest %>% filter(is.na(mean_transect_depth))
has_depth_rows <- nutrients_closest %>% filter(!is.na(mean_transect_depth))


# merge the na depth rows with the regional means - adds new columns with the suffix "_reg" (e.g. fe_mean_reg)
dat_na_filled <- na_depth_rows %>% 
  left_join(region_means, by = "year", suffix = c("", "_reg"))

# replace the standard nutrient_mean values with the regional values
for (v in nutrient_vars) {
  dat_na_filled[[v]] <- dat_na_filled[[paste0(v, "_reg")]]
}

# drop the _reg values
dat_na_filled <- dat_na_filled %>% 
  select(-ends_with("_reg"))


# impute values for sites that were outside of nutrient sampling resolution

# identify missing sites
missing_sites <- site_locations %>% 
  filter(!site_code %in% nutrients_closest$site_code)

# identify regions and years of sampling that we have
year_region_grid <- nutrients_closest %>% 
  distinct(year, region)

# expand missing sites so that there is the possibility of one nutrient value per year
missing_expanded <- missing_sites %>%
  inner_join(year_region_grid, by = "region")

# merge with nutrient values based on nearest year
missing_filled <- missing_expanded %>%
  left_join(region_means, by = c("year", "region"))

# add in nutrient_depth and mean_transect depth
# set nutrient_depth to NA because it's based on variable depths
missing_filled <- missing_filled %>%
  mutate(
    nutrient_depth = NA_real_,
    mean_transect_depth = rowMeans(select(., t1_depth, t2_depth), na.rm = TRUE),
    depth_diff = NA_real_   # or compute if needed later
  )

#### RECOMBINE DATA ####
# combine the complete data with the NA depth data
dat_clean <- bind_rows(has_depth_rows, dat_na_filled)

# combine that data with the fully imputed data
nutrients_final <- bind_rows(dat_clean, missing_filled)



write.csv(nutrients_final, here::here("erddap_data", "nutrients_annual_erddap.csv"), row.names = F)
