rm(list = ls())
# install.packages("librarian")
librarian::shelf(here, terra, tidyverse, lubridate, readr)

#### READ DATA ####
# read metadata
site_locations <- read.csv(here::here("metadata_files",
                                      "site_transect_metadata.csv"))
# project to spatial vector and specify crs
sites_vect <- vect(site_locations, geom = c("longitude", "latitude"), crs = "EPSG:4326")

# Reproject for buffering (meters)
sites_ae <- project(sites_vect, "EPSG:6933")

# 5 km buffer
buffers <- buffer(sites_ae, width = 5000)

# Back to lat/lon
buffers_ll <- project(buffers, "EPSG:4326")


# read kd data
kd <- terra::rast(here::here("copernicus_data", "copernicus_kd_lightattenuation.nc"))

# get band names (includes time and depth steps)
band_names <- names(kd)
n_layers <- nlyr(kd)

# split band names up into desired info
parsed <- tibble(band = band_names) %>%
  mutate(
    kd = str_extract(band, "^[^_]+"),  # everything before first "_" is the kd
    depth = str_extract(band, "(?<=depth=)[0-9\\.]+") |> as.numeric(), # depth band
    t_idx = str_extract(band, "(?<=_)\\d+") |> as.numeric() # time band (as step number)
  )

# get actual time info and append to parsed df
times <- terra::time(kd)
parsed$date <- times

# extract kd data for buffer region within each band
ext_raw <- terra::extract(kd, buffers_ll, fun = mean, na.rm = TRUE)
ext_raw$site_code <- site_locations$site_code


# pivot long and merge based on parsed df
ext_long <- ext_raw %>%
  pivot_longer(
    cols = all_of(parsed$band),
    names_to = "band",
    values_to = "value"
  ) %>%
  left_join(parsed, by = "band") %>%
  select(site_code, kd, depth, date, value)

#### AGGREGATE KD DATA ####
# summarise data to mean values per year at each depth band
annual_depth_means <- ext_long %>%
  mutate(date = date,
         year = year(date)) %>%
  group_by(site_code, kd, depth, year) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

# merge with site metadata
kd_annual <- annual_depth_means %>%
  left_join(site_locations, by = "site_code")

# get mean depth of transects
kd_annual$mean_transect_depth <- rowMeans(kd_annual[,c("t1_depth", 't2_depth')], na.rm=TRUE)

# rename the kd depth column to avoid confusion
names(kd_annual)[names(kd_annual) == "depth"] <- "kd_depth"

# subset kd based on measurement depth closest to mean_transect depth
kd_closest <- kd_annual %>%
  mutate(depth_diff = abs(kd_depth - mean_transect_depth)) %>%
  group_by(year, region, site_code, kd, site_name, habitat, t1_depth, t2_depth) %>% 
  slice_min(order_by = depth_diff, n = 1, with_ties = FALSE) %>%
  ungroup()

# pivot back wide
kd_closest_wide <- kd_closest %>%
  pivot_wider(names_from = kd,
              values_from = mean_value)

#### INFER MISSING KD VALUES ####

# not all sites were included in merge and some sites have NAs
# infer the nturient values based on the mean values from the other sites in that region
kd_vars <- "kd"

# get regional means per year
region_means <- kd_closest_wide %>%
  group_by(year, region) %>%
  summarise(kd = mean(kd, na.rm = T))

# include habitat as an option within each region × year
habitat_means <- kd_closest_wide %>%
  group_by(year, region, habitat) %>%
  summarise(kd = mean(kd, na.rm = T))


# split the kd dataset into rows that have data and the NAs rows from USEC sites with missing depth
na_depth_rows <- kd_closest_wide %>% filter(is.na(mean_transect_depth))
has_depth_rows <- kd_closest_wide %>% filter(!is.na(mean_transect_depth))


# merge the na depth rows with the regional means - adds new columns with the suffix "_reg" (e.g. fe_mean_reg)
# merge with habitat-level means as well in case those more specific data are available
dat_na_filled <- na_depth_rows %>%
  # first bring in habitat-level means
  left_join(habitat_means, 
            by = c("year", "region", "habitat"), 
            suffix = c("", "_hab")) %>%
  # then bring in regional means
  left_join(region_means, 
            by = c("year", "region"), 
            suffix = c("", "_reg"))


# replace the standard kd_mean values with the regional values by habitat
for (v in kd_vars) {
  
  hab_col <- paste0(v, "_hab")
  reg_col <- paste0(v, "_reg")
  
  # if there is habitat data, use the habitat level mean
  dat_na_filled[[v]] <- ifelse(
    !is.na(dat_na_filled[[hab_col]]), 
    dat_na_filled[[hab_col]],    
    
    # otherwise use the regional mean
    ifelse(
      !is.na(dat_na_filled[[reg_col]]),
      dat_na_filled[[reg_col]],  # else use region mean
      dat_na_filled[[v]]         # or if none of the above, leave the original value (shouldn't happen)
    )
  )
}

# drop the _reg and _hab values
dat_na_filled <- dat_na_filled %>%
  select(-ends_with("_hab"), -ends_with("_reg"))

# recombine data
kd_recombined <- bind_rows(has_depth_rows, dat_na_filled)

##### HANDLE "LAND" DATA #####
# Low spatial resolution makes some data appear on land, resulting in nas
# impute those values using the same methods as above
land_rows <- kd_recombined %>% 
  filter(if_any(all_of(kd_vars), is.na))

sea_rows <- kd_recombined %>% 
  filter(if_all(all_of(kd_vars), ~ !is.na(.x)))

# repeat workflow
land_filled <- land_rows %>%
  # first bring in habitat-level means
  left_join(habitat_means, 
            by = c("year", "region", "habitat"), 
            suffix = c("", "_hab")) %>%
  # then bring in regional means
  left_join(region_means, 
            by = c("year", "region"), 
            suffix = c("", "_reg"))

for (v in kd_vars) {
  
  hab_col <- paste0(v, "_hab")
  reg_col <- paste0(v, "_reg")
  
  # if there is habitat data, use the habitat level mean
  land_filled[[v]] <- ifelse(
    !is.na(land_filled[[hab_col]]), 
    land_filled[[hab_col]],    
    
    # otherwise use the regional mean
    ifelse(
      !is.na(land_filled[[reg_col]]),
      land_filled[[reg_col]],  # else use region mean
      land_filled[[v]]         # or if none of the above, leave the original value (shouldn't happen)
    )
  )
}

# drop the _reg and _hab values
land_filled <- land_filled %>%
  select(-ends_with("_hab"), -ends_with("_reg"))


# recombine data
kd_final <- bind_rows(land_filled, sea_rows)



write.csv(kd_final, here::here("copernicus_data", "kd_annual_copernicus.csv"), row.names = F)
