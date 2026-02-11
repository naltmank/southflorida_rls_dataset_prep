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


# read temp data
temp <- terra::rast(
  here::here("copernicus_data", "copernicus_temp.nc"),
  subds = "adjusted_sea_surface_temperature"
)

# date is treated as an index of day here, starting with _1 at 01-01-2021

# generate date sequence
start_date <- as.Date("2021-01-01")
end_date   <- as.Date("2025-12-31")

date_vec <- seq.Date(start_date, end_date, by = "day")

# check that date vector is the same length as the temp vector
length(date_vec) #1826
nlyr(temp) #1826

# parse the index and merge with date vector
parsed <- tibble(
  band = names(temp),
  t_idx = seq_len(nlyr(temp)),
  date = date_vec
)

# extract temp data for buffer region within each band
ext_raw <- terra::extract(temp, buffers_ll, fun = mean, na.rm = TRUE) %>%
  select(-ID)
ext_raw$site_code <- site_locations$site_code


# pivot long and merge based on parsed df
ext_long <- ext_raw %>%
  pivot_longer(
    cols = -site_code,
    names_to = "band",
    values_to = "sst_k"
  ) %>%
  left_join(parsed, by = "band")

# convert kelvin to celsius
ext_long$sst <- ext_long$sst_k - 273.15

# JHO exported temp data daily here

#### AGGREGATE MONTHLY TEMP DATA ####
# summarise data to mean values per year at each depth band
temp_summary <- ext_long %>%
  mutate(date = date,
         year = year(date),
         month = month(date)) %>%
  group_by(site_code, year, month) %>%
  summarise(
    sst_month_mean = mean(sst, na.rm = TRUE),
    sst_month_sd = sd(sst, na.rm = TRUE),
    sst_month_min = min(sst, na.rm = TRUE),
    sst_month_max = max(sst, na.rm = TRUE),
    
    .groups = "drop"
  ) 

# some NAs fron USEC25 triggered Inf values --> mutate back to NA
temp_summary[sapply(temp_summary, is.infinite)] <- NA

# merge with site metadata
temp_monthly <- temp_summary %>%
  left_join(site_locations, by = "site_code")

# make sure NAs and NaNs are treated the same
temp_monthly <- temp_monthly %>%
  mutate(across(all_of(temp_vars), ~ ifelse(is.nan(.x), NA_real_, .x)))

##### INFER MISSING TEMP VALUES #####

# not all sites were included in merge and some sites have NAs
# infer the temp values based on the mean values from the other sites in that region
temp_vars <- c("sst_month_mean", "sst_month_sd", "sst_month_min", "sst_month_max")

# get regional means per year/month
region_means <- temp_monthly %>%
  group_by(year, month, region) %>%
  summarize(across(all_of(temp_vars), ~ mean(.x, na.rm = TRUE))) %>%
  rename_with(~ paste0(.x, "_region"), all_of(temp_vars))

# include habitat as an option within each region × year x month
habitat_means <- temp_monthly %>%
  group_by(year, month, region, habitat) %>%
  summarize(across(all_of(temp_vars), ~ mean(.x, na.rm = TRUE))) %>%
  rename_with(~ paste0(.x, "_habitat"), all_of(temp_vars))

# fill data - merge with helper df...
temp_filled <- temp_monthly %>%
  left_join(habitat_means,
            by = c("year", "month", "region", "habitat"))

# ... then replace the NAs
temp_final <- temp_filled %>%
  mutate(across(
    all_of(temp_vars),
    ~ coalesce(
      .x,
      get(paste0(cur_column(), "_habitat"))
    )
  )) %>%
  # and remove helper columns
  select(-ends_with("_habitat"))

# make sure all NAs were filled
temp_final %>%
  summarize(across(all_of(temp_vars), ~ sum(is.na(.x))))

write.csv(temp_final, here::here("copernicus_data", "temp_monthly_copernicus_20260128.csv"), row.names = F)


#### AGGREGATE ANNUAL TEMP DATA ####
# summarise data to mean values per year at each depth band
temp_annual_summary <- ext_long %>%
  mutate(date = date,
         year = year(date)) %>%
  group_by(site_code, year, ) %>%
  summarise(
    sst_annual_mean = mean(sst, na.rm = TRUE),
    sst_annual_sd = sd(sst, na.rm = TRUE),
    sst_annual_min = min(sst, na.rm = TRUE),
    sst_annual_max = max(sst, na.rm = TRUE),
    sst_annual_range = max(sst, na.rm = TRUE) - min(sst, na.rm = TRUE),
    
    .groups = "drop"
  ) 

# some NAs fron USEC25 triggered Inf values --> mutate back to NA
temp_annual_summary[sapply(temp_annual_summary, is.infinite)] <- NA

# merge with site metadata
temp_annual <- temp_annual_summary %>%
  left_join(site_locations, by = "site_code")

# annual temp vars
temp_vars <- c("sst_annual_mean", "sst_annual_sd", "sst_annual_min", "sst_annual_max", "sst_annual_range")

# make sure NAs and NaNs are treated the same
temp_annual <- temp_annual %>%
  mutate(across(all_of(temp_vars), ~ ifelse(is.nan(.x), NA_real_, .x)))


##### INFER MISSING TEMP VALUES #####

# get regional means per year/month
region_means <- temp_annual %>%
  group_by(year, region) %>%
  summarize(across(all_of(temp_vars), ~ mean(.x, na.rm = TRUE))) %>%
  rename_with(~ paste0(.x, "_region"), all_of(temp_vars))

# include habitat as an option within each region × year x month
habitat_means <- temp_annual %>%
  group_by(year, region, habitat) %>%
  summarize(across(all_of(temp_vars), ~ mean(.x, na.rm = TRUE))) %>%
  rename_with(~ paste0(.x, "_habitat"), all_of(temp_vars))

# fill data - merge with helper df...
temp_filled <- temp_annual %>%
  left_join(habitat_means,
            by = c("year", "region", "habitat"))

# ... then replace the NAs
temp_final <- temp_filled %>%
  mutate(across(
    all_of(temp_vars),
    ~ coalesce(
      .x,
      get(paste0(cur_column(), "_habitat"))
    )
  )) %>%
  # and remove helper columns
  select(-ends_with("_habitat"))

# make sure all NAs were filled
temp_final %>%
  summarize(across(all_of(temp_vars), ~ sum(is.na(.x))))

write.csv(temp_final, here::here("copernicus_data", "temp_annual_copernicus_20260128.csv"), row.names = F)
