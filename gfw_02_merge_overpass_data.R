rm(list = ls())
# install.packages("librarian")
librarian::shelf(here, sf, tidyverse, lubridate,
                 data.table, h3jsr, vroom, stringr, purrr)



# prep site metadata as before
site_locations <- read.csv(here::here("metadata_files",
                                      "site_transect_metadata.csv"))
site_locations_sf <- st_as_sf(site_locations, coords = c("longitude", "latitude"), crs = 4326)
site_locations_sf <- st_transform(site_locations_sf, 3857)

# set 5km radius
radius <- 5000
sites_buffer <- st_buffer(site_locations_sf, dist = radius) 

# get h3 cells for buffer regions around each site
buffers_h3 <- polygon_to_cells(sites_buffer, res = 7)

# label buffer cells with appropriate site code
buffer_h3_lookup <- tibble(
  site_code = site_locations_sf$site_code,
  h3_list = buffers_h3
) %>%
  unnest_longer(h3_list, values_to = "h3_id") %>%
  select(site_code, h3_id)

# read filtered overpass files
filtered_dir <- here::here("global_fishing_watch_data", "filtered_overpass/current")
filtered_files <- list.files(filtered_dir, full.names = TRUE, pattern = "\\.csv$")

# Read and combine filtered data into single df
overpass_dt <- rbindlist(lapply(filtered_files, fread), fill = TRUE)


# merge with the buffer h3_id dataframe to assign site_name
overpass_site <- left_join(overpass_dt, buffer_h3_lookup, by = "h3_id")
# many to many warning - this is expectede because of overlapping borders and cells with sites

# ensure overpass_site is being read as a data table
setDT(overpass_site)

# month column is not really a month column - it's a date column
# correct this and create new month and year column
setnames(overpass_site, "month", "date")



# force into character and trim whitespace
overpass_site[, date := as.character(date)]
overpass_site[, date := trimws(date)]

# extract year and month
overpass_site <- overpass_site %>%
  mutate(
    date = ymd(date),
    year = year(date),
    month = month(date)
  )

# summarize all overpass-related columns by site, year, and month
overpass_summary <- overpass_site[
  , lapply(.SD, sum, na.rm = TRUE),
  by = .(site_code, year, month),
  .SDcols = patterns("^overpasses")  # all columns starting with "overpasses"
]



write.csv(overpass_summary, here::here("global_fishing_watch_data", "satellite_overpass_summary_20260127.csv"),
          row.names = F)

