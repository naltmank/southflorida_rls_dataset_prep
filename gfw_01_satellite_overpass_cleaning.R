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

# extract unique H3 cells needed to subset from the global datasets based on buffer region
h3_ids_needed <- unique(buffer_h3_lookup$h3_id)

# Set up input/output paths
overpass_dir <- here::here("global_fishing_watch_data", "satellite_overpass/current")
filtered_dir <- here::here("global_fishing_watch_data", "filtered_overpass/current")
# dir.create(filtered_dir, showWarnings = FALSE)

# Get list of all overpass CSV files
overpass_files <- list.files(overpass_dir, full.names = TRUE, pattern = "\\.csv$")

# Process each file individually
for (file_path in overpass_files) {
  message("Processing ", basename(file_path), " ...")
  
  # Read the full file — fread is very fast and memory-efficient
  dt <- fread(file_path)
  
  # Filter rows by your H3 IDs
  dt_filtered <- dt[h3_id %in% h3_ids_needed]
  
  # Write the filtered file to disk
  out_path <- file.path(filtered_dir, basename(file_path))
  fwrite(dt_filtered, out_path)
  
  # Clean up memory aggressively
  rm(dt, dt_filtered)
  gc()
  
  # Optional: delete the original file to save space
  # file.remove(file_path)
}

message("✅ All files processed and filtered saved in ", filtered_dir)



