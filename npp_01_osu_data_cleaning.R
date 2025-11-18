rm(list = ls())
# install.packages("librarian")
librarian::shelf(here, terra, tidyverse)

#### READ DATA ####
# read site metadata
site_locations <- read.csv(here("metadata_files", "site_transect_metadata.csv"))

# Convert sites to SpatVector
sites_vect <- vect(site_locations, geom = c("longitude", "latitude"), crs = "EPSG:4326")

# set 5 km buffer in terms of coordinate degrees 
deg_radius <- 5 / 111
buffers <- buffer(sites_vect, width = deg_radius)

# note filepath for geotiff files
tif_files <- list.files("npp_data/osu_npp_geotiff", 
                        pattern = "\\.tif$", full.names = TRUE)

#### TEST CASE ####
npp_test <- rast(tif_files[1])



ext(npp_test) <- ext(-180, 180, -90, 90)
crs(npp_test) <- "EPSG:4326"

# set -9999 values as NAs (used as placeholder for missing data in dataset)
npp_test[npp_test == -9999] <- NA

# take mean npp from within buffer - NOTE: NPP cells are bigger than the buffer!
# multiple cells have the same value because they fall within the minimum resolution of the NPP data
vals <- terra::extract(npp_test, buffers, fun = mean, na.rm = TRUE)

##### END TEST #####

#### EXTRACT DATA FROM FULL FOLDER ####

extract_npp <- function(tif_path, sites_vect) {
  
  # Load raster
  r <- terra::rast(tif_path)
  
  # Fix extent and CRS
  ext(r) <- ext(-180, 180, -90, 90)
  crs(r) <- "EPSG:4326"
  
  # Set NA flag
  r[r == -9999] <- NA
  
  # Extract pixel values at each site
  vals <- terra::extract(r, sites_vect, fun = mean, na.rm = TRUE)
  
  # Remove the ID column given by terra::extract
  vals <- vals[, -1, drop = FALSE]
  names(vals) <- "npp"
  
  # Parse metadata from filename: outputvgpm.yyyyddd.tif
  fname <- basename(tif_path) # set filepath string as temp variable
  yyyyddd <- gsub("outputvgpm\\.|\\.tif", "", fname)  # extract 7 digit string yyyyddd: YYYY + DDD
  
  # break up into year and day
  year <- substr(yyyyddd, 1, 4)
  doy  <- substr(yyyyddd, 5, 7)
  
  # convert to date
  date <- as.Date(as.numeric(doy) - 1, origin = paste0(year, "-01-01"))
  month <- format(date, "%m")
  
  
  # create a tibble with the info you need
  tibble::tibble(
    site_code = sites_vect$site_code,
    year  = year,
    month = month,
#   doy   = doy,
    npp   = vals$npp,
    filename = fname
  )
}

# run function
results <- map_df(tif_files, extract_npp, sites_vect = sites_vect)

# summarize to get annual npp per site
results_ann <- results %>%
  group_by(site_code, year) %>%
  summarize(npp_annual = mean(npp, na.rm = T))

# merge with metadata
site_npp <- left_join(results_ann, site_locations, by = "site_code")

# write csv
write.csv(site_npp, here::here("npp_data", "osu_npp_annual.csv"),
          row.names = F)
