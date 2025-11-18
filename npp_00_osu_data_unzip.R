rm(list = ls())
# install.packages("librarian")
librarian::shelf(here, terra, R.utils)

# extract data from tar compression
tar_files <- list.files("npp_data/", pattern = "\\.tar$", full.names = TRUE)

for (f in tar_files) {
  untar(f, exdir = "npp_data/")
}

# uncompress gz_files to create hdf files
gz_files <- list.files("npp_data/", pattern = "\\.hdf\\.gz$", full.names = TRUE)

for (f in gz_files) {
  gunzip(f, remove = FALSE, overwrite = TRUE)
}


# ML converted the hdf files to geotiff in QGIS for downstream use
# they are now in npp_data/osu_npp_geotiff

