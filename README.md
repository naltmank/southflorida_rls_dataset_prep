## Intro
This repo for synthesizing and prepping external datasets that are too larged to be housed by github. All code relies on "sourceName_data" folders to run properly with raw data downloaded from the respective external data sites (e.g. GFW, Copernicus). This folder can be obtained directly from NAK and should interface with all scripts without issue if you are working within an .Rproj cloned from the repo. The main external datasets are:

* Global Fishing Watch vessel detections
* Copernicus Marine Services remote sensing data - mostly from the Global Ocean Biogeochemistry Analysis and Forecast datasets and the Ocean Color datasets

There are other data sources that were used in preliminary scripts (e.g. OSU/NASA NPP data, ERDDAP data) but these were typically not updated for both years of field collection (e.g. OSU/NASA stopped updating in 2024) so were tabled for this project.

## Orientation
Each script is prefaced by the data source it is using (e.g. copernicus_##) and the number indicates the order in which the script is meant to be run. Scripts create outputs that are written into their respective data folders (e.g. Copernicus data are written into the "copernicus_data" folder) and then imported into the integrated-data/predictor_data_presynth folder in main analysis repo (github.com/MarineGEO/South-Florida-RLS). This subfolder in the main analysis repo is then accessed in the noma_01d_synthesize_predictors.R script. Key scripts are broken up as follows:

### Copernicus data
Copernicus marine services has several global remote sensing datasets that provide valuable information on ovean variables that were not or could not be measured *in situ* at our sites. The tradeoff with these data is that the spatial resolution is relatively course (1/2 degree resolution). This means that 1) the Florida coastline is projected further out into sea in some cases, causing some sites to appear on land, and 2) some sites appear in the same grid cell, causing clustering in some values. For the former scenario, rather than drop these datapoints (a relatively common occurence that would significantly reduce power), data were imputed by taking the average of the relevant habitat types for a given region (e.g. a patch reef in the Middle Keys that was projected to be on land would have its NA value replaced by the mean value of all other patch reefs in the Middle Keys).

Each script processing Copernicus data can be run independently and is not dependent on any prior scripts.

### Global Fishing Watch data
Global Fishing Watch leverages Sentinel satellite imagery to detect possible fishing vessels at a global scale. We leveraged these data to estimate relative fishing pressure at each of our sites by:

* Summing the total number of vessel detections within a 5km radius around each site per year
* Filtering those data to only include vessels < 40m long travelling < 10 knts and with a > 0.5 confidence that they were real vessels
* Standardizing those data by the number of satellites that passed over a given satellite cell as well as the number of overpasses those satellites made

The sequence of scripts is as follows:
gfw_00_vessel_detection_cleaning.R - Cleans the main vessel detection datasets, aggregating to the number of detections per year within our filters (specified above). The main summary output of this is "detections_near_sites_YYYYMMDD.csv"

gfw_01_satellite_overpass_cleaning.R - Cleans the satellite overpass data to output the number of overpasses only for our relevant sites. This outputs annual overpass data in a new subfolder called "filtered_overpass/current"

gfw_02_merge_overpass_data.R - reads and combines the data from "filtered_overpass/current" with the relevant metadata, creating a single overpass dataset called "satellite_overpass_summary_YYYYMMDD.csv"

gfw_03_calculate_fishing_pressure.R - Calculates our novel metric of relative fishing pressure by standardizing the number of vessel detections by the number of satellites observing a given cell and the total number of overpasses that occured within a given year that had < 50% cloud cover. This creates a single output file "annual_detections_standardized_20260127.csv" **that serves as the main dataset in our analyses involving fishing pressure.**

## Notes
None of these scripts include any formal analyses. While the repository is public and can be run by downloading from the relevant sources (both Copernicus and GFW data are open access), it is strongly recommended to reach out to NAK to make sure that subfolders are formatted correctly prior to running any scripts.
