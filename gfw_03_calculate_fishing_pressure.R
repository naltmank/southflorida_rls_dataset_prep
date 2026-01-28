rm(list = ls())
# install.packages("librarian")
librarian::shelf(here, sf, tidyverse, lubridate)

# read detections near sites - strange error with duplicate rownames - modify read functions

detections_near_sites <- read_csv(
  here::here("global_fishing_watch_data", "detections_near_sites_20260127.csv"),
  quote = "\""
)

# add column for satellite ID - for standardization later
detections_near_sites$satellite_id <- sub("_.*", "", detections_near_sites$scene_id)

# summarize detections per satellite per month
detections_monthly <- detections_near_sites %>%
  group_by(year, month, satellite_id, region, site_code, site_name, habitat, t1_depth, t2_depth) %>%
  summarise(
    detections = n(),
    mean_length_m = mean(length_m_inferred, na.rm = T),
    sd_length_m = sd(length_m_inferred, na.rm = T),
    .groups = "drop"
  )

# annual detections
detections_annual <- detections_near_sites %>%
  group_by(year, satellite_id, region, site_code, site_name, habitat, t1_depth, t2_depth) %>%
  summarise(
    detections = n(),
    mean_length_m = mean(length_m_inferred, na.rm = T),
    sd_length_m = sd(length_m_inferred, na.rm = T),
    .groups = "drop"
  )

# calculate the number of satellites making detections per month
sat_per_month <- detections_monthly %>%
  group_by(year, month) %>%
  summarise(
    n_satellites = n_distinct(satellite_id),
    .groups = "drop"
  )

sat_per_year <- detections_annual %>%
  group_by(year) %>%
  summarise(
    n_satellites = n_distinct(satellite_id),
    .groups = "drop"
  )

# add satellites into the monthly or annual detections df
detections_monthly <- detections_monthly %>%
  left_join(sat_per_month, by = c("year", "month"))

detections_annual <- detections_annual %>%
  left_join(sat_per_year, by = "year")

# read overpass data
overpasses <- read.csv(here::here("global_fishing_watch_data", "satellite_overpass_summary_20260127.csv"))

# sum annual
overpasses_annual <- overpasses %>%
  group_by(site_code, year) %>%
  summarise(across(
    .cols = starts_with("overpasses"),
    .fns = sum,
    .names = "{.col}_annual"
  ), .groups = "drop")

# merge overpass and detections data
detections_with_overpass_monthly <- detections_monthly %>%
  left_join(overpasses, by = c("site_code", "year", "month"))

detections_with_overpass_annual <- detections_annual %>%
  left_join(overpasses_annual, by = c("site_code", "year"))

# calculate fishing pressure
# number of boats per number of satellites and overpasses
detections_with_overpass_annual$fishing_pressure <-
  detections_with_overpass_annual$detections / 
  (detections_with_overpass_annual$n_satellites * detections_with_overpass_annual$overpasses_cloud_under_50_annual)

write.csv(detections_with_overpass_annual, here::here("global_fishing_watch_data",
                                                      "annual_detections_standardized_20260127.csv"), row.names = F)


library(lattice)
bwplot(fishing_pressure ~  as.factor(year) | region, data = detections_with_overpass_annual)

mean_fishing_pressure <- detections_with_overpass_annual %>%
  group_by(year, region, site_code, site_name, habitat, t1_depth, t2_depth) %>%
  summarise(fishing_pressure_mean = mean(fishing_pressure, na.rm = T))
  
bwplot(fishing_pressure_mean ~ as.factor(year) | region, data = mean_fishing_pressure)

(mean_fishing_pressure_plot <- 
    ggplot() + 
    geom_jitter(data= mean_fishing_pressure, aes(x = region, y = fishing_pressure_mean, colour = region)) +
    geom_boxplot(data= mean_fishing_pressure, aes( x=region, y = fishing_pressure_mean, colour = region),
                 outlier.shape = NA, fill = NA) +
    facet_wrap(~as.factor(year)) +
    scale_x_discrete( limits = c("MC", "PB", "BC", "DC", "UK", "MK", "LK", "MQ", "DT") ) +
    
#    scale_colour_manual(values = okabe_ito,
#                        limits = c("PB", "BC", "DC", "UK", "MK", "LK", "DT"))   + 

    labs(y = "Mean annual fishing pressure", x = "Region") +
    theme_bw() + 
    theme(axis.title.x = element_text(size=18), 
          axis.title.y = element_text(size=18), 
          strip.text.x = element_text(size = 25),
          panel.grid.major = element_blank(),  #remove major-grid labels
          panel.grid.minor = element_blank(),  #remove minor-grid labels
          legend.position = 'bottom', # move legend to bottom
          legend.box.background = element_rect(colour = "black")) 
)

