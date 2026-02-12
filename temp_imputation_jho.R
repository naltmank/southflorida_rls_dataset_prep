
#### TEMP IMPUTATION SCRIPT ####
# JHO
# FEB 11 2026



#### REQUIRE DEPENDENCIES ####
require(tidyverse)
require(lme4)
require(lmerTest)


# use available remote sensing and site 
## variables to predict bottom temp
## where missing. 

#### LOAD DATA ####
## will need to make sure we have
## the most recent temp logger data

DATE_CUTOFF <- ymd('2024/01/01')

copernicus_temp <- read_csv('./copernicus_data/temp_daily_copernicus_20260211.csv') %>%
    filter(date > DATE_CUTOFF)
site_metadata <- read_csv('./metadata_files/site_metadata.csv') %>%
    rename(site_code = code)
logger_temp_1 <- read_csv('./CREMP_temp_data/florida_keys_master_2025.csv') %>%
    filter(date > DATE_CUTOFF) %>%
    select(-temp_F)
logger_temp_2 <- read_csv('./CREMP_temp_data/dry_tortugas_master_2025.csv') %>%
    filter(date > DATE_CUTOFF) %>%
    select(-temp_F)
logger_temp_3_init <- read_csv('./CREMP_temp_data/SECREMP_Temperature_2024.csv') 
logger_temp_3 <- logger_temp_3_init %>%
    mutate(date = ymd(paste(as.character(Year), 
                            as.character(Month), 
                            as.character(Day), 
                            sep = '/'))) %>%
    select(-c(Year, Month, Day, SiteCode, TempF)) %>%
    filter(date > ymd('2024/01/01')) %>%
    rename(observation_id = OID_,
           site_id = StationID,
           site_name = SiteName,
           temp_C = TempC)

logger_temp_1_daily <- logger_temp_1 %>%
    group_by(site_name, date) %>%
    mutate(temp_C = mean(temp_C, na.rm = T)) %>%
    ungroup() %>%
    select(-c(time, observation_id)) %>%
    distinct()

logger_temp_2_daily <- logger_temp_2 %>%
    group_by(site_name, date) %>%
    mutate(temp_C = mean(temp_C, na.rm = T)) %>%
    ungroup() %>%
    select(-c(time, observation_id)) %>%
    distinct()

logger_temp_3_daily <- logger_temp_3 %>%
    group_by(site_name, date) %>%
    mutate(temp_C = mean(temp_C, na.rm = T)) %>%
    ungroup() %>%
    select(-c(Time, observation_id)) %>%
    distinct()


logger_temp <- bind_rows(logger_temp_1_daily,
                         logger_temp_2_daily,
                         logger_temp_3_daily) 
    
temp_prediction_data <-  logger_temp %>% 
    full_join(site_metadata, by = join_by(site_name)) %>%
    full_join(copernicus_temp %>%
                  left_join(site_metadata, by = join_by(site_code))) %>%
    rename(logger_temp_C = temp_C) %>%
    select(date, 
           site_name, 
           site_code, 
           region,
           logger_temp_C, 
           sst, 
           depth_m, 
           distance_to_coast_km) %>%
    mutate(month = month(date),
           sqrt_distance_km = sqrt(distance_to_coast_km)) %>%
    filter(month %in% c(5:12))
    


#### MODEL BOTTOM TEMP AS A FUNCTION OF PREDICTORS ####
# Response: Bottom logger temp from CREMP
## prune extreme values
# Predictors: (use noams metadata file)
## - SST (daily)
## - Depth
## - Latitude?
## - Distance from shore (site metadata)
## - ...

# this is a predictive model, so best prediction is all that matters
# start using lm
# upgrade to random forest if desired

# quick plots to visualize
ggplot(temp_prediction_data, aes(x = sst, y = logger_temp_C)) +
    geom_point(alpha = 0.3) +
    geom_smooth(color = 'blue') +
    geom_smooth(color = 'red', method = 'lm') +
    theme_bw()

temp_pred_model <- lm(logger_temp_C ~ sst * distance_to_coast_km + depth_m, 
   data = temp_prediction_data) #%>%
       #filter(!is.na(logger_temp_C)))

summary(temp_pred_model)

# model performance metrics
#performance::check_model(temp_pred_model)

# cross validation
#cv::cv(temp_pred_model, k = 10)

#### PREDICT (IMPUTE) MISSING VALUES ####

temp_imputation <- temp_prediction_data %>%
    mutate(predicted_temp_C = predict(temp_pred_model, 
                                    newdata = temp_prediction_data),
           prediction_error = predicted_temp_C - logger_temp_C,
           abs_prediction_error = abs(prediction_error),
           sst_error = sst - logger_temp_C,
           imputed_yn = if_else(!is.na(logger_temp_C), 
                                    'n', 
                                    'y'),
           imputed_temp_C = if_else(!is.na(logger_temp_C), 
                                    logger_temp_C, 
                                    predicted_temp_C))

# random forest option
temp_imputation_rf <- temp_prediction_data %>%
    select(-c(site_name, site_code, region)) %>%
    mutate(date = as.numeric(date),
           month = as.integer(month)) %>%
    as.data.frame() %>% 
    missForest::missForest(verbose = T, variablewise = T)

colnames(temp_imputation_rf$ximp)
temp_imputation_rf$OOB

temp_imputation_rf_df <- temp_prediction_data
temp_imputation_rf_df$predicted_temp_C <- temp_imputation_rf$ximp$logger_temp_C

temp_imputation_rf_df <- temp_imputation_rf_df %>%
    mutate(imputed_yn = if_else(!is.na(logger_temp_C), 
                                'n', 
                                'y'),
           imputed_temp_C = if_else(!is.na(logger_temp_C), 
                                    logger_temp_C, 
                                    predicted_temp_C))


#### PLOT TO VERIFY QUALITY OF PREDICTIONS #### 

# check
# view(filter(temp_imputation, imputed_yn == 'y'))

# # how does residual error change over range of values?
# ggplot(temp_imputation, aes(x = logger_temp_C, 
#                             y = prediction_error, 
#                             color = region)) +
#     geom_smooth(method = 'lm') +
#     geom_point(alpha = 0.3) +
#     theme_bw()

# # how does just regular sst error compare? # basically the same
# ggplot(temp_imputation, aes(x = logger_temp_C, 
#                             y = sst_error, 
#                             color = region)) +
#     geom_smooth(method = 'lm') +
#     geom_point(alpha = 0.3) +
#     theme_bw()
# 
# # compare sst error to prediction error
# ggplot(temp_imputation, aes(x = sst_error, 
#                             y = prediction_error, 
#                             color = region)) +
#     geom_smooth(method = 'lm') +
#     geom_point(alpha = 0.3) +
#     theme_bw()
# 
# # is prediction better than sst and by how much
# hist(temp_imputation$prediction_error - temp_imputation$sst_error)




# RANDOM FOREST 


ggplot(temp_imputation_rf_df, 
       aes(x = sst, y = predicted_temp_C, color = imputed_yn)) +
    geom_point(alpha = 0.5) +
    theme_bw()





#### GENERATE SUMMARY VALUES ####
# 

