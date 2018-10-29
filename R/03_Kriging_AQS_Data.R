#' -----------------------------------------------------------------------------
#' Date created: October 25, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: This script calculates daily means at each monitor
#' Also includes code to calculate MDA8 for ozone data
#' -----------------------------------------------------------------------------

library(sf)
library(sp)
library(gstat)
library(automap)
library(tidyverse)

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' -----------------------------------------------------------------------------
#' Read in pollutant data and summarize to daily means
#' Source: AQS Data Mart (Script: 01_AQS_Data_Scrape.R)
#' -----------------------------------------------------------------------------

#' For which years and which state do we want data?
years <- c(2010:2011)
state <- "08" #Colorado
time_zone <- "America/Denver"

#' https://aqs.epa.gov/aqsweb/codes/data/ParametersByDesc.csv
#' Criteria pollutants and carbon parameters

pol <- c("88101", "44201") # PM2.5 and O3
aqs_summ_name <- paste0("AQS_Daily_Mean_Summary_", state, "_", 
                        years[1], "_to_", years[length(years)], ".csv")

aqs_o3_summ_name <- paste0("AQS_Ozone_MDA8_Summary_", state, "_", 
                           years[1], "_to_", years[length(years)], ".csv")

pts_name <- "Grid_250_m_AEA.csv" #' file with unmeasured locations for kriging

daily <- read_csv(here::here("Data", aqs_summ_name)) %>% 
  filter(POC == 1) %>% 
  mutate(monitor_id = as.numeric(monitor_id))

#' POC is the Parameter Operating Code
#' Here I've filtered to just look at POC == 1, which is the original instrument
#' used to measure data (higher POCs are colocated instruments)
#' Info on POCs is in the AQS data dictionary (4.210 Page 309)
#' https://www.epa.gov/sites/production/files/2017-03/documents/aqs_data_dictionary.pdf

#' -----------------------------------------------------------------------------
#' Set up objects for kriging
#' #' Note: gstat (for kriging) requires sp objects (not sf)
#' -----------------------------------------------------------------------------

#' Points for krigings
#' gstat requires sp objects, not sf objects

krige_pts <- st_read(here::here("Data", pts_name),
                     stringsAsFactors = F, wkt = "WKT",
                     crs = albers) %>% 
  st_centroid() %>% 
  select(-WKT) %>% 
  mutate_if(is.character, as.numeric)
head(krige_pts)

#' Converting from sf to sp 
krige_pts_sp <- as(krige_pts, "Spatial")
plot(krige_pts_sp)

#' Monitor locations
monitor_pts <- st_read(here::here("Data", aqs_mon_name),
                       stringsAsFactors = F, wkt = "WKT",
                       crs = ll_wgs84) %>% 
  st_transform(crs = albers) %>% 
  select(-WKT) %>% 
  mutate_if(is.character, as.numeric)
head(monitor_pts)

#' -----------------------------------------------------------------------------
#' Ordinary kriging

show.vgms()
#' List of models to try to fit
all_models <- c("Exp", "Sph", "Gau", "Cir", "Lin", "Log")

#' cutoff distance is 40 km
c_dist = 40000

#' list of dates, pollutants to loop through
pols <- unique(daily$Parameter.Code)

krige_data <- data.frame()
cv_data <- data.frame()
cv_diagnostics <- data.frame()

for (i in 1:length(pols)) {
  df1 <- filter(daily, Parameter.Code == pols[i])
  dates <- unique(df1$Date.Local)
  
  for (j in 1:length(dates)) {
    df2 <- filter(df1, Date.Local == dates[j])
    
    monitors <- filter(monitor_pts, Parameter.Code == pols[i]) %>% 
      inner_join(df2, by="monitor_id") %>% 
      as("Spatial")
    
    #' Kriging using gstat
    #' First, fit the empirical variogram
    vgm <- variogram(mean ~ 1, monitors, cutoff = c_dist)
    plot(vgm)
    
    #' Second, fit the model
    vgm_fit <- fit.variogram(vgm, model=vgm(all_models), 
                             fit.kappa = seq(.3,5,.01))
    model <- as.character(vgm_fit$model)[nrow(vgm_fit)]
    #plot(vgm, vgm_fit)
  }
}




  

  
  #' Third, krige
  ok_result <- krige(biweekly_average ~ 1, pm_week, ct_sp, vgm_fit,
                     maxdist = c_dist)

  #' Fourth, leave-one out cross validation
  cv_result <- krige.cv(biweekly_average ~ 1, pm_week, vgm_fit)
  summary(cv_result)
  
  #' me_mean = mean error divided by mean observed
  #' MSNE = mean square normalized error (mean of squared z scores)
  #' RMSE = root mean squared error (same units as pollutant)
  #' cor_obs_pred = correlation between observed and predicted, should be 1
  #' cor_pred_res = correlation between predicted and residual, should be 0
  cv_compare <- compare.cv(list(krige.cv_output = cv_result))

  #' Data frame of results
  temp <- data.frame(GEOID = ct_sp@data$GEOID,
                     week_ending = week_end,
                     biweekly_average_pred = ok_result$var1.pred,
                     biweekly_average_var = ok_result$var1.var)
  pm_ct_data <- rbind(pm_ct_data, temp)

  #' Data frame of cross-validation results
  cv_result <- as.data.frame(cv_result)
  cv_result$week_ending <- week_end
  pm_cv_results <- rbind(pm_cv_results, cv_result)
  
  temp2 <- data.frame(week_ending = week_end,
                      monitor_n = nrow(pm_week),
                      monitor_min = min(pm_week$biweekly_average, na.rm=T),
                      monitor_max = max(pm_week$biweekly_average, na.rm=T),
                      monitor_mean = mean(pm_week$biweekly_average, na.rm=T),
                      model = model,
                      modeled_min = min(ok_result$var1.pred, na.rm=T),
                      modeled_max = max(ok_result$var1.pred, na.rm=T),
                      modeled_mean = mean(ok_result$var1.pred, na.rm=T),
                      mean_error = unname(unlist(cv_compare[1,1])),
                      me_mean = unname(unlist(cv_compare[2,1])),
                      msne = unname(unlist(cv_compare[5,1])),
                      rmse = unname(unlist(cv_compare[8,1])),
                      cor_obs_pred = unname(unlist(cv_compare[6,1])),
                      cor_pred_res = unname(unlist(cv_compare[7,1])))
  pm_diagnostics <- rbind(pm_diagnostics, temp2)
  
  rm(pm_week, vgm, vgm_fit, model, ok_result, cv_result, cv_compare,
     temp, temp2, week_end)
}

pm_ct_data <- pm_ct_data %>%
  rename(biweekly_average_pm_pred = biweekly_average_pred,
         biweekly_average_pm_var = biweekly_average_var) %>%
  mutate_if(is.factor, as.character)

pm_ct_data_full <- full_join(ap_dates, pm_ct_data, by="week_ending")

save(pm_ct_data, pm_ct_data_full, pm_cv_results, pm_diagnostics,
     file = "./Data/Air Quality/pm kriging results.RData")
write_xlsx(pm_diagnostics,
           path="./Data/Air Quality/PM Kriging Diagnostics.xlsx")

#' ------------------------------------------------------------------------------
#' Biweekly mean daily 8-hour max O3
#' ------------------------------------------------------------------------------

o3_ct_data <- data.frame()
o3_cv_results <- data.frame()
o3_diagnostics <- data.frame()

for (i in 1:length(week_list)) {
  print(paste("Week", i, "of", length(week_list)))
  
  #' use second week ending date to identify concentrations
  week_start <- week_list[i] - 7
  week_end <- week_list[i]
  
  #' Weekly concentration at monitors
  #' Drop rows with NA values
  o3_week <- filter(o3_monitors, week_ending %in% c(week_start, week_end)) %>%
    filter(!is.na(weekly_average))
  
  if(nrow(o3_week) == 0) {next}
  
  #' Biweekly average for each monitor
  o3_week <- o3_week %>%
    select(-week_ending, -year) %>%
    group_by(monitor_id) %>%
    summarize(pollutant = "o3",
              biweekly_average = mean(weekly_average),
              week_ending = week_end)
  
  #' Converting the monitor points from sf to sp 
  o3_week <- as(o3_week, "Spatial")
  
  #' Kriging using gstat
  #' First, fit the empirical variogram
  vgm <- variogram(biweekly_average ~ 1, o3_week, cutoff = c_dist)
  #plot(vgm)
  
  #' Second, fit the model
  vgm_fit <- fit.variogram(vgm, model=vgm(all_models), fit.kappa = seq(.3,5,.01))
  model <- as.character(vgm_fit$model)[nrow(vgm_fit)]
  #plot(vgm, vgm_fit)
  
  #' Third, krige
  ok_result <- krige(biweekly_average ~ 1, o3_week, ct_sp, vgm_fit,
                     maxdist = c_dist)
  
  #' Fourth, leave-one out cross validation
  cv_result <- krige.cv(biweekly_average ~ 1, o3_week, vgm_fit)
  summary(cv_result)
  
  #' me_mean = mean error divided by mean observed
  #' MSNE = mean square normalized error (mean of squared z scores)
  #' RMSE = root mean squared error (same units as pollutant)
  #' cor_obs_pred = correlation between observed and predicted, should be 1
  #' cor_pred_res = correlation between predicted and residual, should be 0
  cv_compare <- compare.cv(list(krige.cv_output = cv_result))
  
  #' Data frame of results
  temp <- data.frame(GEOID = ct_sp@data$GEOID,
                     week_ending = week_end,
                     biweekly_average_pred = ok_result$var1.pred,
                     biweekly_average_var = ok_result$var1.var)
  o3_ct_data <- rbind(o3_ct_data, temp)
  
  #' Data frame of cross-validation results
  cv_result <- as.data.frame(cv_result)
  cv_result$week_ending <- week_end
  o3_cv_results <- rbind(o3_cv_results, cv_result)
  
  temp2 <- data.frame(week_ending = week_end,
                      monitor_n = nrow(o3_week),
                      monitor_min = min(o3_week$biweekly_average, na.rm=T),
                      monitor_max = max(o3_week$biweekly_average, na.rm=T),
                      monitor_mean = mean(o3_week$biweekly_average, na.rm=T),
                      model = model,
                      modeled_min = min(ok_result$var1.pred, na.rm=T),
                      modeled_max = max(ok_result$var1.pred, na.rm=T),
                      modeled_mean = mean(ok_result$var1.pred, na.rm=T),
                      mean_error = unname(unlist(cv_compare[1,1])),
                      me_mean = unname(unlist(cv_compare[2,1])),
                      msne = unname(unlist(cv_compare[5,1])),
                      rmse = unname(unlist(cv_compare[8,1])),
                      cor_obs_pred = unname(unlist(cv_compare[6,1])),
                      cor_pred_res = unname(unlist(cv_compare[7,1])))
  o3_diagnostics <- rbind(o3_diagnostics, temp2)
  
  rm(o3_week, vgm, vgm_fit, model, ok_result, cv_result, cv_compare,
     temp, temp2, week_end)
}

o3_ct_data <- o3_ct_data %>%
  rename(biweekly_average_o3_pred = biweekly_average_pred,
         biweekly_average_o3_var = biweekly_average_var) %>%
  mutate_if(is.factor, as.character)

o3_ct_data_full <- full_join(ap_dates, o3_ct_data, by="week_ending")

save(o3_ct_data, o3_ct_data_full, o3_cv_results, o3_diagnostics,
     file = "./Data/Air Quality/o3 kriging results.RData")
write_xlsx(o3_diagnostics,
           path="./Data/Air Quality/O3 Kriging Diagnostics.xlsx")


#' ------------------------------------------------------------------------------
#' Combined both sets of data into a single data frame
#' ------------------------------------------------------------------------------

load("./Data/Spatial Data/dm_tracts.RData")
ct <- select(dm_tracts, GEOID) %>%
  arrange(GEOID)

ct_df <- st_set_geometry(ct, NULL)

start <- ceiling_date(as.Date("2009-01-01"), unit="week")  + 7
ap_dates <- data.frame(week_ending = seq.Date(from = start, 
                                              to = as.Date("2017-12-31"),
                                              by="2 weeks"))
week_list <- sort(unique(ap_dates$week_ending))

#' create data frame with all of the dates needed
temp <- data.frame()
for (i in 1:nrow(ap_dates)) {
  temp1 <- data.frame(week_ending = rep(ap_dates[i,1], times = nrow(ct)))
  temp1 <- bind_cols(ct_df, temp1)
  temp <- bind_rows(temp, temp1)
  rm(temp1)
}

ct_air_pollution <- st_as_sf(left_join(temp, ct, by="GEOID"))
head(ct_air_pollution)

rm(dm_tracts)

load("./Data/Air Quality/pm kriging results.RData")
load("./Data/Air Quality/o3 kriging results.RData")

ct_air_pollution <- left_join(ct_air_pollution, pm_ct_data_full,
                              by=c("GEOID", "week_ending")) %>%
  full_join(o3_ct_data_full, by=c("GEOID", "week_ending")) %>%
  arrange(week_ending, GEOID)

save(ct_air_pollution, file="./Data/CEI Data/CT_Air Pollution.RData")

# rm(o3, o3_average, o3_ct_data, o3_cv_results, o3_diagnostics,
#    pm, pm_average, pm_ct_data, pm_cv_results, pm_diagnostics,
#    monitors)

#' ------------------------------------------------------------------------------
#' Mapping weekly concentrations
#' ------------------------------------------------------------------------------

load("./Data/CEI Data/CT_Air Pollution.RData")
week_list <- unique(ct_air_pollution$week_ending)

for(i in 1:length(week_list)) {
  #' PM maps (when available)
  ct_week <- filter(ct_air_pollution, week_ending == week_list[i])
  ct_pm <- filter(ct_week, !(is.na(biweekly_average_pm_pred)))
  
  #' skip the week if pm isn't measured
  if(nrow(ct_pm) == 0) {next}
  
  ggplot() +
    ggtitle(paste("Predicted PM\u2082.\u2085 for biweekly period ending", week_list[i])) +
    geom_sf(data = ct_pm, aes(fill = biweekly_average_pm_pred), col=NA) +
    scale_fill_viridis(name = "\u03BCg/m\u00B3") +
    xlab("") + ylab("") +
    theme(legend.position = "right") +
    simple_theme
  
  ggsave(filename = paste("./Figures/CEI Figures/Biweekly Air Pollution Maps/PM_", week_list[i], ".jpeg", sep=""), 
         device = "jpeg", dpi=300)
}

for(i in 1:length(week_list)) {
  #' Ozone maps (when available)
  ct_week <- filter(ct_air_pollution, week_ending == week_list[i])
  ct_o3 <- filter(ct_week, !(is.na(biweekly_average_o3_pred)))
  
  #' skip the week if ozone isn't measured
  if(nrow(ct_o3) == 0) {next}
  
  ggplot() +
    ggtitle(paste("Predicted O\u2083 for biweekly period ending", week_list[i])) +
    geom_sf(data = ct_o3, aes(fill = biweekly_average_o3_pred), col=NA) +
    scale_fill_viridis(name = "ppb") +
    xlab("") + ylab("") +
    theme(legend.position = "right") +
    simple_theme
  
  ggsave(filename = paste("./Figures/CEI Figures/Biweekly Air Pollution Maps/O3_", week_list[i], ".jpeg", sep=""),
         device = "jpeg", dpi=300)
}


