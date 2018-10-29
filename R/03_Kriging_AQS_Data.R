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
years <- c(2010:2010)
state <- "08" #Colorado
time_zone <- "America/Denver"

#' https://aqs.epa.gov/aqsweb/codes/data/ParametersByDesc.csv
#' Criteria pollutants and carbon parameters

pol <- c("88101", "44201") # PM2.5 and O3

#' Input file names
aqs_summ_name <- paste0("AQS_Daily_Mean_Summary_", state, "_", 
                        years[1], "_to_", years[length(years)], ".csv")

aqs_o3_summ_name <- paste0("AQS_Ozone_MDA8_Summary_", state, "_", 
                           years[1], "_to_", years[length(years)], ".csv")
mon_file_name <- paste0("AQS_Monitors_", state, "_", 
                        years[1], "_to_", years[length(years)], ".csv")

#' Output file names
aqs_krige_name <- paste0("AQS_Kriged_Results_", state, "_",
                         years[1], "_to_", years[length(years)], ".csv")
#' Output file names
aqs_cv_name <- paste0("AQS_Kriged_CV_Results_", state, "_",
                         years[1], "_to_", years[length(years)], ".csv")
#' Output file names
aqs_diagnostics_name <- paste0("AQS_Diagnostics_", state, "_",
                         years[1], "_to_", years[length(years)], ".csv")

#' Name for file with unmeasured locations for kriging
#' this is a grid at 250 m spacing across the Denver area
pts_name <- "Grid_250_m_AEA.csv" 


#' Which daily metrics to load? daily means or ozone 8-h max
daily <- read_csv(here::here("Data", aqs_summ_name)) %>% 
  filter(POC == 1) %>% 
  mutate(monitor_id = str_pad(monitor_id, 7, pad = "0"))

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
  
#' #' just use 5% of points for testing out the script
#' krige_pts <- sample_frac(krige_pts, 0.05)

head(krige_pts)
plot(st_geometry(krige_pts), pch = ".")

#' Converting from sf to sp 
krige_pts_sp <- as(krige_pts, "Spatial")
plot(krige_pts_sp)

#' Monitor locations
monitor_pts <- st_read(here::here("Data", mon_file_name),
                       stringsAsFactors = F, wkt = "WKT",
                       crs = ll_wgs84) %>% 
  mutate(County.Code = str_pad(County.Code, 3, pad = "0"),
         Site.Num = str_pad(Site.Num, 4, pad = "0")) %>% 
  mutate(monitor_id = paste0(County.Code, Site.Num)) %>% 
  st_transform(crs = albers) %>% 
  select(-WKT) 
head(monitor_pts)
plot(st_geometry(monitor_pts), col=as.factor(monitor_pts$Parameter.Code))

#' -----------------------------------------------------------------------------
#' Ordinary kriging

#' List of models to try to fit
#' The ones listed in "all models" are the standard ones to try
show.vgms()
all_models <- c("Exp", "Sph", "Gau", "Cir", "Lin", "Log")

#' cutoff distance is 40 km
#' May need to play with this
c_dist = 40000

#' list of dates, pollutants to loop through
pols <- unique(daily$Parameter.Code)

#' data frames to collect results
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
    
    hist(monitors$mean)
    qqnorm(monitors$mean);qqline(monitors$mean, col=2)
    data_norm_test <- shapiro.test(monitors$mean)
    data_norm_test
    
    #' if data are not normally distributed (based on Shapiro Wilk test), 
    #' use log-transformation-- this can sometimes help, but not always
    #' can change this criterion if needed
    if(data_norm_test$p.value < 0.05) {
      monitors$mean <- log(monitors$mean)
      hist(monitors$mean)
      qqnorm(monitors$mean);qqline(monitors$mean, col=2)
    }
    
    #' Kriging using gstat
    #' First, fit the empirical variogram
    vgm <- variogram(mean ~ 1, monitors, cutoff = c_dist)
    plot(vgm)
    
    #' Second, fit the model
    vgm_fit <- fit.variogram(vgm, model=vgm(all_models), 
                             fit.kappa = seq(.3,5,.01))
    model <- as.character(vgm_fit$model)[nrow(vgm_fit)]
    plot(vgm, vgm_fit)
    
    #' Third, krige
    ok_result <- krige(mean ~ 1, monitors, krige_pts_sp, vgm_fit,
                       maxdist = c_dist)
    
    #' Fourth, leave-one out cross validation
    cv_result <- krige.cv(mean ~ 1, monitors, vgm_fit)
    summary(cv_result)
    hist(cv_result$residual)
    qqnorm(cv_result$residual);qqline(cv_result$residual, col=2)
    cv_res_norm_test <- shapiro.test(cv_result$residual)
    cv_res_norm_test
    
    #' Format kriged data
    #' if data weren't normal, back-transform to original units
    #' need to apply a correction (See Oliver and Webster 2007, PAGE 185)
    #' https://books.google.com/books?hl=en&lr=&id=WBwSyvIvNY8C&oi=fnd&pg=PR5&ots=CCLmSNqK1c&sig=lFZanxv2eVSKec6nPdESzuIFrA4#v=onepage&q&f=false
    #' A back-transformed variance estimate for OK cannot be calculated because
    #' the mean is not known (page 185)
    
    if(data_norm_test$p.value < 0.05) {
      ok_result$var1.pred <- exp(ok_result$var1.pred + (0.5*ok_result$var1.var))
      ok_result$var1.var <- NA
    }
    
    temp <- data.frame(kriged_pt_id = krige_pts_sp@data$grid_id,
                       pollutant = pols[i],
                       date = dates[j],
                       mean_pred = ok_result$var1.pred,
                       mean_var = ok_result$var1.var)
    krige_data <- bind_rows(krige_data, temp)

    #' Data frames of cross-validation and diagnostic results
    
    #' See Li and Heap for a nice explanation of diagnostics
    #' https://pdfs.semanticscholar.org/686c/29a81eab59d7f6b7e2c4b060b1184323a122.pdf
    #' Mean error (same units as pollutants), measures bias should be small
    #' RMSE = root mean squared error (same units as pollutant), should be small
    #' cor_obs_pred = correlation between observed and predicted, should be 1
    #' cor_pred_res = correlation between predicted and residual, should be 0
    cv_compare <- compare.cv(list(krige.cv_output = cv_result))
    cv_result <- as.data.frame(cv_result) %>% 
      mutate(pollutant = pols [i],
             date = dates[j])
    cv_data <- bind_rows(cv_data, cv_result)
    
    temp2 <- data.frame(pollutant = pols [i],
                        date = dates[j],
                        log_transformed = data_norm_test$p.value < 0.05,
                        monitor_n = nrow(monitors),
                        monitor_min = min(monitors$mean, na.rm=T),
                        monitor_max = max(monitors$mean, na.rm=T),
                        monitor_mean = mean(monitors$mean, na.rm=T),
                        model = model,
                        modeled_min = min(ok_result$var1.pred, na.rm=T),
                        modeled_max = max(ok_result$var1.pred, na.rm=T),
                        modeled_mean = mean(ok_result$var1.pred, na.rm=T),
                        mean_error = unname(unlist(cv_compare[1,1])),
                        me_mean = unname(unlist(cv_compare[2,1])),
                        msne = unname(unlist(cv_compare[5,1])),
                        rmse = unname(unlist(cv_compare[8,1])),
                        cor_obs_pred = unname(unlist(cv_compare[6,1])),
                        cor_pred_res = unname(unlist(cv_compare[7,1])),
                        data_norm_test_p = data_norm_test$p.value,
                        cv_res_norm_test_p = cv_res_norm_test$p.value)
    cv_diagnostics <- bind_rows(cv_diagnostics, temp2)
    
    rm(df2, monitors, vgm, vgm_fit, model, 
       ok_result, cv_result, cv_compare,
       temp, temp2)
  }
}

write_csv(krige_data, here::here("Data", aqs_krige_name))
write_csv(cv_data, here::here("Data", aqs_cv_name))
write_csv(cv_diagnostics, here::here("Data", aqs_diagnostics_name))

