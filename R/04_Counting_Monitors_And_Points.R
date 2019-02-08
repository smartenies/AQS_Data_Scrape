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
library(spatialEco)
library(gstat)
library(automap)
library(tidyverse)

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' -----------------------------------------------------------------------------
#' For each year and pollutant, how many monitors are used at each location?
#' -----------------------------------------------------------------------------

#' For which years and which state do we want data?
years <- c(2010:2014)
state <- "08" #Colorado
time_zone <- "America/Denver"

#' List of files to krige
aqs_data_path <- "C:/Users/semarten/OneDrive for Business/Research/For Wande/"
diagnostic_files <- data.frame(diagnostic_files = list.files(path = aqs_data_path)) %>% 
  mutate(diagnostic_files = as.character(diagnostic_files)) %>% 
  filter(str_detect(diagnostic_files, "Diagnostics")) %>% 
  filter(str_detect(diagnostic_files, 
                    paste(as.character(years),collapse = '|')))
diagnostic_files <- diagnostic_files$diagnostic_files

monitor_df <- data.frame()

for (i in 1:length(diagnostic_files)) {
  
  diag <- read_csv(paste0(aqs_data_path, diagnostic_files[i])) %>% 
    group_by(pollutant) %>% 
    summarize(min_monitor_n = min(monitor_n),
              median_monitor_n = median(monitor_n),
              max_monitor_n = max(monitor_n),
              mean_monitor_n = mean(monitor_n),
              sd_monitor_n = sd(monitor_n))
  
  monitor_df <- bind_rows(monitor_df, diag)
  rm(diag)
}
  
for_wande <- "C:/Users/semarten/OneDrive for Business/Research/For Wande/"
write_csv(monitor_df, paste0(for_wande, "Monitor_Counts_by_Pol_Year.csv"))

