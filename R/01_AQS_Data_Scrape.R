#' -----------------------------------------------------------------------------
#' Date created: October 25, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: Scrape air pollution data from the EPA AQS website
#' https://aqs.epa.gov/api
#' Username: sheena.martenies@colostate.edu
#' Password: khakifrog54
#' 
#' NOTE: This can sometimes take a while, depending on the number of monitors,
#' pollutants, and years. I often just let this run after work and come back 
#' to it in the morning
#' 
#' NOTE: In light of some server issues on the EPA website, I've updated the
#' script to just download the premade files
#' ----------------------------------------------------------------------------


library(tidyverse)
library(readxl)

if(!dir.exists("./Data/AQS_Data")) dir.create("./Data/AQS_Data")
if(!dir.exists("./Data/Met_Data")) dir.create("./Data/Met_Data")

years <- c(2000:2018)
met_vars <- c("WIND", "PRESS", "TEMP", "RH_DP")

for (i in 1:length(years)) {
  
  #' Daily PM2.5
  aqs_url <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_88101_", 
                    years[i], ".zip")
  
  download.file(aqs_url, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = here::here("Data/AQS_Data"))
  
  #' Daily met variables
  for (j in 1:length(met_vars)) {
    met_url <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_", met_vars[j],  
                      "_", years[i], ".zip")
    
    #' Download zipfile from the EPA website and unzip
    download.file(met_url, destfile = here::here("Data/Temp", "temp.zip"))
    unzip(here::here("Data/Temp", "temp.zip"), exdir = here::here("Data/Met_Data"))
  }
  
  #' daily mean ozone data
  aqs_url2 <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_44201_", 
                     years[i], ".zip")
  
  download.file(aqs_url2, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = here::here("Data/AQS_Data"))
  
  #' daily 8h max ozone data
  aqs_url3 <- paste0("https://aqs.epa.gov/aqsweb/airdata/8hour_44201_", 
                     years[i], ".zip")
  
  download.file(aqs_url3, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = here::here("Data/AQS_Data"))
}

#' 
#' #' ----------------------------------------------------------------------------
#' #' This script is based on a previous script for scraping AQS data
#' #' AUTHOR: Chad W. Milando (Univ of Michigan; cmilando@umich.edu)
#' #' DATE: 5/18/2016
#' #' PURPOSE: scraper of AQS
#' #' ----------------------------------------------------------------------------
#' 
#' library(Hmisc)
#' library(stringr)
#' library(readr)
#' library(dplyr)
#' library(sf)
#' 
#' #' User info for AQS website:
#' user_name <- "sheena.martenies@colostate.edu"
#' pw <- "khakifrog54"
#' 
#' #' For which years and which state do we want data?
#' years <- c(2010:2010)
#' state <- "08" #Colorado
#' time_zone <- "America/Denver"
#' 
#' #' File name for output data
#' aqs_file_name <- paste0("AQS_Data_", state, "_", 
#'                         years[1], "_to_", years[length(years)], ".csv")
#' mon_file_name <- paste0("AQS_Monitors_", state, "_", 
#'                         years[1], "_to_", years[length(years)], ".csv")
#' 
#' #' Denver Metro counties: Adams (001), Arapahoe (005), Boulder (013), Broomfield
#' #' (014), Denver (031), Douglas (035), Jefferson (059), Larimer (069), Weld (123) 
#' all_counties <-c("001", "003", "005", "007", "009", "011", "013", "014", "015",
#'                  "017", "019", "021", "023", "025", "027", "029", "031", "033",
#'                  "035", "037", "039", "041", "043", "045", "047", "049", "051",
#'                  "053", "055", "057", "059", "061", "063", "065", "067", "069",
#'                  "071", "072", "073", "075", "077", "079", "081", "083", "085",
#'                  "087", "089", "091", "093", "095", "097", "099", "103", "105",
#'                  "107", "109", "111", "113", "115", "117", "119", "121", "123",
#'                  "125") 
#' 
#' #all_counties <- str_pad(as.character(1:125), width = 3, pad = "0")
#' 
#' #' Choose which pollutants to scrape
#' #' https://aqs.epa.gov/aqsweb/codes/data/ParametersByDesc.csv
#' #' Criteria pollutants and carbon parameters
#' 
#' # params <- c("14129", "42101", "42401", "42602", "44201", "88101", 
#' #             "16111", "88317", "88321")
#' params <- c("88101", "44201") #' ozone and PM2.5
#' output <- data.frame()
#' 
#' #' May have to confirm that the aqs_link (below) works
#' for(county in all_counties) {
#'   print(paste("County:", county))
#'   for(param in params) {
#'     for(year in years) {
#'       #for(month in c(1:12)) {
#'       
#'       bdate <- paste0(year,sprintf("%02i",1),"01")
#'       edate <- paste0(year,sprintf("%02i",12),31)
#'       
#'       prefix <- paste0("https://aqs.epa.gov/api/rawData?user=",
#'                        user_name, "&pw=", pw, "&format=DMCSV&param=")
#'       aqs_link <- paste0(prefix,param,
#'                          "&bdate=",bdate,"&edate=",edate,"&state=",state,
#'                          "&county=",county,collapse = "")
#'       error_catch <- F; warn_catch <- F
#'       tryCatch(read.csv(aqs_link),error = function(e) error_catch <- T, 
#'                warning = function(w) warn_catch <- T)
#'       if(!error_catch) {
#'         aqs_data <- read.csv(aqs_link)[-1,]
#'         if(length(which(aqs_data$Latitude == "END OF FILE")) > 0) {
#'           aqs_data <- aqs_data[-which(aqs_data$Latitude == "END OF FILE"),]
#'         }
#'         
#'         if(nrow(aqs_data) > 0) {
#'           if(nrow(output) > 0) {
#'             output <- bind_rows(output,aqs_data)
#'           }
#'           else {
#'             output <- aqs_data
#'           }
#'           rm(aqs_data)
#'         }
#'       }
#'       
#'       cat("param = ",param,year,"; error?",error_catch,
#'           "; warn?", warn_catch,"\n")
#'       #   }
#'     }
#'   }
#' }
#' 
#' output <- output %>% 
#'   mutate(datetime = as.POSIXct(paste(Date.Local, X24.Hour.Local), 
#'                                format="%Y-%m-%d %H",tz = time_zone),
#'          Latitude = as.numeric(as.character(Latitude)),
#'          County.Code = str_pad(County.Code, 3, pad = "0"),
#'          Site.Num = str_pad(Site.Num, 4, pad = "0")) %>% 
#'   mutate(monitor_id = paste0(County.Code, Site.Num))
#' glimpse(output)
#' 
#' write_csv(output, here::here("Data", aqs_file_name))
#' 
#' #' Create an sf object for the monitor locations
#' 
#' ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#' 
#' monitors <- select(output, County.Code, Site.Num, Parameter.Code,
#'                    monitor_id, Longitude, Latitude) %>% 
#'   distinct() %>% 
#'   st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84)
#' 
#' plot(st_geometry(monitors))
#' 
#' st_write(monitors, dsn = here::here("Data", mon_file_name),
#'          layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
