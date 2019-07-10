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

if(!dir.exists("./Data/Temp")) dir.create("./Data/Temp")
if(!dir.exists("./Data/AQS_Data")) dir.create("./Data/AQS_Data")
if(!dir.exists("./Data/Met_Data")) dir.create("./Data/Met_Data")

years <- c(2015:2017)
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

#' -----------------------------------------------------------------------------
#' Read in the MET datasets and simplify 
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)

#' Coordinate reference systems 
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

met_vars <- c("PRESS", "RH_DP", "TEMP", "WIND")

for (met in 1:length(met_vars)) {
  print(paste(met, "of", length(met_vars), "variables"))
  
  file_list <- list.files(here::here("Data/Met_Data"), pattern = met_vars[met])
  
  met_data <- data.frame()
  
  for (i in 1:length(file_list)) {
    temp <- read_csv(here::here("Data/Met_Data", file_list[i]))
    colnames(temp) <- gsub(" ", "_", colnames(temp))
    
    temp <- filter(temp, State_Code == "08") %>% 
      mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
             Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
      mutate(monitor_id = paste0(State_Code, County_Code, Site_Num)) %>% 
      rename(Max_Value = "1st_Max_Value")
    
    met_data <- bind_rows(met_data, temp)
    rm(temp)
  }
  
  #' make the met data spatial
  # met_data <- filter(met_data, !is.na(monitor_id)) %>% 
  #   st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>% 
  #   st_transform(crs = albers)
  # 
  # met_name <- paste0("Monitor_", met_vars[met], "_Data_AEA.csv")
  # st_write(met_data, here::here("Data", met_name),
  #          layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
  
  #' Just get Greeley data for Emily
  met_data <- filter(met_data, County_Code == "123")
  
  if(nrow(met_data) > 0) {
    met_name <- paste0("Greeley_", met_vars[met], "_Data_2015_to_2017.csv")
    write_csv(met_data, here::here("Data", met_name)) 
  }
}

