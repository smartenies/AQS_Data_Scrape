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
#' ----------------------------------------------------------------------------

#' ----------------------------------------------------------------------------
#' This script is based on a previous script for scraping AQS data
#' AUTHOR: Chad W. Milando (Univ of Michigan; cmilando@umich.edu)
#' DATE: 5/18/2016
#' PURPOSE: scraper of AQS
#' ----------------------------------------------------------------------------

library(Hmisc)
library(stringr)
library(readr)
library(dplyr)

#' User info for AQS website:
user_name <- "sheena.martenies@colostate.edu"
pw <- "khakifrog54"

#' File name for output data
aqs_file_name <- "AQS_Data.csv"
mon_file_name <- "AQS_Monitors.csv"

#' For which years do we want data?
years <- c(2009:2017)
state <- "08" #Colorado
time_zone <- "America/Denver"

#' Denver Metro counties: Adams (001), Arapahoe (005), Boulder (013), Broomfield
#' (014), Denver (031), Douglas (035), Jefferson (059), Larimer (069), Weld (123) 
all_counties <-c("001", "003", "005", "007", "009", "011", "013", "014", "015",
                 "017", "019", "021", "023", "025", "027", "029", "031", "033",
                 "035", "037", "039", "041", "043", "045", "047", "049", "051",
                 "053", "055", "057", "059", "061", "063", "065", "067", "069",
                 "071", "072", "073", "075", "077", "079", "081", "083", "085",
                 "087", "089", "091", "093", "095", "097", "099", "103", "105",
                 "107", "109", "111", "113", "115", "117", "119", "121", "123",
                 "125") 

#all_counties <- str_pad(as.character(1:125), width = 3, pad = "0")
#all_counties <- "031"

#' https://aqs.epa.gov/aqsweb/codes/data/ParametersByDesc.csv
#' Criteria pollutants and carbon parameters

params <- c("14129", "42101", "42401", "42602", "44201", "88101", 
            "16111", "88317", "88321")

output <- data.frame()

#' might not work the first time. if not just try typing in the link first yourself
for(county in all_counties) {
  print(paste("County:", county))
  for(param in params) {
    for(year in years) {
      #for(month in c(1:12)) {
      
      bdate <- paste0(year,sprintf("%02i",1),"01")
      edate <- paste0(year,sprintf("%02i",12),31)
      
      prefix <- paste0("https://aqs.epa.gov/api/rawData?user=",
                       user_name, "&pw=", pw, "&format=DMCSV&param=")
      aqs_link <- paste(prefix,param,
                        "&bdate=",bdate,"&edate=",edate,"&state=",state,
                        "&county=",county,collapse = "",sep="")
      error_catch <- F; warn_catch <- F
      tryCatch(read.csv(aqs_link),error = function(e) error_catch <- T, 
               warning = function(w) warn_catch <- T)
      if(!error_catch) {
        aqs_data <- read.csv(aqs_link)[-1,]
        if(length(which(aqs_data$Latitude == "END OF FILE")) > 0) {
          aqs_data <- aqs_data[-which(aqs_data$Latitude == "END OF FILE"),]
        }
        
        if(nrow(aqs_data) > 0) {
          if(nrow(output) > 0) {
            output <- rbind(output,aqs_data)
          }
          else {
            output <- aqs_data
          }
          rm(aqs_data)
        }
      }
      
      cat("param = ",param,year,"; error?",error_catch,
          "; warn?", warn_catch,"\n")
      #   }
    }
  }
}

output <- output %>% 
  mutate(datetime = as.POSIXct(paste(Date.Local, X24.Hour.Local), 
                               format="%Y-%m-%d %H",tz = time_zone),
         Latitude = as.numeric(as.character(Latitude)),
         County.Code = str_pad(County.Code, 3, pad = "0"),
         Site.Num = str_pad(Site.Num, 4, pad = "0")) %>% 
  mutate(monitor_id = paste0(County.Code, Site.Num))
glimpse(output)

write_csv(output, here::here("Data", aqs_file_name))

#' Create and sf object for the monitor locations
library(sf)
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

monitors <- select(output, County.Code, Site.Num, Parameter.Code,
                   monitor_id, Longitude, Latitude) %>% 
  distinct() %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84)

plot(st_geometry(monitors))

st_write(monitors, dsn = here::here("Data", mon_file_name),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
