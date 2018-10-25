#' -----------------------------------------------------------------------------
#' Date created: October 25, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: This script calculates daily means at each monitor
#' Also includes code to calculate MDA8 for ozone data
#' -----------------------------------------------------------------------------

library(sf)
library(tidyverse)

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Read in pollutant data and summarize to daily means
#' Source: AQS Data Mart (Script: 01_AQS_Data_Scrape.R)
#' -----------------------------------------------------------------------------

#' https://aqs.epa.gov/aqsweb/codes/data/ParametersByDesc.csv
#' Criteria pollutants and carbon parameters

pol <- c("88101", "44201") # PM2.5 and O3
aqs_file_name <- "AQS_Data.csv"
aqs_summ_name <- "AQS_Daily_Mean_Summary.csv"
aqs_o3_summ_name <- "AQS_Ozone_MDA8_Summary.csv"

mon_data <- read_csv(here::here("Data", aqs_file_name)) 

#' Daily means for each pollutant
daily_means <- mon_data %>% 
  filter(Parameter.Code %in% pol) %>% 
  group_by(Parameter.Code, monitor_id, POC, Date.Local, Units.of.Measure) %>% 
  summarize(mean = mean(Sample.Measurement, na.rm=T))
  
write_csv(daily_means, here::here("Data", aqs_summ_name))


#' -----------------------------------------------------------------------------
#' Calcuating the daily 8-hour max concentration for ozone
#' 
#' Note: I'm sure there's probably a more efficient, less ugly way to do this,
#' and maybe one day I'll get there... 
#' -----------------------------------------------------------------------------

#' Subset to O3, add an identifier for hour, day, week, month, and year
o3 <- filter(mon_data, Parameter.Code == "44201") %>%
  mutate(mdy = strftime(datetime, format = "%m%d%y"),
         year = strftime(datetime, format = "%Y"),
         month = strftime(datetime, format = "%m"),
         hour = strftime(datetime, format = "%H")) %>%
  arrange(datetime, monitor_id) %>%
  
  #' Drop missing hours (daylight savings)
  filter(!is.na(datetime))

#' #' Need a wide dataset 
#' o3_wide <- select(o3, datetime, monitor_id, Sample.Measurement) %>%
#'   spread(key = monitor_id, value = Sample.Measurement)

#' Calculate the daily 8-hr max 
#' (highest 8 h mean in the 24 h period)
#' See CFR Title 40 Vol 2 Part 50 Appendix P for details
#' https://www.gpo.gov/fdsys/pkg/CFR-2017-title40-vol2/xml/CFR-2017-title40-vol2-part50.xml

length(unique(o3$monitor_id))

monitor_list <- unique(o3$monitor_id)
temp_df <- data.frame()

for (i in 1:length(monitor_list)) {
  print(paste("Monitor", i, "of", length(monitor_list)))
  df1 <- filter(o3, monitor_id == monitor_list[i]) %>%
    arrange(datetime)
  
  for(m in 1:length(unique(df1$POC))) {
    df1a <- filter(df1, POC == unique(df1$POC)[m])
    day_list <- unique(df1a$mdy)
    
    for (j in 1:length(day_list)) {
      days <- c(day_list[j], day_list[j+1])
      df2 <- filter(df1a, mdy %in% days)
      
      avg_list <- list()
      a <- 24 #should have 24 moving averages starting at 0:00 and ending at 23:00
      
      for (k in 1:a) {
        hours <- seq(from=k, to=k+7)
        df3 <- slice(df2, hours) 
        
        #' need at least 6 hourly concentrations (75%) to calculate mean
        avg_list[k] <- ifelse(sum(!is.na(df3$Sample.Measurement)) >= 6, 
                              mean(df3$Sample.Measurement, na.rm=T), 
                              NA)
      }
      rm(df3)
      b <- unlist(avg_list)
      
      #' only valid if there are >= 18 (75%) 8-hr means
      max <- ifelse(sum(!is.na(b)) >= 13, max(b, na.rm=T), NA)
      
      temp <- data.frame(monitor_id = unique(o3$monitor_id)[i],
                         POC = unique(df1$POC)[m],
                         mdy = days[1], 
                         d8hmax = max)
      temp_df <- rbind(temp_df, temp)
      rm(temp, df2) 
    }
  }
  rm(df1)
}

temp_df <- mutate_if(temp_df, is.factor, as.character)
o3_MDA8 <- left_join(o3, temp_df, by=c("monitor_id", "mdy", "POC")) %>% 
  select(Parameter.Code, monitor_id, POC, Date.Local, Units.of.Measure, d8hmax) %>% 
  distinct() %>% 
  arrange(monitor_id, POC, Date.Local)

write_csv(o3_MDA8, here::here("Data", aqs_o3_summ_name))

