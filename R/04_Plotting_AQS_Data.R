

#########################
# UNDER CONSTRUCTION-- 
#########################







library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggthemes)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)
library(viridis)
library(IC2)
library(haven)

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
spaghetti_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks.y = element_line(colour = "black"),
  axis.text.y = element_text(color = "black", size=10),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
map_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

geo_data <- "T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data/"
utm_13 <- "+init=epsg:26913"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"



#' -----------------------------------------------------------------------------
#' Time series plots of weekly pm2.5 and ozone at each monitor
#' -----------------------------------------------------------------------------

load("./Data/CEI Data/pm_monitor_metrics.RData")
load("./Data/CEI Data/o3_monitor_metrics.RData")

class(pm_average)
class(o3_average)

ggplot(data=pm_average, aes(x=week_ending, y=weekly_average, color=monitor_id)) +
  geom_point(aes(group=monitor_id)) +
  geom_line(aes(group=monitor_id)) +
  #geom_smooth(method = "loess", size=1) +
  geom_hline(aes(yintercept = 12), lty=4, color="blue", size=1) +
  #scale_x_datetime(date_breaks = "6 months", date_labels =  "%b %y") +
  xlab("Date") + ylab("PM\u2082.\u2085 (\u03BCg/m\u00B3)") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "left") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Criteria Pollutants/PM_2009-2017.jpeg",
       device = "jpeg", dpi=600)

ggplot(data=o3_average, aes(x=week_ending, y=weekly_average)) +
  geom_point(aes(group=monitor_id, color=monitor_id)) +
  geom_smooth(method = "loess", color="black", size=1) +
  geom_hline(aes(yintercept = 0.075), lty=4, color="blue", size=1) +
  scale_x_datetime(date_breaks = "6 months", date_labels =  "%b %y") +
  xlab("Date") + ylab("O\u2083 (ppm)") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "left") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Criteria Pollutants/O3_2009-2017.jpeg",
       device = "jpeg", dpi=600)

#' -----------------------------------------------------------------------------
#' Histograms of weekly pm2.5 and ozone at each monitor
#' -----------------------------------------------------------------------------

ggplot(data=pm_average, aes(x = weekly_average)) +
  geom_histogram(aes(group=monitor_id, fill=monitor_id)) +
  xlab("PM\u2082.\u2085 (\u03BCg/m\u00B3)") +
  theme(legend.position = "left") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Criteria Pollutants/PM_Histogram.jpeg",
       device = "jpeg", dpi=600)

ggplot(data=o3_average, aes(x = weekly_average)) +
  geom_histogram(aes(group=monitor_id, fill=monitor_id)) +
  xlab("O\u2083 (ppm)") +
  theme(legend.position = "left") +
  simple_theme
ggsave(filename = "./Figures/CEI Figures/Criteria Pollutants/O3_Histogram.jpeg",
       device = "jpeg", dpi=600)