


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