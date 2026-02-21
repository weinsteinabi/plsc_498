#import libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)

#set up files
dir.create("06_week/scripts")
dir.create("06_week/figures")
dir.create("06_week/outputs")

#check file set up
getwd()
list.files("06_week")
list.files("06_week/data")

#import and check data
df <- readRDS("06_week/data/nycflights13.rds")
dim(df)
names(df)
head(df)

#check for vars of interest
c("dep_delay", "arr_delay") %in% names(df)

#clean data
df <- df %>% filter(!is.na(arr_delay)) %>% select(c(dep_delay, arr_delay, air_time, distance, carrier, origin))
dim(df)
names(df)

#histograms for departure delay
hist_dep1 <- ggplot(df, aes(x = dep_delay)) + geom_histogram(fill = "lightgreen", binwidth = 50) + labs(
  caption = "Deparature Delay in Minutes; Binwidth = 50", 
  x = "Departure Delay", 
  y = "Frequency") + 
  theme_minimal() + 
  scale_y_continuous(n.breaks = 5, limits = c(0, 300000), label = comma) + 
  scale_x_continuous(n.breaks = 10, limits = c(-200, 1400)) + 
  annotate("label", x = 1000, y = 175000, label = "Binwidth = 50")

hist_dep2 <- ggplot(df, aes(x = dep_delay)) + geom_histogram(fill = "lightgreen", binwidth = 25) + labs(
  caption = "Deparature Delay in Minutes; Binwidth = 25", 
  x = "Departure Delay", 
  y = "Frequency") + 
  theme_minimal() + 
  scale_y_continuous(n.breaks = 5, limits = c(0, 300000), label = comma) + 
  scale_x_continuous(n.breaks = 10, limits = c(-200, 1400)) + 
  annotate("label", x = 1000, y = 175000, label = "Binwidth = 25")
hist_bin1 <- ggarrange(hist_dep1, hist_dep2)
hist_bin1 <- annotate_figure(hist_bin1, top = text_grob("Flight Depature Delay in Minutes"))

ggsave(hist_bin1, file = "06_week/figures/dep_delay_hist.png")

#histograms for arrival delay
hist_arr1 <- ggplot(df, aes(x = arr_delay)) + geom_histogram(fill = "lightblue", binwidth = 50) + labs(
  caption = "Arrival Delay in Minutes; Binwidth = 50", 
  x = "Arrival Delay", 
  y = "Frequency") + 
  theme_minimal() + 
  scale_y_continuous(n.breaks = 5, limits = c(0, 300000),label = comma) + 
  scale_x_continuous(n.breaks = 10, limits = c(-200, 1400)) + 
  annotate("label", x = 1000, y = 175000, label = "Binwidth = 50")
hist_arr2 <- ggplot(df, aes(x = arr_delay)) + geom_histogram(fill = "lightblue", binwidth = 25) + labs(
  caption = "Arrival Delay in Minutes; Binwidth = 25", 
  x = "Arrival Delay", 
  y = "Frequency") + 
  theme_minimal() + 
  scale_y_continuous(n.breaks = 5, limits = c(0, 300000),label = comma) + 
  scale_x_continuous(n.breaks = 10, limits = c(-200, 1400)) + 
  annotate("label", x = 1000, y = 175000, label = "Binwidth = 25")
hist_bin2 <- ggarrange(hist_arr1, hist_arr2)
hist_bin2 <- annotate_figure(hist_bin2, top = text_grob("Flight Arrival Delay in Minutes"))

ggsave(hist_bin2, file = "06_week/figures/arr_delay_hist.png")

#density curve for departure delay
den_dep1 <- ggplot(df, aes(x = dep_delay)) + geom_density(color = "lightgreen", fill = "lightgreen") + labs(
  caption = "Departure Delay Density Plot; Default Smoothing",
  x = "Departure Delay in Minutes", 
  y = "Density") + 
  scale_y_continuous(n.breaks = 5, limits = c(0, .1)) + 
  scale_x_continuous(n.breaks = 10, limits = c(-200, 1400)) +
  annotate("label", x = 1000, y = .06, label = "Smooth: Default") +
  theme_minimal()
den_dep2 <- ggplot(df, aes(x = dep_delay)) + geom_density(color = "lightgreen", fill = "lightgreen", adjust = .5) + labs(
  caption = "Depature Delay Density Plot; Adjusted Smoothing",
  x = "Departure Delay in Minutes", 
  y = "Density") + 
  scale_y_continuous(n.breaks = 5, limits = c(0, .1)) + 
  scale_x_continuous(n.breaks = 10, limits = c(-200, 1400)) + 
  annotate("label", x = 1000, y = .06, label = "Smooth: .5") + 
  theme_minimal()
den_dep <- ggarrange(den_dep1, den_dep2)
den_dep <- annotate_figure(den_dep, top = text_grob("Departure Delay Density Plots"))

ggsave(den_dep, file = "06_week/figures/density_dep.png")

#select a hist and curve and compare - group
comp_dep <- ggarrange(den_dep1, hist_dep2)
comp_dep <- annotate_figure(comp_dep, top = text_grob("Departure Delay in Minutes: Density Plot vs Histogram"))
ggsave(comp_dep, file = "06_week/figures/dep_delay_hist_vs_density.png")

#grouped histogram - color vs faceting
color_hist <- ggplot(df, aes(x = dep_delay, fill = origin)) + 
  geom_histogram(binwidth = 25, position = "identity", alpha = .4) + 
  theme_minimal() + 
  labs(
    caption = "Origin by Color", 
    fill = "Airport of Origin", 
    x = "Departure Delay in Minutes", 
    y = "Frequency"
  ) + 
  scale_y_continuous(n.breaks = 5, limits = c(0, 90000), label = comma) + 
  scale_x_continuous(n.breaks = 5, limits = c(-200, 1400))
faceted_hist <- ggplot(df, aes(x = dep_delay, fill = origin)) + geom_histogram(binwidth = 25) + 
  facet_wrap(~origin, labeller = labeller("")) + 
  theme_minimal() + 
  labs(
    x = "Departure Delay in Minutes", 
    y = "Frequency", 
    fill = "Airport of Origin",
    caption = "Origin by Facets"
  ) + 
  scale_y_continuous(n.breaks = 5, limits = c(0, 90000), label = comma) + 
  scale_x_continuous(n.breaks = 5, limits = c(-200, 1400)) + theme(strip.text.x = element_blank(), axis.text.x = element_text(angle = 60))
group_hist <- ggarrange(color_hist, faceted_hist, common.legend = TRUE)
group_hist <- annotate_figure(group_hist, top = text_grob("Histograms of Departure Delay by Airport of Origin"))

ggsave(group_hist, file = "06_week/figures/grouped_dep_delay.png")

#distance vs airtime plot
scatter_reg <- ggplot(df, aes(x = distance, y = air_time)) + geom_point(color = "violet") + theme_minimal() + 
  labs(
    x = "Distance (in miles)", 
    y = "Air Time (in minutes)", 
    title = "Distance Traveled and Air Time per Flight", 
    caption = "Alpha Level = 1.0"
  )
ggsave(scatter_reg, file = "06_week/figures/airtime_distance_raw.png")

scatter_alpha <- ggplot(df, aes(x = distance, y = air_time)) + geom_point(color = "violet", alpha = .5) + 
  theme_minimal() + 
  labs(
    x = "Distance (in miles)", 
    y = "Air Time (in minutes)", 
    title = "Distance Traveled and Air Time per Flight",
    caption = "Alpha Level = .5"
  )
ggsave(scatter_alpha, file = "06_week/figures/airtime_distance_alpha.png")
