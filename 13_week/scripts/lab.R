#import libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(readr)
library(patchwork)
library(usmap)
library(stringr)
#set up files
dir.create("13_week/scripts")
dir.create("13_week/outputs")
dir.create("13_week/figures")

list.files("13_week")
list.files("13_week/data")

#import data
us <- read_csv("13_week/data/us_states.csv")
state_data <- read_csv("13_week/data/state_data.csv")

#to-do: 
# write-up

#explore data (boundary)
dim(us) #rows: 15537, columns: 6
names(us)

#explore data (fill)
dim(state_data) #rows: 50, columns: 9
names(state_data)

summary(state_data$Life.Exp)
summary(state_data$Murder)

#transform data as needed: 
state_data <- state_data %>% mutate(
  life_dev = Life.Exp - mean(Life.Exp)
)

state_data <- state_data %>% mutate(
  grad_cat = cut(HS.Grad, 
                 breaks = c(0, 25, 50, 75, 100), 
                 labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%")
                 )
)

#merge data
us_merged <- left_join(state_data, us, by = join_by(region == region))

#fix region title
us_merged$region = str_to_title(us_merged$region)

#deduplicate for different method of visualization (includes Alaska and Hawaii)
us_merged_dup <- rename(us_merged, "state" = region)
us_merged_dup <- us_merged_dup %>% select(-c("group", "order", "long", "lat", 
                                             "subregion")) %>% unique(.keep_all = TRUE)
#top 20 data
top_20 <- us_merged_dup %>% 
  arrange(desc(Murder)) %>% 
  slice_head(n = 20) %>% 
  mutate(state = reorder(state, Murder))

#visual 1: murder choropleth - w/o Alaska and Hawaii
vis1 <- ggplot(us_merged, 
               aes(x = long, y = lat, group = group, fill = Murder)) + 
  geom_polygon(color = "white", 
               linewidth = .3) + 
  coord_quickmap() + 
  scale_fill_viridis_c(begin = 1, 
                       end = 0, 
                       option = "rocket") + 
  theme_void() + 
  labs(
    fill = "Murder Rate \nper 100k"
  ) + 
  theme(  
    legend.title.position = "top",
    legend.title = element_text(size = 8),
    panel.border = element_rect(colour = "black"))

vis1 <- annotate_figure(vis1, top = text_grob("per 100,000 people, 1976", size = 8))
vis1 <- annotate_figure(vis1, top = text_grob("Murder Rate by State"))
vis1

#visual 1: murder choropleth - w/ Alaska and Hawaii
vis1_w <- plot_usmap(regions = "states", 
                     data = us_merged_dup, 
                     values = "Murder", 
                     color = "white") + 
  scale_fill_viridis_c(begin = 1, 
                       end = 0, 
                       option = "rocket", 
                       limits = c(0, 20), 
                       labels = c(0, 5, 10, 15, 20)) + 
  theme_void() + labs(
    fill = "Murder Rate \nper 100k"
  ) + 
  theme(
         legend.title.position = "top",
         legend.title = element_text(size = 8), 
         legend.text = element_text(size = 7))

vis1_w <- annotate_figure(vis1_w, top = text_grob("per 100,000 people, U.S. 1976", size = 8))
vis1_w <- annotate_figure(vis1_w, top = text_grob("Murder Rate by State"))
vis1_w

ggsave("13_week/figures/murder_rate_map.png", vis1_w)

#visual 2: life expectancy - w/o Alaska and Hawaii
vis2 <- ggplot(us_merged, 
               aes(x = long, y = lat, group = group, fill = life_dev)) + 
  geom_polygon(color = "grey", 
               linewidth = .3) + 
  coord_quickmap() + 
  scale_fill_gradient2(
    low = "purple", 
    mid = "white", 
    high = "green", 
    midpoint = 0, 
    name = "Deviation in Years from \nNational Avg. Life Expectancy"
  ) + 
  theme_void() + 
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(size = 8))

vis2 <- annotate_figure(vis2, top = text_grob("in relation to the National Average (70.87 years)", size = 8))
vis2 <- annotate_figure(vis2, top = text_grob("Life Expectancy by State"))
vis2

#visual 2: life expectancy - w/ Alaska and Hawaii
vis2_w <- plot_usmap(regions = "states", 
                     data = us_merged_dup, 
                     values = "life_dev", 
                     color = "grey") + 
  scale_fill_gradient2(
    low = "purple", 
    mid = "white", 
    high = "green", 
    midpoint = 0, 
    name = "Deviation in Years from \nNational Avg. Life \nExpectancy"
  ) + 
  theme_void() + 
  theme(
    legend.title.position = "top",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7))

vis2_w <- annotate_figure(vis2_w, top = text_grob("in relation to the National Average (70.87 years)", size = 8))
vis2_w <- annotate_figure(vis2_w, top = text_grob("Life Expectancy by State"))
vis2_w

ggsave("13_week/figures/life_exp_diverging.png", vis2_w)

#visual 3: high school graduation - w/o Alaska and Hawaii
vis3 <- ggplot(us_merged, 
               aes(x = long, y = lat, group = group, fill = grad_cat)) + 
  geom_polygon(color = "white", 
               linewidth = .3, 
               show.legend = TRUE) + 
  coord_quickmap() +
  scale_fill_brewer(type = "qual", 
                    palette = 5,
                    labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%"),
                    drop = FALSE) + 
  theme_void() + 
  labs(
    fill = "Graduation Rate (%)"
  )

vis3  <- annotate_figure(vis3, top = text_grob("Graduation Rate (%), U.S. 1976", size = 8))
vis3 <- annotate_figure(vis3, top = text_grob("High School Education by State"))
vis3 

#visualization 3: graduation categories - w/ Alaska and Hawaii
vis3_w <- plot_usmap(regions = "states", 
                     data = us_merged_dup, 
                     values = "grad_cat", 
                     color = "white", 
                     show.legend = TRUE) +
  scale_fill_brewer(type = "qual", 
                    palette = 5,
                    limits = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%"),
                    labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 100%"),
                    drop = FALSE) + 
  theme_void() + 
  labs(
    fill = "Graduation Rate (%)"
  ) + 
  theme(legend.title = element_text(size = 7),
        legend.text = element_text(size = 5))

vis3_w <- annotate_figure(vis3_w, top = text_grob("Graduation Rate (%), U.S. 1976", size = 8))
vis3_w <- annotate_figure(vis3_w, top = text_grob("High School Education by State"))
vis3_w 

ggsave("13_week/figures/hs_grad_binned.png", vis3_w)

#visual 4: dot plot top 20
vis1_dup <- plot_usmap(regions = "states", 
                     data = us_merged_dup, 
                     values = "Murder", 
                     color = "white") + 
  scale_fill_viridis_c(begin = 1, 
                       end = 0, 
                       option = "rocket", 
                       limits = c(0, 20), 
                       labels = c(0, 5, 10, 15, 20)) + 
  theme_void() + 
  labs(
    fill = "Murder Rate \nper 100k"
  ) + 
  theme(
    legend.position = "right",
    legend.title.position = "top",
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 5),
    panel.border = element_rect(color = "black", linewidth = .5))

vis4 <- ggplot(top_20, 
               aes(x = Murder, y = state, color = Murder)) + 
  geom_point(size = 1.5) + 
  theme_classic() + 
  labs(
   y = "", 
   x = "Murder Rate per 100,000 People") + 
  scale_x_continuous(n.breaks = 15) + 
  scale_color_viridis_c(begin = 1, 
                        end = 0, 
                        option = "rocket",
                        limits = c(0, 20), 
                        labels = c(0, 5, 10, 15, 20)) + 
  theme(legend.position = 'none', 
        axis.text = element_text(size = 5), 
        axis.title = element_text(size = 7))

vis4 <-  vis1_dup + vis4 + plot_annotation(title = "Murder Rate by State per 100,000 people, \nU.S. 1976",
                                           caption = "Map of Murder Rate by State", 
                                        theme = theme(plot.title = element_text(hjust = .5, size = 10), 
                                                      plot.caption = element_text(hjust = .15, vjust = 20, size = 7)))
vis4

ggsave("13_week/figures/murder_dotplot.png", vis4)

