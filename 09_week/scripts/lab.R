
#import libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(scales)
#set up files
dir.create("09_week/scripts")
dir.create("09_week/outputs")
dir.create("09_week/figures")

#check files
getwd()
list.files("09_week")
list.files("09_week/data")

#import data
df <- readRDS("09_week/data/state_df.rds")

#check data
dim(df)
names(df)
df %>% select(c(totalvotes, biden_votes, trump_votes, covid_deaths_to_2020_11_07, pneumonia_deaths_to_2020_11_07)) %>% 
  summary(df)

#new variable - total deaths
df$totalillness <- df$covid_deaths_to_2020_11_07 + df$pneumonia_deaths_to_2020_11_07

nationaltotal <- sum(df$totalillness)
nationalmin <- min(df$totalillness)
nationalmax <- max(df$totalillness)

#new variable - winner
df$winner <- ifelse(df$biden_share > df$trump_share, "Biden", "Trump")

#rename illness vars
df <- rename(df, `COVID-19` = "covid_deaths_to_2020_11_07") 
df <- rename(df, Pneumonia = "pneumonia_deaths_to_2020_11_07") 

#visual 1: bar plot - total deaths by state
winner_color = c(Biden = "#7a94db", Trump = "#ea2419")
vis1 <- ggplot(df, aes(y = reorder(state_po, totalillness), x = totalillness, fill = winner)) + 
  geom_col(width = .6) + 
  scale_x_continuous(labels = comma) + 
  scale_fill_manual(values = winner_color) + 
  theme_bw() + labs(
    x = "Total Count of Deaths", 
    y = NULL, 
    fill = "Winning Candidate:"
  ) + theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 6), 
    legend.position = "bottom"
  )
title <- expression(scriptstyle("Data collection ended November 7, 2020 - data shown as raw counts"))
vis1 <- annotate_figure(vis1, top = text_grob(title))
vis1 <- annotate_figure(vis1, top = text_grob("Total Deaths (COVID-19 and Pneumonia) by State"))
vis1

ggsave("09_week/figures/total_illness_deaths_by_state.png", vis1)

#visual 2: bar chart - proportion of deaths by winner 
props <- df %>% group_by(winner) %>% summarize(
  totaldeaths = sum(totalillness), 
  n_states = n()) %>% 
  ungroup() %>%
  mutate(pctdeaths = totaldeaths/nationaltotal)

vis2 <- ggplot(props, aes(x = winner, y = pctdeaths, fill = winner)) + 
  geom_col() + 
  scale_fill_manual(values = winner_color) + 
  theme_bw() + 
  geom_text(aes(label = sprintf("%.1f%%\n(%s deaths)",
                                pctdeaths * 100,
                                comma(totaldeaths))), vjust = -0.3, size = 3.8) +
  scale_y_continuous(labels = label_percent(), 
                     limits = c(0, max(props$pctdeaths)*1.2), 
                     n.breaks = 10) + 
  labs(x = NULL, y = "Proportion of Deaths") + 
  theme(
    panel.grid.major.x = element_blank(), 
    legend.position = "none")

title <- expression(scriptstyle("Data collection ended November 7, 2020 - data shown as proportions"))
vis2 <- annotate_figure(vis2, top = text_grob(title))
vis2 <- annotate_figure(vis2, top = text_grob("Total Deaths (COVID-19 and Pneumonia) by Winning Candidate"))
vis2

ggsave("09_week/figures/illness_deaths_by_winner_proportion.png", vis2)

#visual 3: bar plot - total deaths by state by winner, w/ illness type
df <- df %>% pivot_longer(
  cols = `COVID-19`:Pneumonia,
  names_to = "death", 
  values_to = "count"
)
deaths_fill <- c("COVID-19" = "#8adb7a", "Pneumonia" = "#f49953")

vis3 <- ggplot(df, aes(x = count, y = reorder(state_po, totalillness), fill = death)) + 
  geom_col(position = "dodge", width = .6) + 
  facet_wrap(~winner, scales = "free_y") + 
  theme_bw() + 
  scale_x_continuous(label = comma) + 
  scale_fill_manual(values = deaths_fill) + 
  labs(
    x = "Total Count of Deaths", 
    y = NULL, 
    fill = "Cause of Death:") + 
  theme(panel.grid.major.y = element_blank(), 
        legend.position = "bottom")
title <- expression(scriptstyle("Data collection ended November 7, 2020 - data shown as raw counts"))
vis3 <- annotate_figure(vis3, top = text_grob(title))
vis3 <- annotate_figure(vis3, top = text_grob("Total Deaths (COVID-19 and Pneumonia) by Candidate and State"))
vis3
ggsave("09_week/figures/illness_deaths_faceted_by_winner.png", vis3)



