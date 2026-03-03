#import libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(scales)

#set up directory
dir.create("08_week/data")
dir.create("08_week/scripts")
dir.create("08_week/outputs")
dir.create("08_week/figures")
dir.create("08_week/problem_set")

#data loading + cleaning
df <- readRDS("08_week/data/conflict_data.rds")
dim(df)
names(df)
#var of interest: cowinteronset (counts years of a conflict - useful for proportion, etc)
#no obsv after 2008 has var of interest, remove na rows, initiator unnecessary
df_clean <- df %>% filter(!is.na(cowinterongoing)) %>% select(-c(initiator1, initiator2))

#year-system data set
#anocracy as mixed dyad - only interested in demo-auto, drop non-related cols (country name, trade, dist, etc)
df_year <- df_clean %>% mutate(
  regime_id = case_when(
    polity21 == "Democracy" & polity22 == "Democracy" ~ "Democracy", 
    polity21 == "Autocracy" & polity22 == "Autocracy" ~ "Autocracy", 
    (polity21 != "Autocracy" & polity22 == "Autocracy") | 
      (polity21 != "Democracy" & polity22 == "Democracy") | (polity21 == "Anocracy") | (polity22 == "Anocracy") ~ "Mixed"
  )
)
df_year <- df_year %>% group_by(year, regime_id) %>% mutate(
  war_regime = sum(cowinterongoing) 
)
df_year <- df_year %>% select(c(year, regime_id, war_regime)) %>% unique()
df_year <- df_year %>% group_by(year) %>% group_modify(~add_row(.x, regime_id = "Total", war_regime = sum(.x$war_regime)))

#dyad-year dataset
#remove columns with na vars of interest (dist, trade)
#parse down to unique country pairings - my computer keeps crashing
df_dyad <- df_clean %>% filter(!is.na(capdist)) %>%
  group_by(iso3c1, iso3c2) %>% mutate(
  capdist = sum(capdist)/n(),
  trade = ifelse(is.na(trade), 0, trade), 
  avg_trade = sum(trade)/n(), 
  war_sum = sum(cowinterongoing),
  war_fre = war_sum/n(),
) %>% select(c(iso3c1, iso3c2, war_sum, war_fre, capdist, avg_trade)) %>% unique()

#remove large dataframes
rm(df, df_clean)


#system-war trends: total num of wars per year AND wars between democracies
war <- c('Democracy' = "Democratic Regimes", 'Total' = "All Conflicts")
system_war <- ggplot(df_year[df_year$regime_id %in% c("Total", "Democracy"), ], aes(year, war_regime)) + 
  geom_line(linewidth = .75, color = '#373835') + 
  scale_y_continuous(limits = c(0,80), n.breaks = 8) +
  facet_wrap(~regime_id, labeller = as_labeller(war)) + 
  labs(
    caption = "Data from Correlates of War (COW)",
    x = "Year", 
    y = "Active Conflicts"
  ) + theme_light() + theme(strip.text = element_text(color = "#373835"), 
                            strip.background = element_rect(fill = "white", color = "#373835"), 
                            legend.position = "none")
title <- expression(scriptstyle("Globally; 1816 - 2007"))
system_war <- annotate_figure(system_war, top = text_grob(title))
system_war <- annotate_figure(system_war, top = text_grob("Conflicts Between Democratic Regimes Compared to Total Conflicts"))
system_war


#regime type comp: wars across regime
war <- c('Democracy' = "Democratic Regimes", 'Autocracy' = "Autocratic Regimes", 'Mixed' = "Mixed Regime Conflict")
regime_war <- ggplot(subset(df_year, regime_id != "Total"), mapping = aes(x = year, y = war_regime)) + 
  geom_line(linewidth = .75, color ='#373835') + 
  scale_y_continuous(limits = c(0, 80), n.breaks = 8) + 
  facet_grid(~regime_id, labeller = as_labeller(war)) +
  labs(
    caption = "Data from Correlates of War (COW)", 
    x = "Year", 
    y = "Active Conflicts", 
  ) + 
  theme_light() + 
  theme(strip.text = element_text(color = "#373835"), 
        strip.background = element_rect(fill = "white", color = "#373835"), 
        legend.position = "none")
regime_war <- annotate_figure(regime_war, top = text_grob(title))
regime_war <- annotate_figure(regime_war, top = text_grob("Global Conflicts By Regimes Involved"))
regime_war

#distance-war: war frequency by distance 
dist_war <- ggplot(df_dyad, mapping = aes(capdist, war_sum)) + 
  geom_point(aes(alpha = .3, color = cut(war_sum, c(-Inf, 0.001, Inf)))) + 
  labs(
    x = "Distance Between Capitals (km)", 
    y = "# of Conflict Years", 
    caption = "Data from Correlates of War (COW)"
  ) + scale_color_manual(values = c("(-Inf,0.001]" = "#373835",
                                    "(0.001, Inf]" = "#ea5b3f")) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 20000), n.breaks = 10, labels = comma) + 
  scale_y_continuous(limits = c(0, 15), n.breaks = 3)
title <- expression(scriptstyle("Global Dyads; 1816 - 2007"))
dist_war <- annotate_figure(dist_war, top = text_grob(title))
dist_war <- annotate_figure(dist_war, top = text_grob("Number of Conflict Years and Distance Between Capitals (km)"))
dist_war

#trade-dist-war
tdw <- ggplot(df_dyad, aes(capdist, avg_trade, size = war_sum)) + 
  geom_point(data = df_dyad[df_dyad$war_sum == 0, ], alpha = .5, color = "#373835") + 
  geom_point(data = df_dyad[df_dyad$war_sum > 0, ], alpha = .5, color = "#ea5b3f") + 
  labs(
    x = "Distance Between Captials (km)", 
    y = "Avg. Trade Volume (millions of $)", 
    size = "Conflict Years", 
    caption = "Data from Correlates of War (COW)"
  ) + theme_minimal() + 
  scale_y_continuous(limits = c(0, 100000), label = comma) + 
  scale_x_continuous(label = comma) + 
  theme(legend.position = "bottom")
title <- expression(scriptstyle("Global Dyads; 1816 - 2007"))
tdw <- annotate_figure(tdw, top = text_grob(title))
tdw <- annotate_figure(tdw, top = text_grob("Trade-Geography-Conflict Dynamics"))
tdw

#transformed axis vis - log y-axis, change alpha of war_sum > 0
tdw_trans <- ggplot(df_dyad, aes(capdist, avg_trade, size = war_sum)) + 
  geom_point(data = df_dyad[df_dyad$war_sum == 0, ], alpha = .15, color = "#373835") + 
  geom_point(data = df_dyad[df_dyad$war_sum > 0, ], alpha = .5, color = '#ea5b3f') +
  labs(
    x = "Distance Between Captials (km)", 
    y = "Avg. Trade Volume (millions of $)", 
    size = "Conflict Years", 
    caption = "y-axis = log(y + 1) "
  ) + theme_light() + 
  scale_y_continuous(label = comma, trans = "log1p", limits = c(0, 150000), breaks = c(0, 10, 100, 1000, 10000, 100000)) + 
  scale_x_continuous(label = comma) + theme(legend.position = "bottom")
tdw_trans <- annotate_figure(tdw_trans, top = text_grob(title))
tdw_trans <- annotate_figure(tdw_trans, top = text_grob("Trade-Geography-Conflict Dynamics: Transformed"))
tdw_trans

#bad vis. - poor color choice, incorrect allocation of data, difficult to read, misleading title
bad_vis <- ggplot(df_year, aes(year, war_regime, fill = regime_id)) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values = c('#ea5b3f', '#c6275c', '#8cea8c', '#ea8cbb')) + 
  labs(
    x = "Year", 
    y = "Active Conflicts", 
    fill = "Regime Type"
  ) + theme_light()
bad_vis <- annotate_figure(bad_vis, top = text_grob("Total Conflicts by Regime Type"))
bad_vis

#save visuals 
ggsave("08_week/figures/bad_vis.png", bad_vis)
ggsave("08_week/figures/dist_war.png", dist_war)
ggsave("08_week/figures/trade_dist_raw.png", tdw)
ggsave("08_week/figures/trade_dist_trans.png", tdw_trans)
ggsave("08_week/figures/regime_war.png", regime_war)
ggsave("08_week/figures/system_war.png", system_war)


#drafted visual - trans_tdw
tdw_trans_take1 <- ggplot(df_dyad, aes(distance, avg_trade, size = war_sum)) + 
  geom_point(data = df_dyad[df_dyad$war_sum == 0, ], alpha = .3, color = "#373835") + 
  geom_point(data = df_dyad[df_dyad$war_sum > 0, ], alpha = 1, color = "#ea5b3f") +
  facet_wrap(~cut(war_sum, breaks = c(0, .99, 15), include.lowest = TRUE, labels = c("No Conflicts", "Conflict Events"))) + 
  labs(
    x = "Distance Between Captials (km)", 
    y = "Avg. Trade Volume (millions of $)", 
    size = "Number of Conflicts", 
    caption = "y-axis = log(y + 1) "
  ) + theme_light() + 
  scale_y_continuous(label = comma, trans = "log1p", limits = c(0, 150000), breaks = c(0, 10, 100, 1000, 10000, 100000)) + 
  scale_x_continuous(label = comma) + theme(legend.position = "bottom", 
                                            strip.text = element_text(color = "#373835"), 
                                            strip.background = element_rect(fill = "white", color = "#373835"))
tdw_trans_take1 <- annotate_figure(tdw_trans_take1, top = text_grob(title))
tdw_trans_take1 <- annotate_figure(tdw_trans_take1, top = text_grob("Trade-Geography-Conflict Dynamics: Transformed"))
tdw_trans_take1
