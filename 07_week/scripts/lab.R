#import libraries
library(dplyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(kableExtra)

#set up directory
dir.create("07_week/figures")
dir.create("07_week/outputs")
dir.create("07_week/scripts")

#check file paths
getwd()
list.files("07_week")
list.files("07_week/data")


#import data
df <- readRDS("07_week/data/battle_deaths.rds")

#check data
dim(df)
names(df)

#details of data
range(df$year)
list(unique(df$country))
list(unique(df$region))
list(unique(df$income))

#summary table
df2 <- df %>% select(battle_deaths)
sum <- as.data.frame(apply(df2, 2, summary)) %>% 
  kbl(caption = "Summary Statistics of Battle Deaths (1989 - 2023)", col.names = c("Stats", "Battle Deaths"), align = "l", format.args = list(big.mark = ",")) %>% 
  kable_styling(bootstrap_options = "striped")
sum

#binning decisions:
scott <- function(x) {
  n <- length(x)
  s <- sd(x)
  3.5 * s * n^(-1/3)
}

#transformation info: 
df <- df %>% mutate(
  trans_battle = battle_deaths + 1
)

#histogram - battledeaths - baseline + transformed
baseline_hist <- ggplot(df, aes(x = battle_deaths)) + 
  geom_histogram(fill = "darkred", binwidth = scott(df$battle_deaths)) + 
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(limits = c(0, 950), 
                     breaks = c(0, 250, 500, 750)) + 
  theme_minimal() + labs(
    caption = "Untransformed",
    y = "Count"
  )
transformed_hist <- ggplot(df, aes(x = battle_deaths)) + 
  geom_histogram(fill = "darkred", binwidth = scott(log(df$trans_battle))) + 
  scale_x_continuous(trans = "log1p", 
                     breaks = c(0, 1, 10, 100, 1000, 10000, 100000), 
                     labels = comma) + 
  scale_y_continuous(limits = c(0, 950),
                     breaks = c(0, 250, 500, 750)) +
  theme_minimal() + labs(
    caption = "Transformed Axis - log(x + 1)", 
    y = "Count"
  )

hist_battle <- ggarrange(baseline_hist, transformed_hist)
title <- expression(scriptstyle("Globally; 1989 - 2023"))
hist_battle <- annotate_figure(hist_battle, top = text_grob(title))
hist_battle <- annotate_figure(hist_battle, top = text_grob("Distribution of Battle-Related Deaths"))
hist_battle
ggsave("07_week/figures/battle_death_hist.png", hist_battle, width = 9, height = 5)

#battle death density plots - baseline + transformed
baseline_density <- ggplot(df, aes(x = battle_deaths)) + 
  geom_density(color = "darkred", fill = "darkred", alpha = .3, adjust = .01) + 
  theme_minimal() +
  labs(
    caption = "Untransformed", 
    y = "Density"
  ) + 
  scale_y_continuous(limits = c(0, 0.2), 
                     breaks = c(0, .05, .1, .15, .2), 
                     labels = comma) + 
  scale_x_continuous(labels = comma)
transformed_density <- ggplot(df, aes(x = battle_deaths)) + 
  geom_density(color = "darkred", fill = "darkred", alpha = .3, adjust = 1) + 
  theme_minimal() +
  labs(
    caption = "Transformed Axis - log(x + 1)",
    y = "Density"
  ) + 
  scale_y_continuous(limits = c(0, 0.2), breaks = c(0, .05, .1, .15, .2),labels = comma) + 
  scale_x_continuous(trans = "log1p", 
                     breaks = c(0, 1, 10, 100, 1000, 10000, 100000),
                     labels = comma)

density_battle <- ggarrange(baseline_density, transformed_density)
density_battle <- annotate_figure(density_battle, top = text_grob(title))
density_battle <- annotate_figure(density_battle, top = text_grob("Density Curve of Battle-Related Deaths"))
density_battle
ggsave("07_week/figures/battle_deaths_density.png", density_battle)

#q-q plots - baseline + transformed
qq_baseline <- ggplot(df, aes(sample = battle_deaths)) +
  stat_qq(color = "darkred", alpha = .3) + 
  stat_qq_line() + 
  labs(
    caption = "Untransformed", 
    x = "Theoretical Quantiles [Normal Distribution]", 
    y = "Sample Quantiles [Battle Deaths]"
  ) + 
  theme_minimal() + 
  scale_y_continuous(label = comma)
qq_trans <- ggplot(df, aes(sample = log(trans_battle))) + 
  stat_qq(color = "darkred", alpha = .3) + 
  stat_qq_line() + 
  theme_minimal() + 
  labs(
    caption = "Transformed Data - log(x + 1)", 
    x = "Theoretical Quantiles [Normal Distribution]", 
    y = "Sample Quantiles [log(Battle Deaths + 1)]"
  )

qq_battle <- ggarrange(qq_baseline, qq_trans)
qq_battle <- annotate_figure(qq_battle, top = text_grob(title))
qq_battle <- annotate_figure(qq_battle, top = text_grob("Q-Q Plot of Battle Deaths"))
qq_battle
ggsave("07_week/figures/battle_deaths_qq.png", qq_battle)

#histogram - faceted (region)
faceted_hist_region <- ggplot(df %>% filter(region != "North America"), aes(x = battle_deaths)) + 
  geom_histogram(binwidth = scott(log(df$trans_battle)), fill = "darkred") + 
  facet_wrap(~ region, scales = "free") + 
  scale_x_continuous(trans = "log1p", 
                     limits = c(NA, 180000),
                     breaks = c(0, 1, 10, 100, 1000, 10000, 100000), 
                     label = comma) +
  scale_y_continuous(limits = c(0, 75), breaks = c(0, 25, 50, 75)) + 
  labs(
    y = "Count", 
    caption = "Transformed axis - log(x + 1)"
  ) + theme_classic() + theme(legend.position = "none")
title <- expression(scriptstyle("1989 - 2023"))
faceted_hist_region <- annotate_figure(faceted_hist_region, top = text_grob(title))
faceted_hist_region <- annotate_figure(faceted_hist_region, top = text_grob("Distribution of Battle-Related Deaths by Region"))
faceted_hist_region
ggsave("07_week/figures/facet_region.png", faceted_hist_region, width = 10, height = 5)

#histogram - faceted (income)
faceted_hist_income <- ggplot(df %>% filter(income != "Not classified"), aes(x = battle_deaths)) + 
  geom_histogram(fill = "darkred", binwidth = scott(log(df$trans_battle))) + 
  facet_wrap(~ income, scales = "free") + 
  scale_x_continuous(trans = "log1p", 
                     limits = c(NA, 180000),
                     breaks = c(0, 1, 10, 100, 1000, 10000, 100000), 
                     label = comma) +
  scale_y_continuous(limits = c(0, 75), 
                    breaks = c(0, 25, 50, 75)) + 
  labs(
    y = "Count", 
    caption = "Transformed axis - log(x + 1)"
  ) + theme_classic() + theme(legend.position = "none")
faceted_hist_income <- annotate_figure(faceted_hist_income, top = text_grob(title))
faceted_hist_income <- annotate_figure(faceted_hist_income, top = text_grob("Distribution of Battle-Related Deaths by Income-Level"))
faceted_hist_income
ggsave("07_week/figures/facet_income.png", faceted_hist_income)
