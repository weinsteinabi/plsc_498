#import libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(readr)
library(patchwork)


#import data
gap <- read.csv("14_week/data/gapminder.csv")
survey <- read.csv("14_week/data/survey_democracy.csv")

#file structure
dir.create("14_week/scripts")
dir.create("14_week/outputs")
dir.create("14_week/figures")

getwd()
list.files("14_week")
list.files("14_week/data")

#explore data
dim(gap) #rows: 1,704 columns: 6
names(gap) 
summary(gap$year) #years: 1952 - 2007

dim(survey) #rows: 8 columns: 6
names(survey)

#visualization 1: error bar
survey$country <- reorder(survey$country, survey$dem_support)

vis1 <- ggplot(survey, aes(x = dem_support, y = country)) + 
  geom_errorbar(aes(xmin = lower, xmax = upper), height = .25) + 
  geom_point(size = 2) + 
  theme_bw() + 
  scale_x_continuous(limits = c(30, 80), n.breaks = 10) + 
  labs(
    x = "Democratic Support", 
    y = ""
  )
vis1 <- annotate_figure(vis1, top = text_grob("Surveyed Support of Democracy in Eight Countries", size = 8))
vis1 <- annotate_figure(vis1, top = text_grob("Evaluating Attitudes Towards Democracy"))
ggsave("14_week/figures/democracy_support_ci.png", vis1)

#visualization 2: time series
gap_sum <- gap %>% group_by(continent, year) %>% summarize(
  mean_le = mean(lifeExp), 
  se_le = sd(lifeExp)/sqrt(n()), 
  lower = mean_le - (1.96*se_le), 
  upper = mean_le + (1.96*se_le),
  .groups = "drop"
)

vis2 <- ggplot(gap_sum, aes(x = year, color = continent, fill = continent)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .3, color = NA) + 
  geom_line(aes(y = mean_le), linewidth = .7) + 
  theme_bw() +
  scale_x_continuous(limits = c(1950, 2010), n.breaks = 15) + 
  scale_y_continuous(n.breaks = 10) + 
  theme(legend.position = "bottom") + 
  labs(
    x = "", 
    y = "Avg. Life Expectancy (in Years)", 
    color = "", 
    fill = "", 
    caption = "Data from {gapminder} in R, 2026"
  )

#visualization 3: rough version of vis2
vis3 <- ggplot(gap_sum, aes(x = year, color = continent, fill = continent)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .3, color = NA) + 
  geom_line(aes(y = mean_le), linewidth = .7) + 
  theme(legend.position = "none")
vis3 <- vis3 + vis2 + plot_annotation(
  title = "Average Life Expectancy by Global Region", 
  caption = "Before (Left) and After (Right)", 
  theme = theme(plot.title = element_text(hjust = .5, size = 10))
)

ggsave("14_week/figures/redesign_before_after.png", vis3)

vis2 <- annotate_figure(vis2, top = text_grob("1952 - 2007", size = 10))
vis2 <- annotate_figure(vis2, top = text_grob("Average Life Expectancy by Global Region"))
ggsave("14_week/figures/life_exp_ribbon_redesign.png", vis2)

#visualization 4: GDP and Life Expectancy
gap_2007 <- gap %>% filter(year == 2007)

vis4 <- ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(alpha = .4, aes(size = pop)) + 
  geom_smooth(method = "lm", se = TRUE, color = "darkred") + 
  scale_x_log10(limits = c(250, 65000), breaks = c(250, 500, 1000, 2000, 4000, 8000, 16000, 32000, 64000), labels = dollar) + 
  scale_size_continuous(labels = comma, guide = "none") + 
  scale_y_continuous(n.breaks = 10) +
  theme_classic() + 
  labs(
    x = "Gross Domestic Product per Capita (in USD)", 
    y = "Life Expectancy"
  )
vis4 <- annotate_figure(vis4, top = text_grob("2007, Global", size = 10))
vis4 <- annotate_figure(vis4, top = text_grob("Gross Domestic Product per Capita and Life Expectancy"))
ggsave("14_week/figures/gdp_lifeexp_lm_se.png", vis4)
