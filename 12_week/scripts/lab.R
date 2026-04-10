#import libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(scales)
library(patchwork)

#to-do: 
# fix scaling
# titles
# colors
# merge 2 and 3

#create file directory
dir.create("12_week/scripts")
dir.create("12_week/outputs")
dir.create("12_week/figures")

#import data
economics <- read.csv("12_week/data/economics.csv")
presidential <- read.csv("12_week/data/presidential.csv")
gapminder <- read.csv("12_week/data/gapminder.csv")

#explore data
dim(economics)
names(economics)
economics %>% select(c(date, unemploy)) %>% summary(economics)

dim(presidential)
names(presidential)

dim(gapminder)
names(gapminder)
gapminder %>% select(c(year)) %>% summary(gapminder)

#transform data
economics <- economics %>% mutate(
  date = as.Date(date)
)
presidential <- presidential %>% mutate(
  start = as.Date(start), 
  end = as.Date(end)
)


#visual 1: unemployment over time
vis1 <- ggplot(economics, aes(date, unemploy)) + geom_line() + 
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", limits = as.Date(c("1963-01-01", "2015-12-31"))) + 
  theme_classic() + 
  labs(
    x = "Year", 
    y = "Unemployment (in thousands)"
  ) + 
  scale_y_continuous(labels = comma, limits = c(0, 17500), n.breaks = 8)
title <- "1967 to 2015"
vis1 <- annotate_figure(vis1, top = text_grob(title, size = 10))
vis1 <- annotate_figure(vis1, top = text_grob("United States Unemployment over Time"))
ggsave("12_week/figures/unemployment_line.png", vis1)
vis1

#visual 2: unemployment over time - rolling mean
economics <- economics %>% mutate(
  rolling_mean = zoo::rollmean(unemploy, k = 12, fill = NA, align = "right")
)

vis2 <- ggplot(economics, aes(date)) + geom_line(aes(y = unemploy), alpha = .3) + 
  geom_line(aes(y = rolling_mean), color = "darkred") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", limits = as.Date(c("1963-01-01", "2015-12-31"))) + 
  theme_classic() + 
  labs(
    x = "Year", 
    y = "Unemployment (in thousands)"
  ) + 
  scale_y_continuous(labels = comma, limits = c(0, 17500), n.breaks = 8)
vis2 <- annotate_figure(vis2, top = text_grob("12-Month Rolling Mean", size = 10))

#visual 3: unemployment over time - loess
vis3 <- ggplot(economics, aes(date, unemploy)) + geom_line(alpha = .3) + 
  geom_smooth(method = "loess", color = "darkred", se = FALSE, linewidth = .5) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", limits = as.Date(c("1963-01-01", "2015-12-31"))) + 
  theme_classic() + 
  labs(
    x = "Year", 
    y = ""
  ) + 
  scale_y_continuous(labels = comma, limits = c(0, 17500), n.breaks = 8)
vis3 <- annotate_figure(vis3, top = text_grob("LOESS Regression", size = 10))
vis3 <- vis2 + vis3 + plot_annotation(title = "United States Unemployment over Time", 
                                      caption = "1967 to 2015", 
                                      theme = theme(plot.title = element_text(hjust = .5),
                                                    plot.caption = element_text(hjust = .5, vjust = 155) ))
ggsave("12_week/figures/unemployment_smoothed.png", vis3)

#visual 4: add pres terms
vis4 <- ggplot(economics, aes(date, unemploy)) +  geom_line() + 
  geom_rect(data = presidential, aes(xmin = start, xmax = end, fill = party), 
            ymin = -Inf, ymax = Inf,  alpha = .1, inherit.aes = FALSE) +
  scale_fill_manual(values = c(Democratic = "blue", Republican = "red")) + 
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", limits = as.Date(c("1963-01-01", "2017-01-21"))) + 
  theme_classic() + 
  labs(
    x = "Year", 
    y = "Unemployment (in thousands)", 
    fill = "Presidential Party"
  ) + 
  scale_y_continuous(labels = comma, limits = c(0, 17500), n.breaks = 8) + 
  theme(
    legend.position = "bottom"
  )
vis4 <- annotate_figure(vis4, top = text_grob(title, size = 10))
vis4 <- annotate_figure(vis4, top = text_grob("United States Unemployment over Time"))
ggsave("12_week/figures/unemployment_presidents.png", vis4)

#visual 5: gdp per cap by country
gap_sub <- gapminder %>% filter(country %in% c("United States", "Nigeria", "China", "Brazil"))
vis5 <- ggplot(gap_sub, aes(x = year, y = gdpPercap, color = country)) +
  geom_line() +
  geom_point(size = 1) +
  scale_y_continuous(limits = c(0, 45000), n.breaks = 5, labels = comma) + 
  facet_wrap(~ country, scales = "free") +
  theme_classic() + 
  labs(
    y = "GDP per Capita (USD)", 
    x = "Year"
  ) + theme(
    strip.background = element_rect(fill = "#dbcda2"), 
    legend.position = "none"
  )
vis5 <- annotate_figure(vis5, top = text_grob("Gross Domestic Product over Time"))
ggsave("12_week/figures/gdp_faceted.png", vis5)

#visual 6: optional...