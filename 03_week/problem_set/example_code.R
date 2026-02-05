library(plyr)
library(dplyr)
library(ggplot2)


#df <- read.csv("data/HOUSE_precinct_general.csv")

## Format data for plotting
df <- df %>% 
  filter(
    stage == "GEN",
    party_simplified %in% c("DEMOCRAT", "REPUBLICAN"),
    !is.na(votes)
  )

## Aggregate to the county level
county_data <- df %>%
  mutate(county_id = paste0(county_fips, "-", state)) %>% 
  group_by(county_id) %>%
  summarize(
    dem_votes = sum(votes[party_simplified == "DEMOCRAT"], na.rm = T),
    rep_votes = sum(votes[party_simplified == "REPUBLICAN"], na.rm = T)
  ) %>% ungroup(.) %>% 
  mutate(
    county_total_votes = dem_votes + rep_votes, 
    rep_share = rep_votes / (county_total_votes)
  )


## Plot 1
p1 <- ggplot(county_data, aes(x = county_total_votes, y = rep_share, col = rep_share)) + 
  geom_point()

## Plot 2
p2 <- ggplot(county_data, aes(x = county_total_votes, y = rep_share, col = rep_share)) + 
  geom_point(size = 2, alpha = I(2/3)) + 
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) + 
  theme_bw() +
  scale_colour_continuous(palette = c("blue", "gray", "red")) + 
  labs(x = "County vote total", y = "Republican vote share", title = "Vote count by Partisanship") + 
  theme(legend.position = "none") 
dir.create("03_week/problem_set/figures", recursive = T, showWarnings = F)

ggsave("03_week/problem_set/figures/plot_raw.png", p1, units = "in", width = 8, height = 6)
ggsave("03_week/problem_set/figures/plot_scaled.png", p2, units = "in", width = 8, height = 6)
