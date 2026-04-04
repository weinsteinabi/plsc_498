#import libraries
library(ggplot2)
library(dplyr)
library(scales)
library(ggpubr)
library(tidyr)
library(patchwork)
#add labels + titles
#make pretty :)

#set up files
dir.create("11_week/scripts")
dir.create("11_week/figures")
dir.create("11_week/outputs")

#check directory and files
getwd()
list.files("11_week")
list.files("11_week/data")

#import data
basketball <- readRDS("11_week/data/basketball.rds")

#clean data
basketball_clean <- basketball
basketball_clean <- basketball_clean %>% mutate(
  across(c(AGE, PLAYER_HEIGHT_INCHES, PLAYER_WEIGHT, GP:AST_PCT), as.numeric), 
  DRAFT_STATUS = ifelse(DRAFT_YEAR != "Undrafted", "Drafted", "Undrafted"))
basketball_clean <- basketball_clean %>% drop_na(c(AGE, PTS, REB, USG_PCT, TS_PCT, AST, AST_PCT,PLAYER_WEIGHT))
write.csv(basketball_clean, "11_week/data/basketball_clean.csv")
#check data
dim(basketball)
dim(basketball_clean)

basketball_clean %>% select(c(AGE, PTS, REB, USG_PCT)) %>% summary()

#visual 1.1: usage and scoring scatterplot
vis1_1 <- ggplot(basketball_clean, aes(USG_PCT, PTS)) + geom_point(alpha = .5) + theme_classic() + 
  scale_x_continuous(labels = percent, n.breaks = 10) + scale_y_continuous(limits = c(0,40), n.breaks = 10) + labs(
    x = "Usage Rate (%)", 
    y = "Points per Game"
  )
title <- "No Line of Fit"
vis1_1 <- annotate_figure(vis1_1, top = text_grob(title))
vis1_1 <- annotate_figure(vis1_1, top = text_grob("NBA Usage Rate and Points Scored, 2024-25 Season"))
vis1_1
ggsave("11_week/figures/usg_pts_scatter.png", vis1_1)

#visual 1.2: add linear smoother
vis1_2 <- ggplot(basketball_clean, aes(USG_PCT, PTS)) + geom_point(alpha = .5) + theme_classic() + 
  scale_x_continuous(labels = percent, n.breaks = 10) + scale_y_continuous(limits = c(0, 40), n.breaks = 10) + labs(
    x = "Usage Rate (%)", 
    y = "Points per Game") + geom_smooth(method = "lm", se = TRUE, color = "red")
title <- "w/ Line of Fit"
vis1_2 <- annotate_figure(vis1_2, top = text_grob(title))
vis1_2 <- annotate_figure(vis1_2, top = text_grob("NBA Usage Rate and Points Scored, 2024-25 Season"))
vis1_2 <- vis1_1 + vis1_2
vis1_2
ggsave("11_week/figures/usg_pts_lm_se.png", vis1_2)


#visual 1.3: usage and scoring efficiency scatterplot
vis1_3 <- ggplot(basketball_clean, aes(USG_PCT, TS_PCT)) + geom_point(alpha = .5) + theme_classic() + 
  scale_x_continuous(labels = percent, n.breaks = 10) + scale_y_continuous(labels = percent, n.breaks = 10) + 
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(
    x = "Usage Rate (%)", 
    y = "True Shooting Percentage (%)"
  )
title <- "2024-25 Season"
vis1_3 <- annotate_figure(vis1_3, top = text_grob(title))
vis1_3 <- annotate_figure(vis1_3, top = text_grob("NBA Usage Rate and True Shooting Percentage"))
vis1_3
ggsave("11_week/figures/usg_ts_eff_lm_se.png", vis1_3)

#visual 1.4: assists raw vs rate scatterplot
vis1_4 <- ggplot(basketball_clean, aes(AST, AST_PCT)) + geom_point(alpha = .5) + theme_classic() + 
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  scale_y_continuous(limits = c(0, .60), n.breaks = 10, labels = percent) + geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(
    x = "Assists per Game", 
    y = "Assist Percentage (%)"
  )
vis1_4 <- annotate_figure(vis1_4, top = text_grob(title))
vis1_4 <- annotate_figure(vis1_4, top = text_grob("Assists and Assist Percentages in the NBA"))
vis1_4
ggsave("11_week/figures/ast_astpct_lm_se.png", vis1_4)

#visual 2.1: player size and rebounds
vis2_1 <- ggplot(basketball_clean, aes(PLAYER_WEIGHT, REB)) + geom_point(alpha = .5) + theme_classic() +
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(
    x = "Player Weight (in lbs)" , 
    y = "Rebounds per Game"
  ) + 
  scale_x_continuous(limits = c(150, 325), n.breaks = 8) + 
  scale_y_continuous(limits = c(0, 15))
vis2_1 <- annotate_figure(vis2_1, top = text_grob(title))
vis2_1 <- annotate_figure(vis2_1, top = text_grob("NBA Player Size and Rebounds per Game"))
vis2_1
ggsave("11_week/figures/size_reb_scatter.png", vis2_1)

#visual 3.1: player age and scoring efficiency
basketball_subset <- basketball_clean %>% filter(TS_PCT > 0 & TS_PCT < 1)
vis3_1 <- ggplot(basketball_subset, aes(AGE, TS_PCT)) + geom_point(alpha = .5) + theme_classic() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "red") + 
  scale_y_continuous(labels = percent, limits = c(0, 1)) + 
  scale_x_continuous(limits = c(18, 40), n.breaks = 15) +
  labs(
    x = "Player Age", 
    y = "True Shooting Percentage (%)", 
    caption = "Outliers removed"
  )
vis3_1 <- annotate_figure(vis3_1, top = text_grob(title))
vis3_1 <- annotate_figure(vis3_1, top = text_grob("NBA Player Age and True Shooting Percent (%)"))
vis3_1
ggsave("11_week/figures/age_ts_poly2.png", vis3_1)
