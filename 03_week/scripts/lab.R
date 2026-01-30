#import libraries
library(ggplot2)
library(dplyr)
library(scales)

set.seed(123)

#set up files
dir.create("03_week/scripts")
dir.create("03_week/outputs")
dir.create("03_week/figures")

#check file paths 
getwd() #"C:/Users/abiwe/OneDrive - The Pennsylvania State University/
#PLSC - Political Science/PLSC 498.1 - Visualizing Social Data/plsc_498"
list.files("03_week/data") #"codebook.md" "HOUSE_precinct_general.csv" "README.md"
list.files("03_week") #"data""figures""outputs""problem_set""scripts"

#import data
df <- read.csv("03_week/data/HOUSE_precinct_general.csv")

#check data
dim(df)
names(df)
head(df)

#filter out data
df <- df %>% filter(stage == "GEN", 
                    party_simplified == "DEMOCRAT" | party_simplified == "REPUBLICAN", 
                    votes != -1, jurisdiction_name != "{STATISTICAL ADJUSTMENTS}")

#create new variables
juris_df <- df %>% group_by(county_name, state) %>% summarize(
  dem_votes = sum(votes[party_simplified == "DEMOCRAT"], na.rm = TRUE),
  rep_votes = sum(votes[party_simplified == "REPUBLICAN"], na.rm = TRUE) 
) %>% mutate(
  county_total_votes = dem_votes + rep_votes, 
  rep_share = rep_votes/county_total_votes
)

juris_df[1,1] = "A1"
juris_df[2,1] = "R1"

write.csv(juris_df, file = "03_week/data/cleanHouse.csv")

#check data
nrow(juris_df)
summary(juris_df$county_total_votes)
summary(juris_df$rep_share)

#vis 1
plt1 <- ggplot(juris_df, aes(x = county_total_votes, y = rep_share, color = rep_share)) + geom_point() +
  labs(
    title = "Republican Vote Share (%) by County", 
    subtitle = "2018 House Mid-Terms, General Election",
    x = "Total Votes", 
    y = "Republican Vote Share (%)", 
    color = "Rep. Vote Share (%)"
  ) +
  scale_color_gradient(low = "blue", high = "red")
plt1
ggsave(plt1, file = "03_week/figures/plot_raw.png")

#vis 2
plt2 <- ggplot(juris_df, aes(x = county_total_votes, y = rep_share, color = rep_share)) + geom_point() +
  labs(
    title = "Republican Vote Share (%) by County", 
    subtitle = "2018 House Mid-Terms, General Election",
    x = "Total Votes [log_10()]", 
    y = "Republican Vote Share (%)", 
    color = "Rep. Vote Share (%)"
  ) + scale_y_continuous(label = percent) + 
  scale_x_log10(labels = comma, ~ format(.x, scientific = FALSE)) + 
  scale_color_gradient(low = "blue", high = "red", label = percent)
plt2
ggsave(plt2, file = "03_week/figures/plot_scaled.png")
