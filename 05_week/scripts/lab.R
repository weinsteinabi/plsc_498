##
#
################################################################################
##data prep
#import libraries
library(ggplot2)
library(dplyr)
library(stats)
library(stringr)
library(scales)

#file directory
dir.create("05_week/scripts")
dir.create("05_week/figures")
dir.create("05_week/outputs")

#check files
getwd()
list.files("05_week")
list.files("05_week/data")

#load and check data
df <- read.csv("05_week/data/IMDB_Movies_Data.csv")
dim(df)
names(df)

#check variables
summary(df$budget)
range(df$budget)
summary(df$revenue)
range(df$revenue)

################################################################################
##histograms of budget and revenue
#set formula - scott's rule
scott <- function(x) {
  n <- length(x)
  s <- sd(x)
  3.5 * s * n^(-1/3)
}
#set formula - freedman-diaconis
freedman <- function(x){
  n <- length(x)
  iqr <- IQR(x)
  2 * iqr * n^(-1/3)
  
}
#set formula - sturge's rule
sturges <- function(x){
  n <- length(x)
  ceiling(log2(n) + 1)
}
#revenue histogram - default
rev_hist <- ggplot(df, aes(x = revenue)) + geom_histogram()
rev_hist

#revenue histogram - scott's rule
rev_hist_scott <- ggplot(df, aes(x = revenue)) + geom_histogram(binwidth = scott(df$revenue))
rev_hist_scott

#revenue histogram - freedman-diaconis
rev_hist_free <- ggplot(df, aes(x = revenue)) + geom_histogram(binwidth = freedman(df$revenue))
rev_hist_free

#revenue histogram - sturge's rule
rev_hist_sturge <- ggplot(df, aes(x = revenue)) + geom_histogram(bins = sturges(df$revenue))
rev_hist_sturge

#budget histogram - default
budg_hist <- ggplot(df, aes(x = budget)) + geom_histogram()
budg_hist

#budget histogram - scott's rule
budg_hist_scott <- ggplot(df, aes(x = budget)) + geom_histogram(binwidth = scott(df$budget))
budg_hist_scott

#budget histogram = freedman-diaconis
budg_hist_free <- ggplot(df, aes(x = budget)) + geom_histogram(binwidth = freedman(df$budget))
budg_hist_free

#budget histogram - sturge's rule
budg_hist_sturge <- ggplot(df, aes(x = budget)) + geom_histogram(bins = sturges(df$budget))
budg_hist_sturge

#final histogram - revenue
#transform data
df <- df %>% mutate(
  revenue2 = revenue/1000000,
  budget2 = budget/1000000
)
rev_hist <- ggplot(df, aes(x = revenue2)) + geom_histogram(bins = sturges(df$revenue2), color = "lightgrey", fill = "lightblue") + 
  labs(
    title = "Histogram of Movie Revenue (in millions of USD)", 
    x = "Revenue (in millions of USD)", 
    y = "Count"
  ) + theme_minimal() + scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), minor_breaks = NULL,  labels = dollar) 
rev_hist
ggsave("05_week/figures/revenue_hist.png", rev_hist)

#final histogram - budget
budg_hist <- ggplot(df, aes(x = budget2)) + geom_histogram(bins = sturges(df$budget2), color = "lightgrey", fill = "#6cc19e") + 
  labs(
    title = "Histogram of Movie Budgets (in millions of USD)", 
    x = "Budget (in millions of USD)", 
    y = "Count"
  ) + theme_minimal() + scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350), minor_breaks = NULL, labels = dollar)
budg_hist
ggsave("05_week/figures/budget_hist.png", budg_hist)

#clean space
rm(rev_hist_free, rev_hist_scott, rev_hist_sturge, budg_hist_sturge, budg_hist_free, budg_hist_scott)
################################################################################
##top-grossing directors dotplot
top_directors <- df %>% group_by(director) %>% summarize(
  total_revenue = sum(revenue)
) %>% arrange(desc(total_revenue)) %>% slice_head(n = 3)

df_top <- df %>% filter(director %in% top_directors$director)

#dot plot 
dir_dot <- ggplot(df_top, aes(x = revenue2, y = director, color = director, fill = director)) + geom_dotplot() +
  labs(
    title = "Top Grossing Directors by Revenue",
    x = "Revenue (in millions of USD)", 
    y = ""
  ) + theme_minimal() + 
  theme(legend.position = "none") + 
  scale_x_continuous(limit = c(500, 1000), breaks = c(500, 600, 700, 800, 900, 1000), labels = dollar) + 
  scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2")
dir_dot
ggsave("05_week/figures/revenue_by_director.png", dir_dot)
################################################################################
##budget revenue scatterplot
#include only primary genre
df <- df %>% mutate(
  genres = gsub(",.*", "", genres)
)
#plot 
budg_rev <- ggplot(df,aes(x = budget2, y = revenue2, color = genres, size = popularity)) + 
  geom_point() + labs(
    title = "Movie Budget and Revenue by Popularity and Genre", 
    x = "Budget (in millions of USD)", 
    y = "Revenue (in millions of USD)", 
    color = "Movie Genre", 
    size = "Movie Popularity"
  ) + 
  theme_minimal() + 
  scale_x_continuous(labels = dollar) + scale_y_continuous(labels = dollar) + 
  scale_color_brewer(palette = "Paired")
budg_rev

ggsave("05_week/figures/budget_revenue_scatter.png", budg_rev)

