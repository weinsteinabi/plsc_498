##############################################
#Script: Week 4 Assignment - Senate Ideologies
#Description: creates plots of senator idea. 
#   from four separate congresses. ideology
#   is plotted by color and position.
############################################
#import libraries 
library(ggplot2)
library(dplyr)

#set seed
set.seed(123)

#get directory
getwd()

#set up files
dir.create("04_week/outputs")
dir.create("04_week/figures")
dir.create("04_week/scripts")

#list files
list.files("04_week")
list.files("04_week/data")
#############################
#import data
df <- read.csv("04_week/data/Sall_members.csv")

#explore and check
dim(df)
names(df)
head(df)

#filter data
con <- c(101, 106, 111, 116)
df4 <- df %>% filter(chamber == "Senate" & congress %in% con)

summary(df4$nominate_dim1)
summary(df4$nominate_dim2)

table(df4$congress)

###########################
#create figures
#101st congress
plt101 <- ggplot(df4 %>% filter(congress == 101), 
            aes(x = nominate_dim1, y = nominate_dim2, color = nominate_dim1)) +
  geom_point(size = 2.5) + 
  theme_minimal() + 
  scale_color_gradient2(
    low = "darkblue",
    mid = "gray",
    high = "darkred", 
    limits = c(-.8,.8), 
    breaks = c(-.5, 0, .5)
    ) + 
  labs(
    title = "Senate Ideology - 101st Congress", 
    color = "DW-NOMINATE Dim. 1 (Liberal to Conservative)", 
    x = "DW-NOMINATE Dim. 1",
    y = "DW-NOMINATE Dim. 2"
 ) + theme(legend.position = "bottom") + 
  scale_x_continuous(limits = c(-.8, .8)) + 
  scale_y_continuous(limits = c(-1, 1))


plt101
ggsave("04_week/figures/senate_101.png", plt101)

#106th congress
plt106 <- ggplot(df4 %>% filter(congress == 106), 
                 aes(x = nominate_dim1, y = nominate_dim2, color = nominate_dim1)) +
  geom_point(size = 2.5) + 
  theme_minimal() + 
  scale_color_gradient2(
    low = "darkblue",
    mid = "gray",
    high = "darkred", 
    limits = c(-.8,.8), 
    breaks = c(-.5, 0, .5)
  ) + 
  labs(
    title = "Senate Ideology - 106th Congress", 
    color = "DW-NOMINATE Dim. 1 (Liberal to Conservative)", 
    x = "DW-NOMINATE Dim. 1",
    y = "DW-NOMINATE Dim. 2"
  ) + theme(legend.position = "bottom") + 
  scale_x_continuous(limits = c(-.8, .8))  + 
  scale_y_continuous(limits = c(-1, 1)) 
  
plt106
ggsave("04_week/figures/senate_106.png", plt106)

#111th congress
plt111 <- ggplot(df4 %>% filter(congress == 111), 
                 aes(x = nominate_dim1, y = nominate_dim2, color = nominate_dim1)) +
  geom_point(size = 2.5) + 
  theme_minimal() + 
  scale_color_gradient2(
    low = "darkblue",
    mid = "gray",
    high = "darkred", 
    limits = c(-.8,.8), 
    breaks = c(-.5, 0, .5)
  ) + 
  labs(
    title = "Senate Ideology - 111th Congress", 
    color = "DW-NOMINATE Dim. 1 (Liberal to Conservative)", 
    x = "DW-NOMINATE Dim. 1",
    y = "DW-NOMINATE Dim. 2"
  ) + theme(legend.position = "bottom") + 
  scale_x_continuous(limits = c(-.8, .8)) +  
  scale_y_continuous(limits = c(-1, 1))
plt111
ggsave("04_week/figures/senate_111.png", plt111)

#116th congress
plt116 <- ggplot(df4 %>% filter(congress == 116), 
                 aes(x = nominate_dim1, y = nominate_dim2, color = nominate_dim1)) +
  geom_point(size = 2.5)+ 
  theme_minimal() + 
  scale_color_gradient2(
    low = "darkblue",
    mid = "gray",
    high = "darkred", 
    limits = c(-.8,.8), 
    breaks = c(-.5, 0, .5)
  )+ 
  labs(
    title = "Senate Ideology - 116th Congress", 
    color = "DW-NOMINATE Dim. 1 (Liberal to Conservative)", 
    x = "DW-NOMINATE Dim. 1",
    y = "DW-NOMINATE Dim. 2"
  ) + theme(legend.position = "bottom") + 
  scale_x_continuous(limits = c(-.8, .8)) + 
  scale_y_continuous(limits = c(-1, 1))
plt116
ggsave("04_week/figures/senate_116.png", plt116)





