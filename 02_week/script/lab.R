#import libraries
library(ggplot2)
library(dplyr)

#create file directory for assignment
dir.create('plsc_498/02_week/outputs', showWarnings = FALSE)
dir.create('plsc_498/02_week/script', showWarnings = FALSE)
dir.create('plsc_498/02_week/figures', showWarnings = FALSE)
dir.create('plsc_498/02_week/data_processed', showWarnings = FALSE)

#open data + summary
vdem <- readRDS('plsc_498/02_week/data/vdem.rds')

dim(vdem) #rows: 27913, cols: 1818
colnames(vdem[1:3]) # "country_name"    "country_text_id" "country_id" 

#select four vars + summary
vdem <- vdem %>% select("v2clacjstw", "v2clacjstm", "v2clkill", "v2cltort")

names(vdem)
str(vdem[, c(1:4)])

#save to data_processed folder
write.csv(vdem, "plsc_498/02_week/data_processed/vdem_processed.csv")

#assignment plots
#baseline plot
p0 <- ggplot(vdem, aes(
  x = v2clacjstw, 
  y = v2clacjstm
)) + geom_point()

p0
ggsave("plsc_498/02_week/figures/plot_baseline.png", plot = p0)

#plot extensions
#plot 1, color = v2clkill
p1 <- ggplot(vdem, aes(
  x = v2clacjstw, 
  y = v2clacjstm, 
  color = v2clkill
)) + geom_point()
p1
ggsave("plsc_498/02_week/figures/plot_extension1.png", plot = p1)

#plot 2, color = v2clkill, size = v2cltort
p2 <- ggplot(vdem, aes(
  x = v2clacjstw, 
  y = v2clacjstm, 
  color = v2clkill, 
  size = v2cltort
)) + geom_point()
p2
ggsave("plsc_498/02_week/figures/plot_extension2.png", plot = p2)



