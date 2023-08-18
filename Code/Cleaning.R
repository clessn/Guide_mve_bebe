# packages
library(dplyr)

# raw data
Data <- read.csv("_SharedFolder_Guide_mve/data/INSPQ-guide-mve_RAW.csv") %>% 
  slice(-c(1,2))

Cleandata <- data.frame(
 ID = c(1:nrow(Data))
)

