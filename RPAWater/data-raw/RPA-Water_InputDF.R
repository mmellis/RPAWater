### Example script with input dataframe
library(dplyr)
library(tidyr)

# Input initial values dataframe
init<-read.csv('rawdata/RPAWaterInputs.csv')
   yr1<-unique(init$year); stopifnot(length(yr1)>1)
   
   
# Input drivers
   