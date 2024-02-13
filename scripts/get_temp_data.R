#==================================================================================
#Get temperature data 

#by Krista, Feb 2024
#==================================================================================
#Notes:
#==================================================================================

library(tidyverse)

#load data-------

wd <- getwd()
cfsrdat <- read.csv(paste(wd,"/data/cfsr_197901-202308.csv",sep=""))

cfsr_long <- pivot_longer(cfsrdat, !c(Year, Month), names_to="size", values_to = "temp")
ggplot(cfsr_long, aes(Year, temp, col=size)) + geom_point() + facet_wrap(~Month) + geom_line()





