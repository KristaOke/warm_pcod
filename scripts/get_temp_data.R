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

cfsr_an_mean <- cfsr_long %>% group_by(Year, size) %>%
  summarise(mean_ann_temp = mean(temp, na.rm=TRUE))


ggplot(cfsr_an_mean, aes(Year, mean_ann_temp, col=size)) + geom_point() +  geom_line()

#assessment uses June temps and size ranges 0-20 and 40-60, then compute the deviations from a mean for 1982-2012 
cfsr_june <- cfsr_long[which(cfsr_long$Month==6),]

cfsr_jun_early_mean <- cfsr_june[which(cfsr_june$Year<2013 & cfsr_june$Year>1981),] %>% group_by(size) %>%
  summarise(mean_june_early_temp = mean(temp, na.rm=TRUE))

cfsr_join <- left_join(cfsr_june, cfsr_jun_early_mean, by=c("size"))

cfsr_join$jun_temp_dev <- cfsr_join$temp - cfsr_join$mean_june_early_temp 

cfsr_jun_devs <- cfsr_join

ggplot(cfsr_jun_devs, aes(Year, jun_temp_dev, col=size)) + geom_line() + geom_point()

#ESP temp indicators----
wd <- getwd()
esp_dat <- read.csv(paste(wd,"/data/goa_pcod_BAS_indicators_2023.csv",sep=""))
esp_dat <- esp_dat[,1:10] #trim empty columns

esp_temps <- esp_dat[,c(1,3:5)]

esp_temps_long <- esp_temps %>% pivot_longer(!c(Year), names_to="indicator", values_to = "temp")

ggplot(esp_temps_long, aes(Year, temp)) + geom_point() + geom_line() + 
  facet_wrap(~indicator, scales="free")

#summer_temperature_bottom_GOA_Model looks nearly identical to 0-20 CFSR
#ESP says: "Summer bottom temperatures where small Pacific cod (0-20 cm) have been 
#sampled by the AFSC GOA bottom trawl survey from the CFSR dataset"

#GAK1====

GAK1_dat <- read.csv("http://research.cfos.uaf.edu/gak1/data/TimeSeries/gak1.csv")
GAK1_dat <- GAK1_dat[-c(1:2),] #trim metadata
#this dataset includes multiple stations along seward line not just GAK1

GAK_temp <- GAK1_dat[,c()]
