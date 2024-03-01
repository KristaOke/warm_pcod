#==================================================================================
#Get temperature data 

#by Krista, Feb 2024
#==================================================================================
#Notes:
#==================================================================================

library(tidyverse)
library(lubridate)

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

#pivot wider to match to other datasets

cfsr_jun_devs_wide <- cfsr_jun_devs %>% pivot_wider(names_from = size, values_from = jun_temp_dev, -c(Month, temp, mean_june_early_temp))

cfsr_jun_devs_wide$cfsr_jun_dev_0_20 <- cfsr_jun_devs_wide$X0.20
cfsr_jun_devs_wide$cfsr_jun_dev_20_40 <- cfsr_jun_devs_wide$X20.40
cfsr_jun_devs_wide$cfsr_jun_dev_40_60 <- cfsr_jun_devs_wide$X40.60
cfsr_jun_devs_wide$cfsr_jun_dev_60_80 <- cfsr_jun_devs_wide$X60.80
cfsr_jun_devs_wide$cfsr_jun_dev_80 <- cfsr_jun_devs_wide$X80.

cfsr_jun_devs_wide <- cfsr_jun_devs_wide[,c(1,7:11)]

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

#TIME SERIES
GAK1_dat <- read.csv("http://research.cfos.uaf.edu/gak1/data/TimeSeries/gak1.csv")
GAK1_dat <- GAK1_dat[-c(1:2),] #trim metadata
#this dataset includes multiple stations along seward line not just GAK1

GAK1_dat$Temp <- as.numeric(GAK1_dat$Temp)
GAK1_dat$Depth <- as.numeric(GAK1_dat$Depth)

#need to figure out what's going on with date/years

GAK1_dat$date <- format(date_decimal(as.numeric(GAK1_dat$Year)), "%d-%m-%Y")

GAK1_dat <- GAK1_dat %>% dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date))

GAK1_dat <- GAK1_dat %>% dplyr::mutate(day = lubridate::day(date),
                                       year = lubridate::year(date), 
                                       month = lubridate::month(date))
GAK1_dat <- GAK1_dat %>%
  separate(date, c("day", "month", "year"), "-")

GAK1_temp_st1 <- GAK1_dat[which(GAK1_dat$St==1),]

ggplot(GAK1_temp_st1[which(GAK1_temp_st1$Depth==0),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)

ggplot(GAK1_temp_st1[which(GAK1_temp_st1$Depth==30),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)
#very sparse!

ggplot(GAK1_temp_st1[which(GAK1_temp_st1$month=="06"),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~Depth)



#OK let's download the mooring data going to be painful
GAK1_dat <- read.csv("http://research.cfos.uaf.edu/gak1/data/Mooring/gak1_mooring_1998-1999.zip")





#grab csvs for SST and MHWI=============

#these are created in 'CREAT_TEMPANDHEAT.r'

MHWIdat <- read.csv(paste(wd, "/CENTRAL_GOA_HEATWAVE-main/", "MHWI.csv",sep=""))

MHWIdat$annual_MHWI <- MHWIdat$Annual
MHWIdat$summer_MHWI <- MHWIdat$Summer
MHWIdat$winter_MHWI <- MHWIdat$Winter
MHWIdat$spawning_MHWI <- MHWIdat$Spawning

MHWIdat <- MHWIdat[,c(1,6:9)]


seasonalmeanSST <- read.csv(paste(wd,  "/seasonalmeanSST.csv",sep=""))

#pivot wider to match to other datasets

sst_wide <- seasonalmeanSST %>% pivot_wider(names_from = season, values_from = seasonal_mean)

sst_wide$mean_SST_jun_aug_sep <- sst_wide$jun_aug_sep
sst_wide$mean_SST_oct_nov_dec <- sst_wide$oct_nov_dec
sst_wide$mean_SST_apr_may_jun <- sst_wide$apr_may_jun
sst_wide$mean_SST_jan_feb_mar <- sst_wide$jan_feb_mar

sst_wide <- sst_wide[,c(1,6:9)]


#join all datasets together======















