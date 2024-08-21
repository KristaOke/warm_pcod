#==================================================================================
#Get temperature data 

#by Krista, Feb 2024
#==================================================================================
#Notes:
#==================================================================================

library(tidyverse)
library(lubridate)

#load data-------

#CFSR-------

wd <- getwd()
cfsrdat <- read.csv(paste(wd,"/data/cfsr_197901-202308.csv",sep=""))

cfsr_long <- pivot_longer(cfsrdat, !c(Year, Month), names_to="size", values_to = "temp")
ggplot(cfsr_long, aes(Year, temp, col=size)) + geom_point() + facet_wrap(~Month) + geom_line()

cfsr_an_mean <- cfsr_long %>% group_by(Year, size) %>%
  summarise(mean_ann_temp = mean(temp, na.rm=TRUE))


ggplot(cfsr_an_mean, aes(Year, mean_ann_temp, col=size)) + geom_point() +  geom_line()

#assessment uses June temps and size ranges 0-20 and 40-60, then compute the deviations from a mean for 1982-2012 
cfsr_june <- cfsr_long[which(cfsr_long$Month==6),]

cfsr_june_0_20 <- cfsr_june[which(cfsr_june$size=="X0.20"),]
write.csv(cfsr_june_0_20, "data/june_cfsr_raw_temp_mean.csv")

write.csv(cfsr_june, "data/june_cfsr_raw_temp_mean_all_sizes.csv")

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
#

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

#GAK1_temp_st1 <- GAK1_dat[which(GAK1_dat$St==1),]

ggplot(GAK1_temp_st1[which(GAK1_temp_st1$Depth==0),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)

ggplot(GAK1_dat[which(GAK1_dat$Depth==0),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)


ggplot(GAK1_temp_st1[which(GAK1_temp_st1$Depth==30),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)
#very sparse!

ggplot(GAK1_dat[which(GAK1_dat$Depth==30),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)

ggplot(GAK1_dat[which(GAK1_dat$Depth==50),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)

ggplot(GAK1_dat[which(GAK1_dat$Depth==100),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)

ggplot(GAK1_dat[which(GAK1_dat$Depth==150),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)

ggplot(GAK1_temp_st1[which(GAK1_temp_st1$month=="06"),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~Depth)



#GAK1 mooring data is downloaded, open each file

tempdat <-read.csv("data/GAK1/gak1_mooring_1998-1999/UAF_GAK1_1998_ctd027m_L2_v2.csv")
colnames<-colnames(tempdat)
gak_dat <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(gak_dat) <- c("Station", "Type", "Longitude_.decimal_degrees_east.", "Latitude_.decimal_degrees_north.",
                       "Bot..Depth..m.","Date_Time_.UTC.", "Depth_.m." , "Temperature_.C.",
                       "flag", "Date", "year", "month")
#gak_dat <- gak_dat %>% dplyr::select(-Conductivity_.S.m.)

gak_dat$Station <- as.character(gak_dat$Station)
gak_dat$Type <- as.character(gak_dat$Type)
gak_dat$Longitude_.decimal_degrees_east. <- as.numeric(gak_dat$Longitude_.decimal_degrees_east.)
gak_dat$Latitude_.decimal_degrees_north. <- as.numeric(gak_dat$Latitude_.decimal_degrees_north.)
gak_dat$Bot..Depth..m. <- as.numeric(gak_dat$Bot..Depth..m.)
gak_dat$Date_Time_.UTC. <- as.character(gak_dat$Date_Time_.UTC.)
gak_dat$Depth_.m. <- as.numeric(gak_dat$Depth_.m.)
gak_dat$Temperature_.C. <- as.numeric(gak_dat$Temperature_.C.)
gak_dat$flag <- as.integer(gak_dat$flag)
gak_dat$Date <- as.Date(gak_dat$Date)
gak_dat$year <- as.numeric(gak_dat$year)
gak_dat$month <- as.numeric(gak_dat$month)

#loop will work for MOST years but not all
gak_years <- c(1998:1999, 2004:2018)


#CRAZY slow b/c obs are so frequent, let's just use june for now
g<-1
for(g in 1:length(gak_years)) {
  tempyear <- gak_years[g]
  folder_name <- paste0("gak1_mooring_", tempyear, "-", (tempyear+1), sep="")
  file_names <- list.files(paste0("data/GAK1/", folder_name))
  for(j in 1:length(file_names)){
    tempname <- file_names[j]
    tempdat <- read.csv(file = paste0("data/GAK1/", folder_name, "/", tempname))
    tempdat <- tempdat %>%
      mutate(Date = as.Date(Date_Time_.UTC.), 
             year = year(Date_Time_.UTC.),
             month = month(Date_Time_.UTC.))
   # tempdat <- tempdat[which(tempdat$month==6),]
    gak_dat <- dplyr::bind_rows(gak_dat, tempdat)
  }
  print(tempyear)
}
#saveRDS(gak_dat, file="data/GAK1/june_mooring_data.RDS")
saveRDS(gak_dat, file="data/GAK1/allmonths_mooring_data.RDS")

#Other naughty years
#loop will work for MOST years but not all

gak_years2 <- c(2000:2002)
g<-1
for(g in 1:length(gak_years2)) {
  tempyear <- gak_years2[g]
  if (tempyear==2000) {
  folder_name <- paste0("gak1_mooring_", tempyear, "-", (tempyear+2), sep="")
  } 
  if (tempyear==2002) {
    folder_name <- paste0("gak1_mooring_", tempyear, "-", (tempyear+1), "_corrected", sep="")
  }
  file_names <- list.files(paste0("data/GAK1/", folder_name))
  for(j in 1:length(file_names)){
    tempname <- file_names[j]
    tempdat <- read.csv(file = paste0("data/GAK1/", folder_name, "/", tempname))
    tempdat <- tempdat %>%
      mutate(Date = as.Date(Date_Time_.UTC.), 
             year = year(Date_Time_.UTC.),
             month = month(Date_Time_.UTC.))
  #  tempdat <- tempdat[which(tempdat$month==6),]
    gak_dat <- dplyr::bind_rows(gak_dat, tempdat)
  }
  print(tempyear)
}
# saveRDS(gak_dat, file="data/GAK1/june_mooring_data_w00-03.RDS")
# gak_dat <- readRDS(file="data/GAK1/june_mooring_data_w00-03.RDS")

saveRDS(gak_dat, file="data/GAK1/allmonths_mooring_data_w00-03.RDS")
gak_dat <- readRDS(file="data/GAK1/allmpnths_mooring_data_w00-03.RDS")

# years w .dat
gak_bad_years <- c(2020:2021)

bad_yr_cols <- c("Year",                                                                      
                  "Month",                                                                     
                  "Day",                                                                       
                  "Hour",                                                                      
                  "Minute",                                                                    
                  "Second",                                                                    
                  "Pressure_dbar",                                                          
                  "Depth_m",                                                                 
                  "Temperature_C",                                                       
                  "Conductivity_Soverm",                                                       
                  "Salinity_Pracitical" )

gak_bad_dat <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(gak_bad_dat) <- bad_yr_cols

g<-1
for(g in 1:length(gak_bad_years)) {
  tempyear <- gak_bad_years[g]
  folder_name <- paste0("NGA_GAK1_", tempyear, "-", (tempyear+1), "_ctd_L2_v1", sep="")
  file_names <- list.files(paste0("data/GAK1/", folder_name))
  for(j in 1:length(file_names)){
    tempname <- file_names[j]
    tempdat <- read.table(file = paste0("data/GAK1/", folder_name, "/", tempname), 
                          header=TRUE, skip=49, col.names = bad_yr_cols)
   # tempdat <- tempdat[which(tempdat$Month==6),]
    gak_bad_dat <- dplyr::bind_rows(gak_bad_dat, tempdat)
  }
  print(tempyear)
}
# saveRDS(gak_bad_dat, file="data/GAK1/june_mooring_data_2020_2022.RDS")
# gak_bad_dat <- readRDS(file="data/GAK1/june_mooring_data_2020_2022.RDS")

saveRDS(gak_bad_dat, file="data/GAK1/allmonths_mooring_data_2020_2022.RDS")
gak_bad_dat <- readRDS(file="data/GAK1/allmonths_mooring_data_2020_2022.RDS")


#get june means for 40m-60m

gak_40_60 <- gak_dat[which(gak_dat$Depth_.m.>39.99 & gak_dat$Depth_.m.<60.01),]

gak_jun_40_60_means <- gak_40_60 %>% group_by(year) %>%
  summarise(mean_gak_jun_temp_40_60=mean(Temperature_.C., na.rm=TRUE))

#write.csv(gak_jun_40_60_means, file="data/GAK1/june_40m_to_60m_means_GAK1mooring.csv")


gak_bad_40_60 <- gak_bad_dat[which(gak_bad_dat$Depth_m>39.99 & gak_bad_dat$Depth_m<60.01),]
gak_bad_40_60$year <- gak_bad_40_60$Year
gak_bad_40_60_means <- gak_bad_40_60 %>% group_by(year) %>%
  summarise(mean_gak_jun_temp_40_60=mean(Temperature_C, na.rm=TRUE))

gak_40_60_means_wbad <- rbind(gak_jun_40_60_means, gak_bad_40_60_means)
write.csv(gak_40_60_means_wbad, file="data/GAK1/june_40m_to_60m_means_GAK1mooring.csv")

#all months means for 40-60m
#lot of values right below 60m

gak_40_60 <- gak_dat[which(gak_dat$Depth_.m.>39.99 & gak_dat$Depth_.m.<60.01),]

gak_month_40_60_means <- gak_40_60 %>% group_by(year, month) %>%
  summarise(mean_gak_month_temp_40_60=mean(Temperature_.C., na.rm=TRUE))

#and bad yrs

gak_bad_40_60 <- gak_bad_dat[which(gak_bad_dat$Depth_m>39.99 & gak_bad_dat$Depth_m<60.01),]
gak_bad_40_60$year <- gak_bad_40_60$Year
gak_bad_40_60$month <- gak_bad_40_60$Month
gak_bad_month_40_60_means <- gak_bad_40_60 %>% group_by(year, month) %>%
  summarise(mean_gak_month_temp_40_60=mean(Temperature_C, na.rm=TRUE))

gak_40_60_month_means_wbad <- rbind(gak_month_40_60_means, gak_bad_month_40_60_means)
write.csv(gak_40_60_month_means_wbad, file="data/GAK1/monthly_40m_to_60m_means_GAK1mooring.csv")



#get june means for ~100m

gak_90_110 <- gak_dat[which(gak_dat$Depth_.m.>89.99 & gak_dat$Depth_.m.<110.01),]

gak_jun_90_110_means <- gak_90_110 %>% group_by(year) %>%
  summarise(mean_gak_jun_temp_90_110=mean(Temperature_.C., na.rm=TRUE))

write.csv(gak_jun_90_110_means, file="data/GAK1/june_90m_to_110m_means_GAK1mooring.csv")


gak_bad_90_110 <- gak_bad_dat[which(gak_bad_dat$Depth_m>89.99 & gak_bad_dat$Depth_m<110.01),]
gak_bad_90_110$year <- gak_bad_90_110$Year
gak_bad_90_110_means <- gak_bad_90_110 %>% group_by(year) %>%
  summarise(mean_gak_jun_temp_90_110=mean(Temperature_C, na.rm=TRUE))

gak_90_110_means_wbad <- rbind(gak_jun_90_110_means, gak_bad_90_110_means)
write.csv(gak_90_110_means_wbad, file="data/GAK1/june_90m_to_110m_means_GAK1mooring.csv")


#all months means for 90-110m

gak_90_110 <- gak_dat[which(gak_dat$Depth_.m.>89.99 & gak_dat$Depth_.m.<110.01),]

gak_month_90_110_means <- gak_90_110 %>% group_by(year, month) %>%
  summarise(mean_gak_month_temp_90_110=mean(Temperature_.C., na.rm=TRUE))

#and bad yrs

gak_bad_90_110 <- gak_bad_dat[which(gak_bad_dat$Depth_m>89.99 & gak_bad_dat$Depth_m<110.01),]
gak_bad_90_110$year <- gak_bad_90_110$Year
gak_bad_90_110$month <- gak_bad_90_110$Month
gak_bad_month_90_110_means <- gak_bad_90_110 %>% group_by(year, month) %>%
  summarise(mean_gak_month_temp_90_110=mean(Temperature_C, na.rm=TRUE))

gak_90_110_month_means_wbad <- rbind(gak_month_90_110_means, gak_bad_month_90_110_means)
write.csv(gak_90_110_month_means_wbad, file="data/GAK1/monthly_90m_to_110m_means_GAK1mooring.csv")





#get june means for ~150m

gak_140_160 <- gak_dat[which(gak_dat$Depth_.m.>139.99 & gak_dat$Depth_.m.<160.01),]

gak_jun_140_160_means <- gak_140_160 %>% group_by(year) %>%
  summarise(mean_gak_jun_temp_140_160=mean(Temperature_.C., na.rm=TRUE))

write.csv(gak_jun_140_160_means, file="data/GAK1/june_140m_to_160m_means_GAK1mooring.csv")


gak_bad_140_160 <- gak_bad_dat[which(gak_bad_dat$Depth_m>139.99 & gak_bad_dat$Depth_m<160.01),]
gak_bad_140_160$year <- gak_bad_140_160$Year
gak_bad_140_160_means <- gak_bad_140_160 %>% group_by(year) %>%
  summarise(mean_gak_jun_temp_140_160=mean(Temperature_C, na.rm=TRUE))

gak_140_160_means_wbad <- rbind(gak_jun_140_160_means, gak_bad_140_160_means)
write.csv(gak_140_160_means_wbad, file="data/GAK1/june_140m_to_160m_means_GAK1mooring.csv")



#all months means for 140-160m

gak_140_160 <- gak_dat[which(gak_dat$Depth_.m.>139.99 & gak_dat$Depth_.m.<160.01),]

gak_month_140_160_means <- gak_140_160 %>% group_by(year, month) %>%
  summarise(mean_gak_month_temp_140_160=mean(Temperature_.C., na.rm=TRUE))

#and bad yrs

gak_bad_140_160 <- gak_bad_dat[which(gak_bad_dat$Depth_m>139.99 & gak_bad_dat$Depth_m<160.01),]
gak_bad_140_160$year <- gak_bad_140_160$Year
gak_bad_140_160$month <- gak_bad_140_160$Month
gak_bad_month_140_160_means <- gak_bad_140_160 %>% group_by(year, month) %>%
  summarise(mean_gak_month_temp_140_160=mean(Temperature_C, na.rm=TRUE))

gak_140_160_month_means_wbad <- rbind(gak_month_140_160_means, gak_bad_month_140_160_means)
write.csv(gak_140_160_month_means_wbad, file="data/GAK1/monthly_140m_to_160m_means_GAK1mooring.csv")


#GAK seasonal means=======================

#copied from Mike update below
# divide into seasons!
#40-60m----

gak_40_60_month_means_wbad$season <- "NA"
gak_40_60_month_means_wbad$season[which(gak_40_60_month_means_wbad$month>10|
                                          gak_40_60_month_means_wbad$month<4)] <- "winter"
gak_40_60_month_means_wbad$season[which(gak_40_60_month_means_wbad$month>3 &
                                          gak_40_60_month_means_wbad$month<7)] <- "spring"
gak_40_60_month_means_wbad$season[which(gak_40_60_month_means_wbad$month==7 |
                                          gak_40_60_month_means_wbad$month==8)] <- "summer"
gak_40_60_month_means_wbad$season[which(gak_40_60_month_means_wbad$month==9 |
                                          gak_40_60_month_means_wbad$month==10)] <- "fall"

gak_40_60_month_means_wbad$season_year <- gak_40_60_month_means_wbad$year

i <- 1
for(i in 1:length(gak_40_60_month_means_wbad$year)){
  temprow <- gak_40_60_month_means_wbad[i,]
  tempyear <- temprow$year
  if(temprow$month > 10)
  {
    gak_40_60_month_means_wbad$season_year[i] <- tempyear + 1
  }
}

season_means_40_60_gak <- gak_40_60_month_means_wbad %>% group_by(season_year, season) %>%
  summarise(seasonal_gak_mean=mean(mean_gak_month_temp_40_60))
season_means_40_60_gak$depth <- "40-60"

gak_40_60_month_means_wbad$spawning <- NA
gak_40_60_month_means_wbad$spawning[which(gak_40_60_month_means_wbad$month<5 &
                                      gak_40_60_month_means_wbad$month>2)] <- "spawning_season"
gak_40_60_month_means_wbad$spawning[which(gak_40_60_month_means_wbad$month<3)] <- "prespawning_season"
gak_40_60_month_means_wbad$spawning[which(gak_40_60_month_means_wbad$month>4)] <- "not_spawning"

sp_season_means_40_60_gak <- gak_40_60_month_means_wbad %>% group_by(year, spawning) %>%
  summarise(spawn_season_gak_mean=mean(mean_gak_month_temp_40_60))
sp_season_means_40_60_gak$depth <- "40-60"

gak_40_60_month_means_wbad$depth <- "40-60"
gak_40_60_month_means_wbad$mean_gak_monthly_temp <- gak_40_60_month_means_wbad$mean_gak_month_temp_40_60

gak_summary_40_60 <- left_join(gak_40_60_month_means_wbad[,c(1:2,4:8)], sp_season_means_40_60_gak)
gak_summary_40_60 <- left_join(gak_summary_40_60, season_means_40_60_gak)

#repeat for other depths
#90-110m-----

# divide into seasons!

gak_90_110_month_means_wbad$season <- "NA"
gak_90_110_month_means_wbad$season[which(gak_90_110_month_means_wbad$month>10|
                                            gak_90_110_month_means_wbad$month<4)] <- "winter"
gak_90_110_month_means_wbad$season[which(gak_90_110_month_means_wbad$month>3 &
                                            gak_90_110_month_means_wbad$month<7)] <- "spring"
gak_90_110_month_means_wbad$season[which(gak_90_110_month_means_wbad$month==7 |
                                            gak_90_110_month_means_wbad$month==8)] <- "summer"
gak_90_110_month_means_wbad$season[which(gak_90_110_month_means_wbad$month==9 |
                                            gak_90_110_month_means_wbad$month==10)] <- "fall"

gak_90_110_month_means_wbad$season_year <- gak_90_110_month_means_wbad$year

i <- 1
for(i in 1:length(gak_90_110_month_means_wbad$year)){
  temprow <- gak_90_110_month_means_wbad[i,]
  tempyear <- temprow$year
  if(temprow$month > 10)
  {
    gak_90_110_month_means_wbad$season_year[i] <- tempyear + 1
  }
}

season_means_90_110_gak <- gak_90_110_month_means_wbad %>% group_by(season_year, season) %>%
  summarise(seasonal_gak_mean=mean(mean_gak_month_temp_90_110))
season_means_90_110_gak$depth <- "90-110"

gak_90_110_month_means_wbad$spawning <- NA
gak_90_110_month_means_wbad$spawning[which(gak_90_110_month_means_wbad$month<5 &
                                              gak_90_110_month_means_wbad$month>2)] <- "spawning_season"
gak_90_110_month_means_wbad$spawning[which(gak_90_110_month_means_wbad$month<3)] <- "prespawning_season"
gak_90_110_month_means_wbad$spawning[which(gak_90_110_month_means_wbad$month>4)] <- "not_spawning"

sp_season_means_90_110_gak <- gak_90_110_month_means_wbad %>% group_by(year, spawning) %>%
  summarise(spawn_season_gak_mean=mean(mean_gak_month_temp_90_110))
sp_season_means_90_110_gak$depth <- "90-110"

gak_90_110_month_means_wbad$depth <- "90-110"
gak_90_110_month_means_wbad$mean_gak_monthly_temp <- gak_90_110_month_means_wbad$mean_gak_month_temp_90_110

gak_summary_90_110 <- left_join(gak_90_110_month_means_wbad[,c(1:2,4:8)], sp_season_means_90_110_gak)
gak_summary_90_110 <- left_join(gak_summary_90_110, season_means_90_110_gak)



#140-160m-----

# divide into seasons!

gak_140_160_month_means_wbad$season <- "NA"
gak_140_160_month_means_wbad$season[which(gak_140_160_month_means_wbad$month>10|
                                          gak_140_160_month_means_wbad$month<4)] <- "winter"
gak_140_160_month_means_wbad$season[which(gak_140_160_month_means_wbad$month>3 &
                                          gak_140_160_month_means_wbad$month<7)] <- "spring"
gak_140_160_month_means_wbad$season[which(gak_140_160_month_means_wbad$month==7 |
                                          gak_140_160_month_means_wbad$month==8)] <- "summer"
gak_140_160_month_means_wbad$season[which(gak_140_160_month_means_wbad$month==9 |
                                          gak_140_160_month_means_wbad$month==10)] <- "fall"

gak_140_160_month_means_wbad$season_year <- gak_140_160_month_means_wbad$year

i <- 1
for(i in 1:length(gak_140_160_month_means_wbad$year)){
  temprow <- gak_140_160_month_means_wbad[i,]
  tempyear <- temprow$year
  if(temprow$month > 10)
  {
    gak_140_160_month_means_wbad$season_year[i] <- tempyear + 1
  }
}

season_means_140_160_gak <- gak_140_160_month_means_wbad %>% group_by(season_year, season) %>%
  summarise(seasonal_gak_mean=mean(mean_gak_month_temp_140_160))
season_means_140_160_gak$depth <- "140-160"

gak_140_160_month_means_wbad$spawning <- NA
gak_140_160_month_means_wbad$spawning[which(gak_140_160_month_means_wbad$month<5 &
                                            gak_140_160_month_means_wbad$month>2)] <- "spawning_season"
gak_140_160_month_means_wbad$spawning[which(gak_140_160_month_means_wbad$month<3)] <- "prespawning_season"
gak_140_160_month_means_wbad$spawning[which(gak_140_160_month_means_wbad$month>4)] <- "not_spawning"

sp_season_means_140_160_gak <- gak_140_160_month_means_wbad %>% group_by(year, spawning) %>%
  summarise(spawn_season_gak_mean=mean(mean_gak_month_temp_140_160))
sp_season_means_140_160_gak$depth <- "140-160"

gak_140_160_month_means_wbad$depth <- "140-160"
gak_140_160_month_means_wbad$mean_gak_monthly_temp <- gak_140_160_month_means_wbad$mean_gak_month_temp_140_160

gak_summary_140_160 <- left_join(gak_140_160_month_means_wbad[,c(1:2,4:8)], sp_season_means_140_160_gak)
gak_summary_140_160 <- left_join(gak_summary_140_160, season_means_140_160_gak)

#join up all depths
sum1 <- left_join(gak_summary_40_60, gak_summary_90_110)
gak_summary <- left_join(sum1, gak_summary_140_160)

#plot
ggplot(gak_summary, aes(year, mean_gak_monthly_temp)) + geom_point() + facet_wrap(~month)

ggplot(gak_summary, aes(season_year, seasonal_gak_mean, col=season)) + geom_point() + geom_line()

ggplot(gak_summary, aes(year, spawn_season_gak_mean, col=spawning)) + geom_point() + geom_line()


#grab csvs for SST and MHWI=============

#these are created in 'CREAT_TEMPANDHEAT.r'

MHWIdat <- read.csv(paste(wd, "/CENTRAL_GOA_HEATWAVE-main/", "MHWI.csv",sep=""))

MHWIdat$annual_MHWI <- MHWIdat$Annual
MHWIdat$summer_MHWI <- MHWIdat$Summer
MHWIdat$winter_MHWI <- MHWIdat$Winter
MHWIdat$spawning_MHWI <- MHWIdat$Spawning

MHWIdat <- MHWIdat[,c(1,6:9)]
MHWIdat$depth <- 0


seasonalmeanSST <- read.csv(paste(wd,  "/seasonalmeanSST.csv",sep=""))

#pivot wider to match to other datasets

sst_wide <- seasonalmeanSST %>% pivot_wider(names_from = season, values_from = seasonal_mean)

sst_wide$mean_SST_jun_aug_sep <- sst_wide$jun_aug_sep
sst_wide$mean_SST_oct_nov_dec <- sst_wide$oct_nov_dec
sst_wide$mean_SST_apr_may_jun <- sst_wide$apr_may_jun
sst_wide$mean_SST_jan_feb_mar <- sst_wide$jan_feb_mar

sst_wide <- sst_wide[,c(1,6:9)]


#ESP bottom temp data from survey=======

goa_bot_temp <- read.csv(file=paste0(wd, "/data/", "ESP_goa_bot_temp.csv", sep=""))

#ESP bottom temp data from survey=======

goa_LL_temp <- read.csv(file=paste0(wd, "/data/", "ESP_goa_LL_temp.csv", sep=""))


#join all data sets together======

dat1 <- left_join(sst_wide, MHWIdat, by=join_by(year == Year))
dat2 <- left_join(dat1, esp_temps, by=join_by(year == Year))
dat3 <- left_join(dat2, cfsr_jun_devs_wide, by=join_by(year == Year))
dat4 <- left_join(dat3, goa_bot_temp, by=join_by(year == YEAR))
dat5 <- left_join(dat4, goa_LL_temp, by=join_by(year == YEAR))
dat6 <- left_join(dat5, gak_summary)

write.csv(dat6,paste0(wd,"/data/temp_metrics_dataset.csv"),row.names=F)


#plot for funsies======

dat_wide <- pivot_longer(dat6, -c(year, season, spawning, depth, month, season_year), names_to="temp_type", values_to = "temp")

ggplot(dat_wide, aes(year, temp, col=temp_type)) + geom_point() + geom_line()


#seperate join only temp at depth========

long1 <- left_join(cfsr_long, gak_summary, by=join_by("Year"=="year", "Month"=="month" ))

write.csv(long1,paste0(wd,"/data/temp_depth_gak_cfsr_dataset.csv"),row.names=F)

