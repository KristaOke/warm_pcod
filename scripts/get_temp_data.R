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

#seasonal means for CFSR=======
#HERE FRIDAY

#seasonal means=====

cfsr_long$season <- "NA"
cfsr_long$season[which(cfsr_long$Month>10|
                         cfsr_long$Month<4)] <- "winter"
cfsr_long$season[which(cfsr_long$Month>3 &
                         cfsr_long$Month<7)] <- "spring"
cfsr_long$season[which(cfsr_long$Month==7 |
                         cfsr_long$Month==8)] <- "summer"
cfsr_long$season[which(cfsr_long$Month==9 |
                         cfsr_long$Month==10)] <- "fall"

cfsr_long$season_year <- cfsr_long$Year

i <- 1
for(i in 1:length(cfsr_long$Year)){
  temprow <- cfsr_long[i,]
  tempyear <- temprow$Year
  if(temprow$Month > 10)
  {
    cfsr_long$season_year[i] <- tempyear + 1
  }
}

cfsr_season_means <- cfsr_long %>% group_by(season_year, season, size) %>%
  summarise(seasonal_cfsr_mean=mean(temp, na.rm=TRUE))

ggplot(cfsr_season_means, aes(season_year, seasonal_cfsr_mean, col=size)) + facet_wrap(~season) + geom_point() + geom_line()

#spawning seasons

cfsr_long$spawning <- NA
cfsr_long$spawning[which(cfsr_long$Month<5 &
                           cfsr_long$Month>2)] <- "spawning_season"
cfsr_long$spawning[which(cfsr_long$Month<3)] <- "prespawning_season"
cfsr_long$spawning[which(cfsr_long$Month>4)] <- "not_spawning"

cfsr_sp_season_means <- cfsr_long %>% group_by(Year, spawning, size) %>%
  summarise(spawn_season_cfsr_mean=mean(temp, na.rm=TRUE))

ggplot(cfsr_sp_season_means, aes(Year, spawn_season_cfsr_mean, col=size)) + facet_wrap(~spawning) + geom_point() + geom_line()




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
#GAK1_dat <- read.csv("http://research.cfos.uaf.edu/gak1/data/TimeSeries/gak1.csv")
GAK1_dat <- read.csv(paste0(getwd(), "/data/GAK1/gak1-2.csv"))
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


ggplot(GAK1_dat[which(GAK1_dat$Depth==0),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)

ggplot(GAK1_dat[which(GAK1_dat$Depth==30),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)

ggplot(GAK1_dat[which(GAK1_dat$Depth==50),], aes(year, Temp)) + geom_point() + 
  geom_line() + facet_wrap(~month)

table(GAK1_dat$year[which(GAK1_dat$Depth==50)], GAK1_dat$month[which(GAK1_dat$Depth==50)])

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

write.csv(gak_jun_40_60_means, file="data/GAK1/june_40m_to_60m_means_GAK1mooring.csv")


gak_bad_40_60 <- gak_bad_dat[which(gak_bad_dat$Depth_m>39.99 & gak_bad_dat$Depth_m<60.01),]
gak_bad_40_60$year <- gak_bad_40_60$Year
gak_bad_40_60_means <- gak_bad_40_60 %>% group_by(year) %>%
  summarise(mean_gak_jun_temp_40_60=mean(Temperature_C, na.rm=TRUE))

gak_40_60_means_wbad <- rbind(gak_jun_40_60_means, gak_bad_40_60_means)
write.csv(gak_40_60_means_wbad, file="data/GAK1/june_40m_to_60m_means_GAK1mooring.csv")
gak_40_60_means_wbad<-read.csv(file="data/GAK1/june_40m_to_60m_means_GAK1mooring.csv", row.names = 1)

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
gak_40_60_month_means_wbad <- read.csv(file="data/GAK1/monthly_40m_to_60m_means_GAK1mooring.csv", row.names = 1)



#get june means for ~100m

gak_90_110 <- gak_dat[which(gak_dat$Depth_.m.>89.99 & gak_dat$Depth_.m.<110.01),]

gak_jun_90_110_means <- gak_90_110 %>% group_by(year) %>%
  summarise(mean_gak_jun_temp_90_110=mean(Temperature_.C., na.rm=TRUE))

write.csv(gak_jun_90_110_means, file="data/GAK1/june_90m_to_110m_means_GAK1mooring.csv")
gak_jun_90_110_means <- read.csv(file="data/GAK1/june_90m_to_110m_means_GAK1mooring.csv", row.names = 1)


gak_bad_90_110 <- gak_bad_dat[which(gak_bad_dat$Depth_m>89.99 & gak_bad_dat$Depth_m<110.01),]
gak_bad_90_110$year <- gak_bad_90_110$Year
gak_bad_90_110_means <- gak_bad_90_110 %>% group_by(year) %>%
  summarise(mean_gak_jun_temp_90_110=mean(Temperature_C, na.rm=TRUE))

gak_90_110_means_wbad <- rbind(gak_jun_90_110_means, gak_bad_90_110_means)
write.csv(gak_90_110_means_wbad, file="data/GAK1/june_90m_to_110m_means_GAK1mooring.csv")
gak_90_110_means_wbad <- read.csv(file="data/GAK1/june_90m_to_110m_means_GAK1mooring.csv", row.names = 1)


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
gak_90_110_month_means_wbad <- read.csv(file="data/GAK1/monthly_90m_to_110m_means_GAK1mooring.csv", row.names = 1)





#get june means for ~150m

gak_140_160 <- gak_dat[which(gak_dat$Depth_.m.>139.99 & gak_dat$Depth_.m.<160.01),]

gak_jun_140_160_means <- gak_140_160 %>% group_by(year) %>%
  summarise(mean_gak_jun_temp_140_160=mean(Temperature_.C., na.rm=TRUE))

write.csv(gak_jun_140_160_means, file="data/GAK1/june_140m_to_160m_means_GAK1mooring.csv")
gak_jun_140_160_means <- read.csv(file="data/GAK1/june_140m_to_160m_means_GAK1mooring.csv", row.names = 1)


gak_bad_140_160 <- gak_bad_dat[which(gak_bad_dat$Depth_m>139.99 & gak_bad_dat$Depth_m<160.01),]
gak_bad_140_160$year <- gak_bad_140_160$Year
gak_bad_140_160_means <- gak_bad_140_160 %>% group_by(year) %>%
  summarise(mean_gak_jun_temp_140_160=mean(Temperature_C, na.rm=TRUE))

gak_140_160_means_wbad <- rbind(gak_jun_140_160_means, gak_bad_140_160_means)
write.csv(gak_140_160_means_wbad, file="data/GAK1/june_140m_to_160m_means_GAK1mooring.csv")
gak_140_160_means_wbad <- read.csv(file="data/GAK1/june_140m_to_160m_means_GAK1mooring.csv", row.names = 1)



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
gak_140_160_month_means_wbad <- read.csv(file="data/GAK1/monthly_140m_to_160m_means_GAK1mooring.csv", row.names = 1)

#GAK seasonal means=======================

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
  summarise(seasonal_gak_mean=mean(mean_gak_month_temp_40_60, na.rm=TRUE))
season_means_40_60_gak$depth <- "40-60"

gak_40_60_month_means_wbad$spawning <- NA
gak_40_60_month_means_wbad$spawning[which(gak_40_60_month_means_wbad$month<5 &
                                      gak_40_60_month_means_wbad$month>2)] <- "spawning_season"
gak_40_60_month_means_wbad$spawning[which(gak_40_60_month_means_wbad$month<3)] <- "prespawning_season"
gak_40_60_month_means_wbad$spawning[which(gak_40_60_month_means_wbad$month>4)] <- "not_spawning"

sp_season_means_40_60_gak <- gak_40_60_month_means_wbad %>% group_by(year, spawning) %>%
  summarise(spawn_season_gak_mean=mean(mean_gak_month_temp_40_60, na.rm=TRUE))
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
  summarise(seasonal_gak_mean=mean(mean_gak_month_temp_90_110, na.rm=TRUE))
season_means_90_110_gak$depth <- "90-110"

gak_90_110_month_means_wbad$spawning <- NA
gak_90_110_month_means_wbad$spawning[which(gak_90_110_month_means_wbad$month<5 &
                                              gak_90_110_month_means_wbad$month>2)] <- "spawning_season"
gak_90_110_month_means_wbad$spawning[which(gak_90_110_month_means_wbad$month<3)] <- "prespawning_season"
gak_90_110_month_means_wbad$spawning[which(gak_90_110_month_means_wbad$month>4)] <- "not_spawning"

sp_season_means_90_110_gak <- gak_90_110_month_means_wbad %>% group_by(year, spawning) %>%
  summarise(spawn_season_gak_mean=mean(mean_gak_month_temp_90_110, na.rm=TRUE))
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
  summarise(seasonal_gak_mean=mean(mean_gak_month_temp_140_160, na.rm=TRUE))
season_means_140_160_gak$depth <- "140-160"

gak_140_160_month_means_wbad$spawning <- NA
gak_140_160_month_means_wbad$spawning[which(gak_140_160_month_means_wbad$month<5 &
                                            gak_140_160_month_means_wbad$month>2)] <- "spawning_season"
gak_140_160_month_means_wbad$spawning[which(gak_140_160_month_means_wbad$month<3)] <- "prespawning_season"
gak_140_160_month_means_wbad$spawning[which(gak_140_160_month_means_wbad$month>4)] <- "not_spawning"

sp_season_means_140_160_gak <- gak_140_160_month_means_wbad %>% group_by(year, spawning) %>%
  summarise(spawn_season_gak_mean=mean(mean_gak_month_temp_140_160, na.rm=TRUE))
sp_season_means_140_160_gak$depth <- "140-160"

gak_140_160_month_means_wbad$depth <- "140-160"
gak_140_160_month_means_wbad$mean_gak_monthly_temp <- gak_140_160_month_means_wbad$mean_gak_month_temp_140_160

gak_summary_140_160 <- left_join(gak_140_160_month_means_wbad[,c(1:2,4:8)], sp_season_means_140_160_gak)
gak_summary_140_160 <- left_join(gak_summary_140_160, season_means_140_160_gak)

#join up all depths
sum1 <- full_join(gak_summary_40_60, gak_summary_90_110)
gak_summary <- full_join(sum1, gak_summary_140_160)

#plot
ggplot(gak_summary, aes(year, mean_gak_monthly_temp)) + geom_point() + facet_wrap(~month)

ggplot(gak_summary, aes(season_year, seasonal_gak_mean, col=season)) + geom_point() + geom_line()

ggplot(gak_summary, aes(year, spawn_season_gak_mean, col=spawning)) + geom_point() + geom_line()


#read in HYCOM=========


wd <- getwd()
hycom_season <- read.csv(paste(wd,"/data/hycom_season_means.csv",sep=""))
hycom_spawn <- read.csv(paste(wd,"/data/hycom_spawnseason_means.csv",sep=""))
hycom_month <- read.csv(paste(wd,"/data/hycom_monthly_means.csv",sep=""))







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


#Survey observations=====================

obs_goa <- readRDS(file=paste0(wd, "/data/goa_obs_w_depth.rds", sep=""))

obs_goa <- obs_goa %>%
  mutate( day = day(date), month = month(date), year = year(date))

obs_goa <- obs_goa[which(obs_goa$bot_dep<201),]

ggplot(obs_goa[which(obs_goa$bot_dep<200),], aes(longitude,latitude,  col=source)) + 
  geom_point() + facet_wrap(~month)

#lot of depths, look like casts, take range or only exact?

obs50 <- obs_goa[which(obs_goa$depth==50),]
obs100 <- obs_goa[which(obs_goa$depth==100),]
obs150 <- obs_goa[which(obs_goa$depth==150),]

obs50_table <- table(obs50$year, obs50$month, obs50$source)
obs100_table <- table(obs100$year, obs100$month, obs100$source)
obs150_table <- table(obs150$year, obs150$month, obs150$source)

write.csv(obs50_table, paste0(wd,"/data/table_observations_50m.csv"),row.names=F)
write.csv(obs100_table, paste0(wd,"/data/table_observations_100m.csv"),row.names=F)
write.csv(obs150_table, paste0(wd,"/data/table_observations_150m.csv"),row.names=F)

#trying to plot

ggplot(obs50, aes(longitude, latitude, col=as.factor(month))) + facet_wrap(~interaction(source, year)) + geom_point()

ggplot(obs100, aes(longitude, latitude, col=as.factor(month))) + facet_wrap(~interaction(source, year)) + geom_point()

ggplot(obs150, aes(longitude, latitude, col=as.factor(month))) + facet_wrap(~interaction(source, year)) + geom_point()


#get monthly means
obs50means <- obs50 %>% group_by(year, month, source) %>%
  summarise(mean_monthly_survey_temp=mean(obs_temp, na.rm=TRUE))
obs100means <- obs100 %>% group_by(year, month, source) %>%
  summarise(mean_monthly_survey_temp=mean(obs_temp, na.rm=TRUE))
obs150means <- obs150 %>% group_by(year, month, source) %>%
  summarise(mean_monthly_survey_temp=mean(obs_temp, na.rm=TRUE))

#pivot to get each survey as a column
obs50wide <- obs50means %>% pivot_wider(names_from="source", values_from = "mean_monthly_survey_temp")
obs100wide <- obs100means %>% pivot_wider(names_from="source", values_from = "mean_monthly_survey_temp")
obs150wide <- obs150means %>% pivot_wider(names_from="source", values_from = "mean_monthly_survey_temp")

obs50wide$depth <- "50m"
obs100wide$depth <- "100m"
obs150wide$depth <- "150m"

obs_wide <- rbind(obs50wide, obs100wide, obs150wide)

#asign seasons get season means
obs_wide$season <- "NA"
obs_wide$season[which(obs_wide$month>10|
                        obs_wide$month<4)] <- "winter"
obs_wide$season[which(obs_wide$month>3 &
                        obs_wide$month<7)] <- "spring"
obs_wide$season[which(obs_wide$month==7 |
                         obs_wide$month==8)] <- "summer"
obs_wide$season[which(obs_wide$month==9 |
                        obs_wide$month==10)] <- "fall"

obs_wide$season_year <- obs_wide$year

i <- 1
for(i in 1:length(obs_wide$year)){
  temprow <- obs_wide[i,]
  tempyear <- obs_wide$year
  if(temprow$month > 10)
  {
    obs_wide$season_year[i] <- tempyear + 1
  }
}

obs_season_means <- obs_wide %>% group_by(season_year, season, depth) %>%
  summarise(seasonal_BTS_mean=mean(AFSC_BTS, na.rm=TRUE), seasonal_LLS_mean=mean(AFSC_LLS, na.rm=TRUE), seasonal_IPHC_mean=mean(IPHC_FISS, na.rm=TRUE))

#spawning seasons

obs_wide$spawning <- NA
obs_wide$spawning[which(obs_wide$month<5 &
                          obs_wide$month>2)] <- "spawning_season"
obs_wide$spawning[which(obs_wide$month<3)] <- "prespawning_season"
obs_wide$spawning[which(obs_wide$month>4)] <- "not_spawning"

obs_sp_season_means <- obs_wide %>% group_by(year, spawning, depth) %>%
  summarise(spawn_BTS_mean=mean(AFSC_BTS, na.rm=TRUE), spawn_LLS_mean=mean(AFSC_LLS, na.rm=TRUE), spawn_IPHC_mean=mean(IPHC_FISS, na.rm=TRUE))


#join all data sets together======

dat1 <- left_join(sst_wide, MHWIdat, by=join_by(year == Year))
dat2 <- left_join(dat1, esp_temps, by=join_by(year == Year))
dat3 <- left_join(dat2, cfsr_jun_devs_wide, by=join_by(year == Year))
dat4 <- left_join(dat3, goa_bot_temp, by=join_by(year == YEAR))
dat5 <- left_join(dat4, goa_LL_temp, by=join_by(year == YEAR))
dat5$depth <- as.character(dat5$depth)
dat6 <- left_join(dat5, gak_summary)

write.csv(dat6,paste0(wd,"/data/temp_metrics_dataset.csv"),row.names=F)


#plot for funsies======

dat_wide <- pivot_longer(dat6, -c(year, season, spawning, depth, month, season_year), names_to="temp_type", values_to = "temp")

ggplot(dat_wide, aes(year, temp, col=temp_type)) + geom_point() + geom_line()






#INSTEAD match seperate dfs for monthly, seasonal, and spawn season means======
#for GAK, CFSR, HYCOM
#HERE ALSO FRIDAY


#cfsr
cfsr_compare_dat <- cfsr_long[which(cfsr_long$size=="X0.20"|
                                      cfsr_long$size=="X40.60"),]
cfsr_compare_dat$depth <- NA
cfsr_compare_dat$depth[which(cfsr_compare_dat$size=="X0.20")] <- "50m"
cfsr_compare_dat$depth[which(cfsr_compare_dat$size=="X40.60")] <- "100m" #DOUBLECHECK W STEVE

gak_summary$depth <- as.numeric(gak_summary$depth)
cfsr_compare_dat <- cfsr_compare_dat[,-3] #drop size

# cfsr_season_means

cfsr_season_means <- cfsr_season_means[which(cfsr_season_means$size=="X0.20"|
                                      cfsr_season_means$size=="X40.60"),]
cfsr_season_means$depth <- NA
cfsr_season_means$depth[which(cfsr_season_means$size=="X0.20")] <- "50m"
cfsr_season_means$depth[which(cfsr_season_means$size=="X40.60")] <- "100m" #DOUBLECHECK W STEVE

cfsr_season_means <- cfsr_season_means[,-3] #drop size

# cfsr_sp_season_means

cfsr_sp_season_means <- cfsr_sp_season_means[which(cfsr_sp_season_means$size=="X0.20"|
                                               cfsr_sp_season_means$size=="X40.60"),]
cfsr_sp_season_means$depth <- NA
cfsr_sp_season_means$depth[which(cfsr_sp_season_means$size=="X0.20")] <- "50m"
cfsr_sp_season_means$depth[which(cfsr_sp_season_means$size=="X40.60")] <- "100m" #DOUBLECHECK W STEVE

cfsr_sp_season_means <- cfsr_sp_season_means[,-3] #drop size


#join months

cfsr_compare_dat$depth <- as.character(cfsr_compare_dat$depth)

mon1 <- full_join(cfsr_compare_dat[,-c(4:6)], hycom_month[,-1], by=join_by("Year"=="year", "Month"=="month", "depth"=="depth_cat"))#, 
                                                            #"season"=="season", "season_year"=="season_year", "spawning"=="spawning" ))
#mon1 <- mon1[which(duplicated(mon1)==FALSE),] #check for duplicates, may need this if they exist
mon1$cfsr_temp <- mon1$temp
mon1 <- mon1[,-3]

gak_summary$depth_cat <- NA

gak_summary$depth_cat[which(gak_summary$depth=="40-60")] <- "50m"
gak_summary$depth_cat[which(gak_summary$depth=="90-110")] <- "100m"
gak_summary$depth_cat[which(gak_summary$depth=="140-160")] <- "150m"

gak_month <- gak_summary[,c(1:5,7,10)]

month_combined <- full_join(mon1, gak_month[,-c(3:5)], by=join_by("Year"=="year", "Month"=="month", "depth"=="depth_cat"))#,"season_year"=="season_year", "spawning"=="spawning",
                                                                  # "season"=="season"))
month_combined <- full_join(month_combined, obs_wide[,-c(7:9)], by=join_by("Year"=="year", "Month"=="month","depth"=="depth"))#, #"season_year"=="season_year", "spawning"=="spawning", 
                                                      # "season"=="season"))

months_long <- month_combined %>% pivot_longer(names_to = "type", values_to = 'temp', -c(Year, Month, depth)) #season, season_year, spawning, depth))

ggplot(months_long[which(months_long$Month=="1"),], aes(Year, temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

ggplot(months_long[which(months_long$Month=="3"),], aes(Year, temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

ggplot(months_long[which(months_long$Month=="4"),], aes(Year, temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

ggplot(months_long[which(months_long$Month=="6"),], aes(Year, temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

write.csv(month_combined,paste0(wd,"/data/monthly_temp_depth_gak_cfsr_hycom_surv_dataset.csv"),row.names=F)


#join season

cfsr_season_means$depth <- as.character(cfsr_season_means$depth)

seas1 <- full_join(cfsr_season_means, hycom_season[,-1], by=join_by("depth"=="depth_cat", 
                                                                 "season"=="season", "season_year"=="season_year"))

gak_seas <- gak_summary[,c(1,3,4,9,10)]

season_combined <- full_join(seas1, gak_seas[,-1], by=join_by("season_year"=="season_year", "depth"=="depth_cat", "season"=="season"))

season_combined <- full_join(season_combined, obs_season_means, by=join_by("season"=="season", "season_year"=="season_year", "depth"=="depth"))
season_combined <- season_combined[!duplicated(season_combined),]                                                         
                                                         
                                                                                                                
season_long <- season_combined %>% pivot_longer(names_to = "type", values_to = 'temp', -c(season, season_year, depth))

ggplot(season_long, aes(season_year, temp, col=type, linetype=season)) + facet_wrap(~depth) + geom_point() + geom_line()

#join spawning season

cfsr_sp_season_means$depth <- as.character(cfsr_sp_season_means$depth)

spseas1 <- full_join(cfsr_sp_season_means, hycom_spawn[,-1], by=join_by("depth"=="depth_cat", 
                                                                    "spawning"=="spawning", "Year"=="year"))

gak_sp_seas <- gak_summary[,c(1,5,8,10)]

sp_season_combined <- full_join(spseas1, gak_sp_seas, by=join_by("spawning"=="spawning", "Year"=="year", "depth"=="depth_cat"))

sp_season_combined <- full_join(sp_season_combined, obs_sp_season_means, by=join_by("spawning"=="spawning", "Year"=="year", "depth"=="depth"))
sp_season_combined <- sp_season_combined[!duplicated(sp_season_combined),]  

sp_season_long <- sp_season_combined %>% pivot_longer(names_to = "type", values_to = 'temp', -c(Year, spawning, depth))

ggplot(sp_season_long, aes(Year, temp, col=type, linetype=spawning)) + facet_wrap(~depth) + geom_point() + geom_line()


ggplot(sp_season_long[which(sp_season_long$spawning=="spawning_season"),], aes(Year, temp, col=type)) + facet_wrap(~depth) + geom_point() + geom_line()

ggplot(sp_season_long[which(sp_season_long$spawning=="prespawning_season"),], aes(Year, temp, col=type)) + facet_wrap(~depth) + geom_point() + geom_line()

ggplot(sp_season_long[which(sp_season_long$spawning=="not_spawning"),], aes(Year, temp, col=type)) + facet_wrap(~depth) + geom_point() + geom_line()


#standardize=============================
#z-score by month and depth

months_scaled <- month_combined %>% group_by(Month, depth) %>% #checked and working
  mutate_at(vars(cfsr_temp, mean_gak_monthly_temp,            
 AFSC_BTS, AFSC_LLS, IPHC_FISS, mean_monthly), scale)



#plot
months_scaled_long <- months_scaled %>% pivot_longer(names_to = "type", values_to = 'scaled_temp', -c(Year, Month, depth)) #season, season_year, spawning, depth))

ggplot(months_scaled_long[which(months_scaled_long$Month=="6"),], aes(Year, scaled_temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

ggplot(months_scaled_long[which(months_scaled_long$Month=="5"),], aes(Year, scaled_temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

ggplot(months_scaled_long[which(months_scaled_long$Month=="7"),], aes(Year, scaled_temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

#save
write.csv(months_scaled, "data/scaled_temp_by_month.csv")



#standardize season=============================
#z-score by season and depth

season_scaled <- season_combined %>% group_by(season, depth) %>% 
  mutate_at(vars(seasonal_cfsr_mean, seasonal_gak_mean,            
                 seasonal_BTS_mean, seasonal_LLS_mean, seasonal_IPHC_mean, seasonal_hycom_mean), scale)
write.csv(season_scaled, "data/scaled_temp_by_season.csv")


#standardize spawning season=============================
#z-score by spawning season and depth

sp_season_scaled <- sp_season_combined %>% group_by(spawning, depth) %>% 
  mutate_at(vars(spawn_season_cfsr_mean, spawn_season_gak_mean,            
                 spawn_BTS_mean, spawn_LLS_mean, spawn_IPHC_mean, spawn_season_hycom_mean), scale)
write.csv(sp_season_scaled, "data/scaled_temp_by_spawningseason.csv")


