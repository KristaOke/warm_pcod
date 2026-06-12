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



#NEW GAK1 best combined data=========
#April 2025

combo <- read.csv(paste(wd,"/data/GAK1/GAK1_CTDmooringCombined.csv",sep=""), skip=20)

ggplot(combo[which(combo$Year==2020),], aes(Year, Depth..m.)) + geom_point() + facet_wrap(~Month)

table(combo$Year, combo$Month)

combo <- combo %>% rename(depth = Depth..m., gak_best_temp = Temperature..degC.)

combo_sub <- combo[which(combo$depth=="50"|combo$depth=="100"|combo$depth=="150"),]


#read in HYCOM=========


wd <- getwd()
#hycom_season <- read.csv(paste(wd,"/data/hycom_season_means.csv",sep=""))
#hycom_spawn <- read.csv(paste(wd,"/data/hycom_spawnseason_means.csv",sep=""))
hycom_month <- read.csv(paste(wd,"/data/hycom_monthly_means.csv",sep=""))







#Survey observations=====================

# ORIGINAL CODE - raw data too big to archive, 
#if working with archived data please skip down to next comment or contact
obs_goa <- readRDS(file=paste0(wd, "/data/goa_obs_w_depth.rds", sep=""))

obs_goa <- obs_goa %>%
  mutate( day = day(date), month = month(date), year = year(date))

obs_goa <- obs_goa[which(obs_goa$bot_dep<201),]

ggplot(obs_goa[which(obs_goa$bot_dep<200),], aes(longitude,latitude,  col=source)) + 
  geom_point() + facet_wrap(~month)

obs50 <- obs_goa[which(obs_goa$depth==50),]
obs100 <- obs_goa[which(obs_goa$depth==100),]
obs150 <- obs_goa[which(obs_goa$depth==150),]

obs50_table <- table(obs50$year, obs50$month, obs50$source)
obs100_table <- table(obs100$year, obs100$month, obs100$source)
obs150_table <- table(obs150$year, obs150$month, obs150$source)

write.csv(obs50_table, paste0(wd,"/data/table_observations_50m.csv"),row.names=F)
write.csv(obs100_table, paste0(wd,"/data/table_observations_100m.csv"),row.names=F)
write.csv(obs150_table, paste0(wd,"/data/table_observations_150m.csv"),row.names=F)

saveRDS(obs50, paste0(wd,"/data/goa_observations_50m.rds"))
saveRDS(obs100, paste0(wd,"/data/goa_observations_100m.rds"))
saveRDS(obs150, paste0(wd,"/data/goa_observations_150m.rds"))

# TO REPRODUCE THIS SCRIPT WITH ARCHIVED DATA START HERE
obs50 <-readRDS(obs50, paste0(wd,"/data/goa_observations_50m.rds"))
obs100 <-readRDS(obs100, paste0(wd,"/data/goa_observations_100m.rds"))
obs150 <-readRDS(obs150, paste0(wd,"/data/goa_observations_150m.rds"))

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



#Match seperate dfs for monthly, seasonal, and spawn season means======
#for GAK, CFSR, HYCOM



#cfsr
cfsr_compare_dat <- cfsr_long[which(cfsr_long$size=="X0.20"|
                                      cfsr_long$size=="X40.60"),]
cfsr_compare_dat$depth <- NA
cfsr_compare_dat$depth[which(cfsr_compare_dat$size=="X0.20")] <- "50m"
cfsr_compare_dat$depth[which(cfsr_compare_dat$size=="X40.60")] <- "100m" #DOUBLECHECK W STEVE

gak_summary$depth <- as.numeric(gak_summary$depth)
cfsr_compare_dat <- cfsr_compare_dat[,-3] #drop size



#join months

cfsr_compare_dat$depth <- as.character(cfsr_compare_dat$depth)

#mon1 <- full_join(cfsr_compare_dat[,-c(4:6)], hycom_month[,-1], by=join_by("Year"=="year", "Month"=="month", "depth"=="depth_cat"))
mon1 <- full_join(cfsr_compare_dat, hycom_month[,-1], by=join_by("Year"=="year", "Month"=="month", "depth"=="depth_cat"))#, 
                                                            #"season"=="season", "season_year"=="season_year", "spawning"=="spawning" ))
#mon1 <- mon1[which(duplicated(mon1)==FALSE),] #check for duplicates, may need this if they exist
mon1$cfsr_temp <- mon1$temp
mon1 <- mon1[,-3]



#new gak best combined
gak_month <- combo_sub

gak_month$depth[which(gak_month$depth=="50")] <- "50m"
gak_month$depth[which(gak_month$depth=="100")] <- "100m"
gak_month$depth[which(gak_month$depth=="150")] <- "150m"

gak_month <- gak_month[,c(1:4)]

month_combined <- full_join(mon1, gak_month, by=join_by("Year"=="Year", "Month"=="Month", "depth"=="depth"))#,"season_year"=="season_year", "spawning"=="spawning",
                                                                  # "season"=="season"))
month_combined <- full_join(month_combined, obs_wide[,-c(7:8)], by=join_by("Year"=="year", "Month"=="month","depth"=="depth"))#, #"season_year"=="season_year", "spawning"=="spawning", 
                                                      # "season"=="season"))

months_long <- month_combined %>% pivot_longer(names_to = "type", values_to = 'temp', -c(Year, Month, depth)) #season, season_year, spawning, depth))

ggplot(months_long[which(months_long$Month=="1"),], aes(Year, temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

ggplot(months_long[which(months_long$Month=="3"),], aes(Year, temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

ggplot(months_long[which(months_long$Month=="4"),], aes(Year, temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

ggplot(months_long[which(months_long$Month=="6"),], aes(Year, temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

write.csv(month_combined,paste0(wd,"/data/monthly_temp_depth_gak_cfsr_hycom_surv_dataset.csv"),row.names=F)


#standardize=============================
#z-score by month and depth

months_scaled <- month_combined %>% group_by(Month, depth) %>% #checked and working
  mutate_at(vars(cfsr_temp, gak_best_temp,            
 AFSC_BTS, AFSC_LLS, IPHC_FISS, mean_monthly), scale)

#March 2025 - CHANGED how I am scaling to instead de-mean using mean up to 2012

months_demeaned <- month_combined %>% 
  group_by(Month, depth) %>%
  mutate(mean_monthly = mean_monthly-mean(mean_monthly[which(Year<2013)], na.rm=TRUE),
         cfsr_temp = cfsr_temp-mean(cfsr_temp[which(Year<2013)], na.rm=TRUE),
         gak_best_temp = gak_best_temp-mean(gak_best_temp[which(Year<2013)], na.rm=TRUE),
         AFSC_BTS = AFSC_BTS-mean(AFSC_BTS[which(Year<2013)], na.rm=TRUE),
         AFSC_LLS = AFSC_LLS-mean(AFSC_LLS[which(Year<2013)], na.rm=TRUE),
         IPHC_FISS = IPHC_FISS-mean(IPHC_FISS[which(Year<2013)], na.rm=TRUE))



#plot
months_scaled_long <- months_scaled %>% pivot_longer(names_to = "type", values_to = 'scaled_temp', -c(Year, Month, depth)) #season, season_year, spawning, depth))

ggplot(months_scaled_long[which(months_scaled_long$Month=="6"),], aes(Year, scaled_temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

ggplot(months_scaled_long[which(months_scaled_long$Month=="5"),], aes(Year, scaled_temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

ggplot(months_scaled_long[which(months_scaled_long$Month=="7"),], aes(Year, scaled_temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

months_demeaned_long <- months_demeaned %>% pivot_longer(names_to = "type", values_to = 'scaled_temp', -c(Year, Month, depth)) #season, season_year, spawning, depth))

ggplot(months_demeaned_long[which(months_demeaned_long$Month=="6"),], aes(Year, scaled_temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)


#save
write.csv(months_scaled, "data/scaled_temp_by_month.csv")

write.csv(months_demeaned, "data/demeaned_temp_by_month.csv")

months_demeaned <- read.csv("data/demeaned_temp_by_month.csv", row.names = 1)

