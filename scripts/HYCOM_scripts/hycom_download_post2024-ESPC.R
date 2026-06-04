library(dplyr)
library(lubridate)
library(doParallel)
library(httr)
library(ncdf4)
library(tidync)
library(akgfmaps)
library(beepr)
library(data.table)

# functions ----

# # Function to get url
get_hycom_url <- function(date, lim) {
  url <- get.hycom.V(limits = lim, time = as.Date(date))
  url_dat <- list("date" = as.Date(date), "url" = url)
}

# First, get temperature/depth profiles at specific HYCOM depths from GLBv0.08
# given inputs lat/lon/time, which goes from Jan 1, 1994 to present.
get.hycom.V <- function(limits, time, vars=c('water_temp'), include_latlon=TRUE,
                        filename='', download.file=TRUE, dir = getwd(), depLevels=NULL) {
  
  dir.create(file.path(dir), recursive = TRUE, showWarnings = FALSE)
  setwd(dir)
  
  ## Set the base URL based on the start date. If the ending date exceeds the
  ## period for this experiment, then print a warning and truncate the output
  ## early.
  
  expts = data.frame(
    start=c(as.Date('1994-01-01'), as.Date('2015-12-31'),
            as.Date('2016-05-01'), as.Date('2017-02-01'),
            as.Date('2017-06-01'), as.Date('2017-10-01'),
            as.Date('2018-01-01'), as.Date('2020-02-19'), as.Date('2020-02-20')),
    end=c(as.Date('2015-12-30'), as.Date('2016-04-30'), 
          as.Date('2017-01-31'), as.Date('2017-05-31'),
          as.Date('2017-09-30'), as.Date('2017-12-31'),
          as.Date('2020-02-18'), as.Date('2025-01-01'), Sys.Date() + 1),
    url=c(paste0('https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_53.X/data/', year(time),'?'),
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_56.3?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_57.2?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_92.8/ts3z?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_57.7?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_92.9/ts3z?',
          'https://ncss.hycom.org/thredds/ncss/GLBv0.08/expt_93.0/ts3z?',
          'https://ncss.hycom.org/thredds/ncss/GLBy0.08/expt_93.0?',
          'https://ncss.hycom.org/thredds/ncss/grid/ESPC-D-V02/t3z?'))
  
  if(time[1] < expts$start[1])
    stop('Data begins at %s and is not available at %s.',
         strftime(expts$start[1], '%d %b %Y'),
         strftime(time[1], '%d %b %Y'))
  if(time[1] > expts$end[nrow(expts)])
    stop('Data ends at %s and is not available at %s.',
         strftime(expts$end[nrow(expts)], '%d %b %Y'),
         strftime(time[1], '%d %b %Y'))
  for(i in seq(nrow(expts))) {
    if((time[1] >= expts$start[i]) & (time[1] <= expts$end[i]))
      url = expts$url[i]
  }
  
  ## Add the variables.
  for(var in vars)
    url = sprintf('%svar=%s&', url, var)
  ## Add the spatial domain.
  url = sprintf('%snorth=%f&west=%f&east=%f&south=%f&horizStride=1&',
                url, limits[[4]], limits[[1]], limits[[2]], limits[[3]])
  # north, west, east, south
  
  ## Add the time domain.
  if(length(time) == 2){
    url = sprintf('%stime_start=%s%%3A00%%3A00Z&time_end=%s%%3A00%%3A00Z&',
                  url, strftime(time[1], '%Y-%m-%dT18'),
                  strftime(time[2], '%Y-%m-%dT18'))
  } else if(length(time) == 1){
    url = sprintf('%stime_start=%s%%3A00%%3A00Z&time_end=%s%%3A00%%3A00Z&',
                  url, strftime(time[1], '%Y-%m-%dT18'),
                  strftime(time[1], '%Y-%m-%dT18'))
  }
  ## Check for the newer HYCOM experiments (3hr time resolution) and add stride=8 if needed, otherwise 1 for daily HYCOM data
  if(any(grep('GLBy', url))){
    url = sprintf('%stimeStride=%s&', url, 8)
  } else{
    url = sprintf('%stimeStride=%s&', url, 1)
  }
  
  ## Add the lat-lon points if requested.
  if(include_latlon)
    url = sprintf('%saddLatLon=true&', url)
  ## Finish the URL.
  if (is.null(depLevels)){
    url = sprintf('%sdisableProjSubset=on&vertCoord=&accept=netcdf', url)
  } else{
    url = paste(url,'disableProjSubset=on&vertCoord=', depLevels, '&accept=netcdf', sep='')
  }
}


# Get GOA stat areas to use for a mask ('auto' returns Alaska Albers)
goa <- akgfmaps::get_nmfs_areas(set.crs = 'auto') %>% 
  filter(REP_AREA > 609) 


# Time span
start <- as.Date("2025-02-23")
end <- as.Date("2025-09-15")
dates <- seq(start, end, by="days")
dates <- dates[seq(1, length(dates), 3)] #every THIRD day
#dates <- dates[which(dates>"2024-04-18")]

#RUN Aug 9 2024 2020-01-01 - 2023-12-31 dates[seq(1, length(dates), 2)] #every SECOND day
#failed at 2020-05-16
#completely restarted after some edits
#RUN 2020-01-02 - 2023-12-31 every 3 days DIED AT 2021-05-25, restarted, done
#Run 2015-01-01 - 2019-12-31 every 3 days, done
#Run 2005-01-01 - 2014-12-31 every 3 days, had to restart at 2005-10-31 and 2008-02-12 and
#2010-05-05
#ran as far as 2012-01-04 , then to 2012-08-01 COME BACK TO THIS
#running 1995-01-01 - 2004-12-31, stopped at 1995-04-16
#stopped at 1999-12-30
#running from 2012, stopped at 2013-07-09
#running 2024 to make figure for PT meeting 2024-09-18



# # Set the boundary
#lim <- c(-150.000000, -120.000000, 49.000000, 51.000000)
lim <- c(-170.0001, -129.9999, 51.9999, 62.0001 )

# Parallel processing...change cores as desired
i<-1
doParallel::registerDoParallel(parallel::detectCores() - 1)
hycom_url <- foreach(i = 1:length(dates), .packages = c("lubridate")) %dopar% {
  j = dates[i]
  get_hycom_url(j, lim)
}
doParallel::stopImplicitCluster()

urls_df <- data.frame(matrix(unlist(hycom_url), nrow=length(hycom_url), byrow=TRUE)) 
names(urls_df) <- c("date", "url")
urls_df$date <- as.Date(as.numeric(urls_df$date), origin = "1970-01-01")

#adding some trimming based on depth

# Need bathymetry used to assign depth to HYCOM bottom (bottom temps are actually 10-m off bottom)
# bathy_nc <- tidync("data/Example/depth_GLBv0.08_09m11.nc") %>% hyper_tibble()
# saveRDS(bathy_nc, file = "data/Example/hycom_bathy.rds", compress = TRUE)
hycom_bathy <- readRDS("data/hycom_bathy.rds") # this is global so needs to be trimmed
# bat shrinks it down before moving to sf
bat <- hycom_bathy %>%
  mutate(lat = Latitude, lon = Longitude - 360, bot_dep = bathymetry) %>%
  filter(lat > 51.9999, lat < 62.0001,
         lon > -170.0001, lon < -129.9999,
         bot_dep < 300.0001) %>% #LIMITED to bottom depths shallower than 300m!
  select(lon, lat, bot_dep)

ptm<-proc.time()
i<-1
for(i in 1:nrow(urls_df)) {
  url <- urls_df$url[i]
  date <- urls_df$date[i]
  tmp <- tempfile()
  response <- GET(url, write_disk(tmp))
  
  if(response$status_code==500){
    tmp <- tempfile()
    response <- GET(url, write_disk(tmp))} 
  #once more for good measure
  if(response$status_code==500){
    tmp <- tempfile()
    response <- GET(url, write_disk(tmp))} 
  
  tidy_nc <- tidync(tmp) %>% hyper_tibble()
  
  mean_lon <- mean(tidy_nc$lon)
  if(mean_lon > 180 & mean_lon < 230){
    tidy_nc$lon <- tidy_nc$lon - 360
  }
  
  tidy_nc <- left_join(tidy_nc, bat)
  tidy_nc <- na.omit(tidy_nc)
  tidy_nc <- tidy_nc[,c(1:4,6)] #drop time stamp
  #tidy_nc$date <- date
  
  #drop most Bering Sea cells by brute force
  tidy_nc$drop <- "N"
  tidy_nc$drop[which(tidy_nc$lon< -158 & tidy_nc$lat>57)]<- "Y"
  tidy_nc$drop[which(tidy_nc$lon< -159 & tidy_nc$lat>56)]<- "Y"
  tidy_nc$drop[which(tidy_nc$lon< -162 & tidy_nc$lat>55.5)]<- "Y"
  tidy_nc$drop[which(tidy_nc$lon< -157 & tidy_nc$lat>57)]<- "Y"
  tidy_nc$drop[which(tidy_nc$lon< -164 & tidy_nc$lat>55)]<- "Y"
  
  #plot to check which values are dropped
  #plot(tidy_nc$lon, tidy_nc$lat, col=as.factor(tidy_nc$drop))
  
  tidy_nc <- tidy_nc[which(tidy_nc$drop=="N"),] #drop those sorted into bering sea
  tidy_nc <- tidy_nc[,c(1:5)] #drop drop col
  
  tidy_nc$water_temp <- round(tidy_nc$water_temp, 2)
  tidy_nc$bot_dep <- round(tidy_nc$bot_dep, 2)
  
  #tidy_nc <- tidy_nc %>% slice_sample(prop=0.5) doesn't save much space
  
  # saveRDS(tidy_nc, file = paste0("data/hycom_temp_data/", urls_df$date[i], ".rds"), compress = TRUE)
  saveRDS(tidy_nc, file = paste0("/Volumes/USB DISK/hycom_watercol_temp_data/", urls_df$date[i], ".rds"), compress = TRUE)
  Sys.sleep(1)
  rm(tidy_nc)
}
beep(sound=8)
proc.time()-ptm

#
#clip to GOA map===========


#clip to GOA map FAST===========

#to replace urls_df in saveRDS
start <- as.Date("2024-09-23")
end <- as.Date("2025-02-20")
dates <- seq(start, end, by="days")
dates <- dates[seq(1, length(dates), 3)]
#MAKE SURE THIS MATCHES THE DATA FILES

#111 "1994-11-27" didn't work, skippin to 1995
#2005-01-1 needed to be restarted, think off sequence, same 2015, 2020

i<-1
for(i in 1:length(dates)){ #updating as code breaks
  
  temp_nc <- readRDS(file = paste0("/Volumes/USB DISK/hycom_watercol_temp_data/", dates[i], ".rds"))
  
  # Pick a depth in the water column, like 50 m 
  wc_50m <- temp_nc %>% filter(depth == 50)
  
  wc_50m_sf <- st_as_sf(wc_50m, coords = c("lon", "lat"), crs = "EPSG:4326") %>% # Sets to WGS 1984
    st_transform(st_crs(goa)) #%>% # projects to Alaska Albers
  wc_50m_sf <-    st_join(wc_50m_sf, goa,  join=st_intersects) # clips to GOA area, and takes a few minutes
  wc_50m_sf <- na.omit(wc_50m_sf)
  #ggplot(wc_50m_sf, aes(col = water_temp)) + geom_sf(size = 0.01) # Note that the variable is just 'water_temp' now
  
  #--
  
  wc_100m <- temp_nc %>% filter(depth == 100)
  
  wc_100m_sf <- st_as_sf(wc_100m, coords = c("lon", "lat"), crs = "EPSG:4326") %>% # Sets to WGS 1984
    st_transform(st_crs(goa)) #%>% # projects to Alaska Albers
  wc_100m_sf <-  st_join(wc_100m_sf, goa,  join=st_intersects) # clips to GOA area, and takes a few minutes
  wc_100m_sf <- na.omit(wc_100m_sf)
  
  #ggplot(wc_100m_sf, aes(col = water_temp)) + geom_sf(size = 0.01) # Note that the variable is just 'water_temp' now
  
  
  #--
  
  wc_150m <- temp_nc %>% filter(depth == 150)
  
  wc_150m_sf <- st_as_sf(wc_150m, coords = c("lon", "lat"), crs = "EPSG:4326") %>% # Sets to WGS 1984
    st_transform(st_crs(goa)) #%>% # projects to Alaska Albers
  wc_150m_sf <- st_join(wc_150m_sf, goa,  join=st_intersects) # clips to GOA area, and takes a few minutes
  wc_150m_sf <- na.omit(wc_150m_sf)
  #ggplot(wc_150m_sf, aes(col = water_temp)) + geom_sf(size = 0.01) # Note that the variable is just 'water_temp' now
  
  #repeat with depth ranges to compare to GAK1
  #hycom has only 90 & 100, and only 150 in the higher bins
  
  wc_40_60m <- temp_nc %>% filter(depth >39 & depth <61)
  
  wc_40_60m_sf <- st_as_sf(wc_40_60m, coords = c("lon", "lat"), crs = "EPSG:4326") %>% # Sets to WGS 1984
    st_transform(st_crs(goa)) #%>% # projects to Alaska Albers
  wc_40_60m_sf <- st_join(wc_40_60m_sf, goa,  join=st_intersects) # clips to GOA area, and takes a few minutes
  wc_40_60m_sf <- na.omit(wc_40_60m_sf)
  
  ggplot(wc_40_60m_sf, aes(col = water_temp)) + geom_sf(size = 0.01) # Note that the variable is just 'water_temp' now
  
  #--
  
  wc_90_100m <- temp_nc %>% filter(depth >89 & depth <101)
  
  wc_90_100m_sf <- st_as_sf(wc_90_100m, coords = c("lon", "lat"), crs = "EPSG:4326") %>% # Sets to WGS 1984
    st_transform(st_crs(goa)) #%>% # projects to Alaska Albers
  wc_90_100m_sf <- st_join(wc_90_100m_sf, goa,  join=st_intersects) # clips to GOA area, and takes a few minutes
  wc_90_100m_sf <- na.omit(wc_90_100m_sf)
  
  ggplot(wc_90_100m_sf, aes(col = water_temp)) + geom_sf(size = 0.01) # Note that the variable is just 'water_temp' now
  
  saveRDS(wc_40_60m_sf, file=paste0("/Volumes/USB DISK/hycom_clipped_goa/clipped_40_60m_", dates[i], ".rds"), compress = TRUE)
  saveRDS(wc_90_100m_sf, file=paste0("/Volumes/USB DISK/hycom_clipped_goa/clipped_90_100m_", dates[i], ".rds"), compress = TRUE)
  saveRDS(wc_50m_sf, file=paste0("/Volumes/USB DISK/hycom_clipped_goa/clipped_50m_", dates[i], ".rds"), compress = TRUE)
  saveRDS(wc_100m_sf, file=paste0("/Volumes/USB DISK/hycom_clipped_goa/clipped_100m_", dates[i], ".rds"), compress = TRUE)
  saveRDS(wc_150m_sf, file=paste0("/Volumes/USB DISK/hycom_clipped_goa/clipped_150m_", dates[i], ".rds"), compress = TRUE)
  
  #rm(tidy_nc)
  
}

beep()

#combine into df======
#now need a loop to read back in clipped files, with date, and combine into big dataframes

start <- as.Date("2024-09-23")
end <- as.Date("2025-02-20")
dates <- seq(start, end, by="days")
dates <- dates[seq(1, length(dates), 3)]
#MAKE SURE THIS MATCHES THE DATA FILES
#dates <- dates[which(dates>"2010-12-31")]

#starting 1995, return to 1994
#got slow had to estart at 2003-06-27
#seems 2003-06-27 through 2005-01-01 missing, redoing
#restarting 2005-2010

#use previous wc sf file to create new one


wc_dates_combined <- wc_100m_sf[1,]
#wc_dates_combined <- wc_dates_combined[-1,]
wc_dates_combined$date <- as.Date("1000-01-01")
wc_dates_combined$depth_cat <- NA
i <- 1
for(i in 1:length(dates)){
  wc_40_60m_sf <-   readRDS(file=paste0("/Volumes/USB DISK/hycom_clipped_goa/clipped_40_60m_", dates[i], ".rds"))
  wc_90_100m_sf <- readRDS(file=paste0("/Volumes/USB DISK/hycom_clipped_goa/clipped_90_100m_", dates[i], ".rds"))
  wc_50m_sf <- readRDS(file=paste0("/Volumes/USB DISK/hycom_clipped_goa/clipped_50m_", dates[i], ".rds"))
  wc_100m_sf <- readRDS(file=paste0("/Volumes/USB DISK/hycom_clipped_goa/clipped_100m_", dates[i], ".rds"))
  wc_150m_sf <- readRDS(file=paste0("/Volumes/USB DISK/hycom_clipped_goa/clipped_150m_", dates[i], ".rds"))
  
  wc_40_60m_sf$date <- dates[i]
  wc_90_100m_sf$date <- dates[i]
  wc_50m_sf$date <- dates[i]
  wc_100m_sf$date <- dates[i]
  wc_150m_sf$date <- dates[i]
  
  wc_40_60m_sf$depth_cat <- "40-60m"
  wc_90_100m_sf$depth_cat <- "90-100m"
  wc_50m_sf$depth_cat <- "50m"
  wc_100m_sf$depth_cat <- "100m"
  wc_150m_sf$depth_cat <- "150m"
  
  wc_list <- list(wc_40_60m_sf, wc_90_100m_sf,
                  wc_50m_sf, wc_100m_sf,
                  wc_150m_sf)
  wc_combined <- data.table::rbindlist(wc_list)
  comb_list <- list(wc_dates_combined, wc_combined)
  wc_dates_combined <- data.table::rbindlist(comb_list)
  
  if(i==1){
    wc_dates_combined <- wc_dates_combined[which(wc_dates_combined$date!="1000-01-1"),]
  }
  
  print(dates[i])
  
}

saveRDS(wc_dates_combined,file=paste0("/Volumes/USB DISK/hycom_clipped_goa/all_combined_end", dates[i], ".rds"), compress = TRUE)
beep(4)

#monthly means====

filelist<-list.files("/Volumes/USB DISK/hycom_clipped_goa/", pattern="all_combined_end", full.names=TRUE)


i<-1
for(i in 1:length(filelist)){
  tempcomb <- readRDS(file=filelist[i])
  
  tempcomb <- as.data.frame(tempcomb)
  tempcomb <- tempcomb[,c(1,3,10:11)]
  
  #split date into year and month
  tempcomb <- tempcomb %>%
    mutate( day = day(date), month = month(date), year = year(date))
  
  temp_means <- tempcomb %>% group_by(year, month, depth_cat) %>%
    summarize(mean_monthly=mean(water_temp, na.rm=TRUE))
  
  saveRDS(temp_means, file=paste0("/Volumes/USB DISK/hycom_clipped_goa/clipped_monthly_means_end", max(tempcomb$date), ".rds"), compress = TRUE)
  
  
  print(i)
}

beep(4)



#stitch means back together

#DELETE first 1995 file, september getting duplicated presumably b/c its incomplete?
#not an exact match so not removed by duplicated
filelist2<-list.files("/Volumes/USB DISK/hycom_clipped_goa/", pattern="clipped_monthly_means_end", full.names=TRUE)

means_dat <- readRDS(file=filelist2[i])[1,]

i<-1
for(i in 1:length(filelist2)){
  tempm <- readRDS(file=filelist2[i])
  
  #IMPORTANT remove this if statement if not running using Krista's downloaded files!
  if(i==3){print("removing 2003 June double observation") 
    tempm <- tempm[-c(1:5),]}
  
  m_list <- list(means_dat, tempm)
  means_dat <- data.table::rbindlist(m_list)
  print(i)
}
beep(2)
means_dat <- means_dat[which(year>1500),]
means_dat <- means_dat[duplicated(means_dat)==FALSE,]

write.csv(means_dat, file="/Volumes/USB DISK/hycom_clipped_goa/hycom_monthly_means.csv")
write.csv(means_dat, file="data/hycom_monthly_means.csv")
write.csv(means_dat, file="/Users/krista/Dropbox/Work folder/Pacific cod/Analysis/warm_pcod/data/hycom_monthly_means.csv")

means_dat <- read.csv(file="data/hycom_monthly_means.csv", row.names = 1)



ggplot(means_dat, aes(year, mean_monthly, col=depth_cat)) + facet_wrap(~month) + geom_point() + geom_line()

View(means_dat[which(means_dat$month==6),])


#seasonal means=====

means_dat$season <- "NA"
means_dat$season[which(means_dat$month>10|
                         means_dat$month<4)] <- "winter"
means_dat$season[which(means_dat$month>3 &
                         means_dat$month<7)] <- "spring"
means_dat$season[which(means_dat$month==7 |
                         means_dat$month==8)] <- "summer"
means_dat$season[which(means_dat$month==9 |
                         means_dat$month==10)] <- "fall"

means_dat$season_year <- means_dat$year

i <- 1
for(i in 1:length(means_dat$year)){
  temprow <- means_dat[i,]
  tempyear <- temprow$year
  if(temprow$month > 10)
  {
    means_dat$season_year[i] <- tempyear + 1
  }
}

season_means <- means_dat %>% group_by(season_year, season, depth_cat) %>%
  summarise(seasonal_hycom_mean=mean(mean_monthly, na.rm=TRUE))

ggplot(season_means, aes(season_year, seasonal_hycom_mean, col=depth_cat)) + facet_wrap(~season) + geom_point() + geom_line()

write.csv(season_means, file="/Volumes/USB DISK/hycom_clipped_goa/hycom_season_means.csv")
write.csv(season_means, file="data/hycom_season_means.csv")
write.csv(season_means, file="/Users/krista/Dropbox/Work folder/Pacific cod/Analysis/warm_pcod/data/hycom_season_means.csv")


means_dat$spawning <- NA
means_dat$spawning[which(means_dat$month<5 &
                           means_dat$month>2)] <- "spawning_season"
means_dat$spawning[which(means_dat$month<3)] <- "prespawning_season"
means_dat$spawning[which(means_dat$month>4)] <- "not_spawning"

sp_season_means <- means_dat %>% group_by(year, spawning, depth_cat) %>%
  summarise(spawn_season_hycom_mean=mean(mean_monthly, na.rm=TRUE))

ggplot(sp_season_means, aes(year, spawn_season_hycom_mean, col=depth_cat)) + facet_wrap(~spawning) + geom_point() + geom_line()

write.csv(sp_season_means, file="/Volumes/USB DISK/hycom_clipped_goa/hycom_spawnseason_means.csv")
write.csv(sp_season_means, file="data/hycom_spawnseason_means.csv")
write.csv(sp_season_means, file="/Users/krista/Dropbox/Work folder/Pacific cod/Analysis/warm_pcod/data/hycom_spawnseason_means.csv")


