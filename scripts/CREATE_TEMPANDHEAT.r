#  Created: 06/30/2023
#  Author: Steve.Barbeaux@noaa.gov
# Creates the TEMPANDHEAT.csv data for the assessment

#EDITED by Krista 02/23/2024 trying to run script to get SST and heatwave data
#--------------------------------------------------------------------------------
## set default directory
### This will need to be changed...
#dir1="C:/Users/steve.barbeaux/Work/GitHub/CENTRAL_GOA_HEATWAVE"
dir1 <- getwd()

#  Load R libraries
library(ncdf4)
library(curl)
library(lubridate)
library(PCICt)
library(data.table)
library(sp)
library(rgdal)
library(heatwaveR)
library(dplyr)
library(png)
library(ggpubr)
library(extrafont)
library(raster)
library(zoo)
library(scales)
library(tidyverse)
library(tidync)


#----------------------------------------------------------------------------------------------------------
#  This chunk should not need to be run again. It downloads data from 9/1/1981 to 5/16/2020 and might not work anymore as 
#----------------------------------------------------------------------------------------------------------

#  Download the data for a fixed spatial and temporal period.
#  (note this is a lot of data and will take a few minutes to download if you do the whole thing)

# options(timeout = max(300, getOption("timeout")))

# setwd(paste0(dir1,"/old_files"))

# download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst%5B(1981-09-01T12:00:00Z):1:(1985-12-31T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(52):1:(62)%5D%5B(200):1:(215)%5D"),
#                method = "auto", mode="wb",destfile = "x81_85.nc")

# download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst%5B(1986-01-01T12:00:00Z):1:(1990-12-31T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(52):1:(62)%5D%5B(200):1:(215)%5D"),
#                method = "auto", mode="wb",destfile = "x86_90.nc")

# download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst%5B(1991-01-01T12:00:00Z):1:(1995-12-31T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(52):1:(62)%5D%5B(200):1:(215)%5D"),
#                method = "auto", mode="wb",destfile = "x91_95.nc")

# download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst%5B(1996-01-01T12:00:00Z):1:(2000-12-31T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(52):1:(62)%5D%5B(200):1:(215)%5D"),
#                method = "auto", mode="wb",destfile = "x96_00.nc")

# download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst%5B(2001-01-01T12:00:00Z):1:(2005-12-31T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(52):1:(62)%5D%5B(200):1:(215)%5D"),
#                method = "auto", mode="wb",destfile = "x01_05.nc")

# download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst%5B(2006-01-01T12:00:00Z):1:(2010-12-31T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(52):1:(62)%5D%5B(200):1:(215)%5D"),
#                method = "auto", mode="wb",destfile = "x06_10.nc")

# download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst%5B(2011-01-01T12:00:00Z):1:(2015-12-31T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(52):1:(62)%5D%5B(200):1:(215)%5D"),
#                method = "auto", mode="wb",destfile = "x11_15.nc")

# download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst%5B(2016-01-01T12:00:00Z):1:(2020-12-31T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(52):1:(62)%5D%5B(200):1:(215)%5D"),
#                method = "auto", mode="wb",destfile = "x16_20.nc")

# download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst%5B(2021-01-01T12:00:00Z):1:(2022-12-31T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(52):1:(62)%5D%5B(200):1:(215)%5D"),
#                method = "auto", mode="wb",destfile = "x21_22.nc")


# ## in the central Gulf of Alaska between 160°W and 145°W longitude 52N and 62N latitude.

  files=list.files(path=paste0(dir1,"/CENTRAL_GOA_HEATWAVE-main/old_files"), pattern=".nc") #K note: had to play with path to get this to run
 
  buffer<-readOGR(dsn=path.expand(paste0(dir1,"/CENTRAL_GOA_HEATWAVE-main")),layer="CENTRALGOA_CLIP") #K note: had to play with path to get this to run

  meanSST<-vector("list",length=length(files))
  for(i in 1:length(files)){
  	pre1.brick = brick(paste0(dir1,"/CENTRAL_GOA_HEATWAVE-main/old_files/",files[i])) #more editing path to run
  	pre1.brick = rotate(pre1.brick)
  	shp = spTransform(buffer, crs(pre1.brick))
  	pre1.mask = mask(pre1.brick, shp)
  	pre1.df = as.data.frame(pre1.mask, xy=TRUE)
  	pre1.df = pre1.df[complete.cases(pre1.df),]
  	SST=colSums(pre1.df[,3:ncol(pre1.df)])/nrow(pre1.df)

  	dates<-tidync(paste0(dir1,"/CENTRAL_GOA_HEATWAVE-main/old_files/",files[i])) %>% #path edited
  	     hyper_tibble() %>%
    	     mutate(date=as_datetime(time))

  	meanSST[[i]]<-data.frame(day=unique(dates$date),SST=SST)
  }

  meanSST<-data.table(do.call(rbind,meanSST))
  meanSST<-meanSST[order(day),]

   meanSST$day1<-as.Date(meanSST$day)

# setwd(dir1)

#  img=readPNG("FISHERIES Wide 360px.png")
#  rast <- grid::rasterGrob(img, interpolate = T)
#  save.image('Heatwave_figure.RData')

#####################################################################################################
setwd(dir1)
load('Heatwave_figure.RData')

file=list.files(path=paste0(dir1,"/CENTRAL_GOA_HEATWAVE-main/"), pattern=".nc") #more editing paths
file.remove(file)


download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.nc?sst%5B(2023-01-01T12:00:00Z):1:(last)%5D%5B(0.0):1:(0.0)%5D%5B(52):1:(62)%5D%5B(200):1:(215)%5D"),
               method = "auto", mode="wb",destfile = "latest_OISST.nc")


if(!file.exists("latest_OISST.nc")) {stop("File did not download")}

file=list.files(path=paste0(dir1,"/CENTRAL_GOA_HEATWAVE-main/"), pattern=".nc")  #more editing paths

## pull the .nc file, clip it using the shape file to the 300m isobath in central GOA, and calculate the daily mean
pre1.brick = brick(paste0(dir1,"/CENTRAL_GOA_HEATWAVE-main/", file)) %>% rotate()
shp = spTransform(buffer, crs(pre1.brick))
pre1.mask = mask(pre1.brick, shp)


pre1.df = as.data.frame(pre1.mask, xy=TRUE)
pre1.df = pre1.df[complete.cases(pre1.df),]
SST=colSums(pre1.df[,3:ncol(pre1.df)])/nrow(pre1.df)
	
dates<-tidync(paste0(dir1,"/CENTRAL_GOA_HEATWAVE-main/", file)) %>% hyper_tibble() %>% mutate(date=as_datetime(time))  ## create list of dates

meanSST2<-data.frame(day=unique(dates$date),SST=SST)
meanSST2<-data.table(meanSST2)
meanSST2<-meanSST2[order(day),]
meanSST2$day1<-as.Date(meanSST2$day)
meanSST<-rbind(meanSST,meanSST2)

hobday=ts2clm(meanSST, x =day1, y = SST, climatologyPeriod=c("1982-01-01", "2012-12-31"),pctile=90)
event=data.table(detect_event(hobday, x = day1, y = SST, coldSpells=FALSE,minDuration = 5,joinAcrossGaps = TRUE,maxGap=2)$event)
event$date_start<-as.Date(event$date_start,format="%Y-%d-%m")
event$date_end<-as.Date(event$date_end,format="%Y-%d-%m")
event$day1<-event$date_start
event$day2<-event$date_end
event[is.na(event$day2)] <- as.Date(Sys.Date(),format="%Y-%d-%m")
event$day_end<-event$date_end-event$day1


## Heatwave Index Calcs (long drawnout defining of periods within the events to calculate indices by year and seasonal cutoffs pertinent to cod...)

events1<-event[,c(5,6,8,9)]
events1$YS<-year(events1$date_start)
events1$YE<-year(events1$date_end)
events1$ME<-month(events1$date_end)
events1$MS<-month(events1$date_start)

events1.1<-events1[YS==YE & MS%in% 1:3 & ME %in% 1:3]

events1.2<-events1[YS==YE & MS%in% 4:9 & ME %in% 4:9]

events1.3<-events1[YS==YE & MS%in% 10:12 & ME %in% 10:12]


eventsYYMM4<-events1[YS==YE & MS%in% 1:3 & ME %in% 4:9]
    eventsYYMM4.1<-data.table(duration=0, date_start=eventsYYMM4$date_start, 
                  date_end=as.Date(paste0(eventsYYMM4$YS,"-03-31"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM4$intensity_mean,   
                  YS = eventsYYMM4$YS,  
                  YE = eventsYYMM4$YE,
                  ME = 3,
                  MS = eventsYYMM4$MS)
    eventsYYMM4.2<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM4$YS,"-04-01"),format="%Y-%m-%d"), 
                  date_end=eventsYYMM4$date_end, 
                  intensity_mean=eventsYYMM4$intensity_mean,   
                  YS = eventsYYMM4$YS,  
                  YE = eventsYYMM4$YE,
                  ME = eventsYYMM4$ME,
                  MS = 4)
    eventsYYMM4<-rbind(eventsYYMM4.1,eventsYYMM4.2)  


eventsYYMM5<-events1[YS==YE & MS %in% 4:9 & ME %in% 10:12]
    eventsYYMM5.1<-data.table(duration=0, date_start=eventsYYMM5$date_start, 
                  date_end=as.Date(paste0(eventsYYMM5$YS,"-09-30"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM5$intensity_mean,   
                  YS = eventsYYMM5$YS,  
                  YE = eventsYYMM5$YS,
                  ME = 9,
                  MS = eventsYYMM5$MS)
    eventsYYMM5.2<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM5$YS,"-10-01"),format="%Y-%m-%d"), 
                  date_end=eventsYYMM5$date_end, 
                  intensity_mean=eventsYYMM5$intensity_mean,   
                  YS = eventsYYMM5$YE,  
                  YE = eventsYYMM5$YE,
                  ME = eventsYYMM5$ME,
                  MS = 10)
    eventsYYMM5<-rbind(eventsYYMM5.1,eventsYYMM5.2) 
 

eventsYYMM6<-events1[YE==YS+1 & MS %in% 10:12 & ME %in% 1:3]
    eventsYYMM6.1<-data.table(duration=0, date_start=eventsYYMM6$date_start, 
                  date_end=as.Date(paste0(eventsYYMM6$YS,"-12-31"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM6$intensity_mean,   
                  YS = eventsYYMM6$YS,  
                  YE = eventsYYMM6$YS,
                  ME = 12,
                  MS = eventsYYMM6$MS)
    eventsYYMM6.2<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM6$YE,"-01-01"),format="%Y-%m-%d"), 
                  date_end=eventsYYMM6$date_end, 
                  intensity_mean=eventsYYMM6$intensity_mean,   
                  YS = eventsYYMM6$YE,  
                  YE = eventsYYMM6$YE,
                  ME = eventsYYMM6$ME,
                  MS = 1)

    eventsYYMM6<-rbind(eventsYYMM6.1,eventsYYMM6.2)

eventsYYMM7<-events1[YE==YS+1 & MS %in% 4:9 & ME %in% 1:3]
if(nrow(eventsYYMM7)>0){
    eventsYYMM7.1<-data.table(duration=0, date_start=eventsYYMM7$date_start, 
                  date_end=as.Date(paste0(eventsYYMM7$YS,"-09-30"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM7$intensity_mean,   
                  YS = eventsYYMM7$YS,  
                  YE = eventsYYMM7$YS,
                  ME = 9,
                  MS = eventsYYMM7$MS)
    eventsYYMM7.2<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM7$YS,"-10-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM7$YS,"-12-31"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM7$intensity_mean,   
                  YS = eventsYYMM7$YS,  
                  YE = eventsYYMM7$YS,
                  ME = 12,
                  MS = 10)
    eventsYYMM7.3<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM7$YE,"-01-01"),format="%Y-%m-%d"), 
                  date_end=eventsYYMM7$date_end, 
                  intensity_mean=eventsYYMM7$intensity_mean,   
                  YS = eventsYYMM7$YE,  
                  YE = eventsYYMM7$YE,
                  ME = eventsYYMM7$ME,
                  MS = 1)
    eventsYYMM7<-rbind(eventsYYMM7.1,eventsYYMM7.2,eventsYYMM7.3)
}


eventsYYMM8<-events1[YE==YS+1 & MS %in% 1:3 & ME %in% 1:3]
if(nrow(eventsYYMM8)>0){
    eventsYYMM8.1<-data.table(duration=0, date_start=eventsYYMM8$date_start, 
                  date_end=as.Date(paste0(eventsYYMM8$YS,"-03-31"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM8$intensity_mean,   
                  YS = eventsYYMM8$YS,  
                  YE = eventsYYMM8$YS,
                  ME = 3,
                  MS = eventsYYMM8$MS)
    eventsYYMM8.2<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM8$YS,"-04-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM8$YS,"-09-30"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM8$intensity_mean,   
                  YS = eventsYYMM8$YS,  
                  YE = eventsYYMM8$YS,
                  ME = 9,
                  MS = 4)
    eventsYYMM8.3<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM8$YS,"-10-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM8$YS,"-12-31"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM8$intensity_mean,   
                  YS = eventsYYMM8$YS,  
                  YE = eventsYYMM8$YS,
                  ME = 12,
                  MS = 10)
    eventsYYMM8.4<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM8$YE,"-01-01"),format="%Y-%m-%d"), 
                  date_end=eventsYYMM8$date_end, 
                  intensity_mean=eventsYYMM8$intensity_mean,   
                  YS = eventsYYMM8$YE,  
                  YE = eventsYYMM8$YE,
                  ME = eventsYYMM8$ME,
                  MS = 1)
    eventsYYMM8<-rbind(eventsYYMM8.1,eventsYYMM8.2,eventsYYMM8.3,eventsYYMM8.4)
    }



eventsYYMM12<-events1[YE==YS+1 & MS %in% 10:13 & ME %in% 4:9]
if(nrow(eventsYYMM12)>0){
    
    eventsYYMM12.1<-data.table(duration=0, date_start=eventsYYMM12$date_start, 
                  date_end=as.Date(paste0(eventsYYMM12$YS,"-12-31"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM12$intensity_mean,   
                  YS = eventsYYMM12$YS,  
                  YE = eventsYYMM12$YS,
                  ME = 12,
                  MS = eventsYYMM12$MS)

    eventsYYMM12.2<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM12$YE,"-01-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM12$YE,"-03-31"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM12$intensity_mean,   
                  YS = eventsYYMM12$YE,  
                  YE = eventsYYMM12$YE,
                  ME = 3,
                  MS = 1)
    
    eventsYYMM12.3<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM12$YE,"-04-01"),format="%Y-%m-%d"), 
                  date_end=eventsYYMM12$date_end, 
                  intensity_mean=eventsYYMM12$intensity_mean,   
                  YS = eventsYYMM12$YE,  
                  YE = eventsYYMM12$YE,
                  ME = eventsYYMM12$ME,
                  MS = 4)
    eventsYYMM12<-rbind(eventsYYMM12.1,eventsYYMM12.2,eventsYYMM12.3)
    }


eventsYYMM9<-events1[YE==YS+1 & MS %in% 4:9 & ME %in% 4:9]
if(nrow(eventsYYMM9)>0){
    eventsYYMM9.1<-data.table(duration=0, date_start=eventsYYMM9$date_start, 
                  date_end=as.Date(paste0(eventsYYMM9$YS,"-09-30"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM9$intensity_mean,   
                  YS = eventsYYMM9$YS,  
                  YE = eventsYYMM9$YS,
                  ME = 9,
                  MS = eventsYYMM9$MS)
    eventsYYMM9.2<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM9$YS,"-10-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM9$YS,"-12-31"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM9$intensity_mean,   
                  YS = eventsYYMM9$YS,  
                  YE = eventsYYMM9$YS,
                  ME = 12,
                  MS = 10)
    eventsYYMM9.3<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM9$YE,"-01-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM9$YE,"-03-30"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM9$intensity_mean,   
                  YS = eventsYYMM9$YE,  
                  YE = eventsYYMM9$YE,
                  ME = 3,
                  MS = 1)
    eventsYYMM9.4<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM9$YE,"-04-01"),format="%Y-%m-%d"), 
                  date_end=eventsYYMM9$date_end, 
                  intensity_mean=eventsYYMM9$intensity_mean,   
                  YS = eventsYYMM9$YE,  
                  YE = eventsYYMM9$YE,
                  ME = eventsYYMM9$ME,
                  MS = 4)

    eventsYYMM9<-rbind(eventsYYMM9.1,eventsYYMM9.2,eventsYYMM9.3,eventsYYMM9.4)
    }


eventsYYMM10<-events1[YE==YS+1 & MS %in% 4:9 & ME %in% 10:12]
if(nrow(eventsYYMM10)>0){
    eventsYYMM10.1<-data.table(duration=0, date_start=eventsYYMM10$date_start, 
                  date_end=as.Date(paste0(eventsYYMM10$YS,"-09-30"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM10$intensity_mean,   
                  YS = eventsYYMM10$YS,  
                  YE = eventsYYMM10$YS,
                  ME = 10,
                  MS = eventsYYMM10$MS)
    eventsYYMM10.2<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM10$YS,"-10-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM10$YS,"-12-31"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM10$intensity_mean,   
                  YS = eventsYYMM10$YS,  
                  YE = eventsYYMM10$YS,
                  ME = 12,
                  MS = 10)
    eventsYYMM10.3<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM10$YE,"-01-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM10$YE,"-03-30"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM10$intensity_mean,   
                  YS = eventsYYMM10$YE,  
                  YE = eventsYYMM10$YE,
                  ME = 3,
                  MS = 1)
    eventsYYMM10.4<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM10$YE,"-04-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM10$YE,"-09-30"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM10$intensity_mean,   
                  YS = eventsYYMM10$YE,  
                  YE = eventsYYMM10$YE,
                  ME = 9,
                  MS = 4)
    eventsYYMM10.5<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM10$YE,"-10-01"),format="%Y-%m-%d"), 
                  date_end=eventsYYMM10$date_end, 
                  intensity_mean=eventsYYMM10$intensity_mean,   
                  YS = eventsYYMM10$YE,  
                  YE = eventsYYMM10$YE,
                  ME = eventsYYMM10$ME,
                  MS = 10)

    eventsYYMM10<-rbind(eventsYYMM10.1,eventsYYMM10.2,eventsYYMM10.3,eventsYYMM10.4)
    }

eventsYYMM11<-events1[YE==YS+2 & MS %in% 10:12 & ME %in% 1:3]
    eventsYYMM11.1<-data.table(duration=0, date_start=eventsYYMM11$date_start, 
                  date_end=as.Date(paste0(eventsYYMM11$YS,"-12-31"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM11$intensity_mean,   
                  YS = eventsYYMM11$YS,  
                  YE = eventsYYMM11$YS,
                  ME = 12,
                  MS = eventsYYMM11$MS)
    eventsYYMM11.2<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM11$YS+1,"-01-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM11$YS+1,"-03-30"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM11$intensity_mean,   
                  YS = eventsYYMM11$YS+1,  
                  YE = eventsYYMM11$YS+1,
                  ME = 3,
                  MS = 1)
    eventsYYMM11.3<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM11$YS+1,"-04-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM11$YS+1,"-09-30"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM11$intensity_mean,   
                  YS = eventsYYMM11$YS+1,  
                  YE = eventsYYMM11$YS+1,
                  ME = 9,
                  MS = 4)
    eventsYYMM11.4<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM11$YS+1,"-10-01"),format="%Y-%m-%d"), 
                  date_end=as.Date(paste0(eventsYYMM11$YS+1,"-12-31"),format="%Y-%m-%d"), 
                  intensity_mean=eventsYYMM11$intensity_mean,   
                  YS = eventsYYMM11$YS+1,  
                  YE = eventsYYMM11$YS+1,
                  ME = 12,
                  MS = 10)
    eventsYYMM11.5<-data.table(duration=0, date_start=as.Date(paste0(eventsYYMM11$YE,"-01-01"),format="%Y-%m-%d"), 
                  date_end=eventsYYMM11$date_end, 
                  intensity_mean=eventsYYMM11$intensity_mean,   
                  YS = eventsYYMM11$YE,  
                  YE = eventsYYMM11$YE,
                  ME = eventsYYMM11$ME,
                  MS = 1)
eventsYYMM11<-rbind(eventsYYMM11.1,eventsYYMM11.2,eventsYYMM11.3,eventsYYMM11.4,eventsYYMM11.5)

events2<-rbind(eventsYYMM4,eventsYYMM5,eventsYYMM6,eventsYYMM7,eventsYYMM8,eventsYYMM9,eventsYYMM10,eventsYYMM11,eventsYYMM12)


events2$SEASON="Winter"
events2[MS%in%c(4:9)]$SEASON="Summer"
events2$duration=as.numeric(1+(events2$date_end-events2$date_start))
events2$Intensity_Total=events2$duration*events2$intensity_mean
events2$Intensity_Winter=0
events2$Intensity_Summer=0
events2[SEASON=='Winter']$Intensity_Winter=events2[SEASON=='Winter']$Intensity_Total
events2[SEASON=='Summer']$Intensity_Summer=events2[SEASON=='Summer']$Intensity_Total

events3<-events1[YS==YE]

events3<-data.table(duration=0, date_start=events3$date_start, 
                  date_end=events3$date_end, 
                  intensity_mean=events3$intensity_mean,   
                  YS = events3$YS,  
                  YE = events3$YE,
                  ME = events3$ME,
                  MS = events3$MS)


events3$SEASON="Winter"
events3$Intensity_Total=0
events3$Intensity_Summer=0
events3$Intensity_Winter=0
events3$duration=as.numeric(1+(events3$date_end-events3$date_start))
events3$Intensity_Total=events3$duration*events3$intensity_mean


eventsYYMM1<-events3[MS %in% 1:3 & ME%in% 1:3]
eventsYYMM1$Intensity_Winter=eventsYYMM1$Intensity_Total

eventsYYMM2<-events3[MS %in% 10:12 & ME %in% 10:12]
eventsYYMM2$Intensity_Winter=eventsYYMM2$Intensity_Total

eventsYYMM3<-events3[MS %in% 4:9 & ME %in% 4:9]
eventsYYMM3$SEASON="Summer"
eventsYYMM3$Intensity_Summer=eventsYYMM3$Intensity_Total

events3<-rbind(eventsYYMM1,eventsYYMM2,eventsYYMM3)


events4<-rbind(events2,events3)

##Leap_year_correction
events4[year(date_start)==2016&duration==90]$duration=91
events4[year(date_start)==2016&duration==90]$Intensity_Total=91*1.7284
events4[year(date_start)==2016&duration==90]$Intensity_Winter=91*1.7284

events4$Intensity_CodRec<-0
events4[ME%in%c(2:3)]$Intensity_CodRec<-events4[ME%in%c(2:3)]$Intensity_Total


c_year<-year(Sys.time())

MHWI<-events4[,list(Annual=sum(Intensity_Total),Summer=sum(Intensity_Summer),Winter=sum(Intensity_Winter),Spawning=sum(Intensity_CodRec)),by="YS"]
names(MHWI)[1]<-"Year"
x<-data.table(Year=c(1982:c_year))
MHWI=merge(MHWI,x,all.y=T)
MHWI[is.na(Winter)]$Winter<-0
MHWI[is.na(Summer)]$Summer<-0
MHWI[is.na(Annual)]$Annual<-0
MHWI[is.na(Spawning)]$Spawning<-0
write.csv(MHWI,"MHWI.csv",row.names=F)

### creating files for stock assessment
#DO NOT RUN for warm_pcod-------

# HW<-data.table(read.csv('MHWI.csv'))
# CFSR<-data.table(read.csv('raw_cfsr.csv'))
# CFSR1<-data.table(YR=CFSR[Month==2]$Year,JUNE_TEMP=CFSR[Month==6]$X0_20,FEB_TEMP=CFSR[Month==2]$X0_20)
# CFSR1$TEMP<-CFSR1$JUNE_TEMP-mean(CFSR1[YR%in%c(1982:2012)]$JUNE_TEMP)
# HeatWave<-data.table(YR=HW$Year,THW=HW$Annual,WHW=HW$Winter,SHW=HW$Spawning)

# TEMP_HEAT<-merge(CFSR1,HeatWave, all.x=T)
# TEMP_HEAT[is.na(TEMP_HEAT)]<-0

# write.csv(TEMP_HEAT,'TEMPANDHEAT.csv',row.names =FALSE)

# meanSST_cfsr<-data.table(meanSST)
# meanSST_cfsr$MONTH<-month(meanSST_cfsr$day)
# meanSST_cfsr$YR<-year(meanSST_cfsr$day)
# 
# SST2<-meanSST_cfsr[,list(MEANSST=mean(SST)),by=c('YR','MONTH')]
# 
# HW<- MHWI 
# 
# CFSR1<-data.table(YR=SST2[MONTH==2]$YR,JUNE_TEMP=SST2[MONTH==6]$MEANSST,FEB_TEMP=SST2[MONTH==2]$MEANSST)
# CFSR1$TEMP<-CFSR1$JUNE_TEMP-mean(CFSR1[YR%in%c(1982:2012)]$JUNE_TEMP)
# HeatWave<-data.table(YR=HW$Year,THW=HW$Annual,WHW=HW$Winter,SHW=HW$Spawning)
# 
# TEMP_HEAT<-merge(CFSR1,HeatWave, all.x=T)
# TEMP_HEAT[is.na(TEMP_HEAT)]<-0
# 
# write.csv(TEMP_HEAT,'TEMPANDHEAT.csv',row.names =FALSE)
# 
# 
# 




#SST calculation for warm_pcod============================================================

#added by Krista to get monthly SSTs

meanSST_2edit <- meanSST %>% dplyr::mutate(year = lubridate::year(day1), 
                                           month = lubridate::month(day1), 
                                           day = lubridate::day(day1))

monthlymeanSST <- meanSST_2edit %>% group_by(year, month) %>%
                    summarise(monthly_mean=mean(SST, na.rm=TRUE))

monthlymeanSST <- monthlymeanSST
monthlymeanSST$season <- NA
monthlymeanSST$season[which(monthlymeanSST$month==1|monthlymeanSST$month==2|monthlymeanSST$month==3)] <- "jan_feb_mar"
monthlymeanSST$season[which(monthlymeanSST$month==4|monthlymeanSST$month==5|monthlymeanSST$month==6)] <- "apr_may_jun"
monthlymeanSST$season[which(monthlymeanSST$month==7|monthlymeanSST$month==8|monthlymeanSST$month==9)] <- "jun_aug_sep"
monthlymeanSST$season[which(monthlymeanSST$month==10|monthlymeanSST$month==11|monthlymeanSST$month==12)] <- "oct_nov_dec"

seasonalmeanSST <- monthlymeanSST %>% group_by(year, season) %>%
  summarise(seasonal_mean=mean(monthly_mean, na.rm=TRUE))

write.csv(seasonalmeanSST,"seasonalmeanSST.csv",row.names=F)





