#=====================================================================================
#Plot comparing temp metrics for 2024 assessment presentations and manuscript

#By Krista Oke, Sept 2024
#=====================================================================================
library(tidyverse)
library(cowplot)
#=====================================================================================

#get data

months_scaled <- read.csv("data/scaled_temp_by_month.csv", row.names = 1)

months_scaled_long <- months_scaled %>% pivot_longer(names_to = "type", values_to = 'scaled_temp', -c(Year, Month,  depth))

jun_plot_dat <- months_scaled_long[which(months_scaled_long$Month=="6"),]
jun_plot_dat <- jun_plot_dat[which(jun_plot_dat$type=="cfsr_temp"|
                                     jun_plot_dat$type=="mean_monthly" |
                                     jun_plot_dat$type=="AFSC_BTS" ),]
jun_plot_dat <- jun_plot_dat[which(jun_plot_dat$depth=="50m"),]

p2 <- ggplot(jun_plot_dat, 
       aes(Year, scaled_temp, col=type)) + geom_point() + geom_line() + 
  facet_wrap(~depth) + theme_bw() + 
  scale_color_manual(labels = c("AFSC bottom trawl", "CFSR", "HYCOM"), 
                     values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(y="Temperature (°C)", color="Metric")

#look at ALL metrics june 50m

jun_plot_all <- months_scaled_long[which(months_scaled_long$Month=="6"),]
#jun_plot_all <- jun_plot_all[which(jun_plot_all$depth=="50m"),]

jun_plot_all$type[which(jun_plot_all$type=="mean_monthly")] <- "hycom"
jun_plot_all$type[which(jun_plot_all$type=="gak_best_temp")] <- "gak"
jun_plot_all$type[which(jun_plot_all$type=="cfsr_temp")] <- "cfsr"

jun_plot_all <- jun_plot_all %>%
  mutate(depth = factor(depth, levels = c("50m", "100m", "150m")))

p4 <- ggplot(jun_plot_all[which(jun_plot_all$type!="AFSC_LLS"&
                                  jun_plot_all$Year>1989&
                                  jun_plot_all$depth!="40-60m" &
                                  jun_plot_all$depth!="90-100m"),], 
             aes(Year, scaled_temp, fill=type)) +  geom_line(aes( col=type)) + geom_point(pch=21) +
   facet_wrap(~depth) + theme_bw()  +
  #scale_color_brewer(palette = "RdYlBu") + 
  scale_fill_brewer(palette = "RdYlBu") +
scale_color_manual(values = c("#d7191c", "#fd8d3c", "#cccccc", "#abd9e9", "#2c7bb6")) +
  labs(y="Scaled temperature (°C)", color="Metric", fill="Metric") + facet_wrap(~depth, nrow=3)

#just 50m
ggplot(jun_plot_all[which(jun_plot_all$type!="AFSC_LLS"&
                            jun_plot_all$Year>1989&
                            jun_plot_all$depth=="50m"),], 
       aes(Year, scaled_temp, fill=type)) +  geom_line(aes( col=type)) + geom_point(pch=21) +
  facet_wrap(~depth) + theme_bw()  +
  #scale_color_brewer(palette = "RdYlBu") + 
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_manual(values = c("#d7191c", "#fd8d3c", "#cccccc", "#abd9e9", "#2c7bb6")) +
  labs(y="Scaled temperature (°C)", color="Temperature metric", fill="Temperature metric") + facet_wrap(~depth, nrow=3)

#not scaled----

month_combined <- read.csv(paste0(wd,"/data/monthly_temp_depth_gak_cfsr_hycom_surv_dataset.csv"))

months_long <- month_combined %>% pivot_longer(names_to = "type", values_to = 'temp', -c(Year, Month, depth))

ggplot(months_long[which(months_long$Month=="6"),], aes(Year, temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)

raw_jun_plot_dat <- months_long[which(months_long$Month=="6"),]
raw_jun_plot_dat <- raw_jun_plot_dat[which(raw_jun_plot_dat$type=="cfsr_temp"|
                                             raw_jun_plot_dat$type=="mean_monthly" |
                                             raw_jun_plot_dat$type=="AFSC_BTS"),]
raw_jun_plot_dat <- raw_jun_plot_dat[which(raw_jun_plot_dat$depth=="50m"),]

p1 <- ggplot(raw_jun_plot_dat, 
       aes(Year, temp, col=type)) + geom_point() + geom_line() + 
  facet_wrap(~depth) + theme_bw() + 
  scale_color_manual(labels = c("AFSC bottom trawl", "CFSR", "HYCOM"), 
                     values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(y="Temperature (°C)", color="Metric")

plot_grid(p1, p2, nrow=2)

#look at ALL metrics june 50m

raw_jun_plot_all <- months_long[which(months_long$Month=="6"),]
#raw_jun_plot_all <- raw_jun_plot_all[which(raw_jun_plot_all$depth=="50m"),]

raw_jun_plot_all$type[which(raw_jun_plot_all$type=="mean_monthly")] <- "hycom"
raw_jun_plot_all$type[which(raw_jun_plot_all$type=="gak_best_temp")] <- "gak"
raw_jun_plot_all$type[which(raw_jun_plot_all$type=="cfsr_temp")] <- "cfsr"

raw_jun_plot_all <- raw_jun_plot_all %>%
  mutate(depth = factor(depth, levels = c("50m", "100m", "150m")))

p3 <- ggplot(raw_jun_plot_all[which(raw_jun_plot_all$type!="AFSC_LLS"&
                                      raw_jun_plot_all$Year>1989 &
                                    jun_plot_all$depth!="40-60m" &
                                      jun_plot_all$depth!="90-100m"),], 
             aes(Year, temp, fill=type)) +  geom_line(aes( col=type)) + geom_point(pch=21) +
  facet_wrap(~depth) + theme_bw()  +
  #scale_color_brewer(palette = "RdYlBu") + 
  scale_fill_brewer(palette = "RdYlBu")+ 
 scale_color_manual(values = c("#d7191c", "#fd8d3c", "#cccccc", "#abd9e9", "#2c7bb6")) +
labs(y="Temperature (°C)", color="Metric", fill="Metric") + facet_wrap(~depth, nrow=3)

plot_grid(p3, p4, nrow=2)


#plot demeaned for SI===========

months_demeaned <- read.csv("data/demeaned_temp_by_month.csv", row.names = 1)

months_demeaned_long <- months_demeaned %>% pivot_longer(names_to = "type", values_to = 'scaled_temp', -c(Year, Month, depth)) #season, season_year, spawning, depth))

ggplot(months_demeaned_long[which(months_demeaned_long$Month=="6"),], aes(Year, scaled_temp, col=type)) + geom_point() + geom_line() + facet_wrap(~depth)


dm_plot_dat <- months_demeaned_long[which(months_demeaned_long$Month=="6"),]
dm_plot_dat <- dm_plot_dat[which(dm_plot_dat$type=="cfsr_temp"|
                                             dm_plot_dat$type=="mean_monthly" |
                                             dm_plot_dat$type=="gak_best_temp"),]
dm_plot_dat <- dm_plot_dat[which(dm_plot_dat$depth!="40-60m"&
                                   dm_plot_dat$depth!="90-100m"),]
dm_plot_dat <- dm_plot_dat[which(dm_plot_dat$Year>1976),]

dm_plot_dat <- dm_plot_dat %>%
  mutate(depth = factor(depth, levels = c("50m", "100m", "150m")))

p9 <- ggplot(dm_plot_dat, 
             aes(Year, scaled_temp, col=type)) + geom_point() + geom_line() + 
  facet_wrap(~depth, nrow=3) + theme_bw() + 
  scale_color_manual(labels = c("CFSR", "GAK1", "HYCOM"),
                     values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(y="Demeaned temperature (°C)", color="Metric")
p9





