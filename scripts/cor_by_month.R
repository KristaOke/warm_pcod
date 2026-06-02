#========================================================================================================
#Correlations by month
#
#Krista, Oct 2024
#========================================================================================================
#Notes
#========================================================================================================

library(tidyverse)
library(corrplot)
library(Hmisc)
library(janitor)


#load data-------

wd <- getwd()
mnt_temp_dat <- read.csv(paste0(wd,"/data/scaled_temp_by_month.csv"), row.names = 1)

#this data is already scaled but needs to be converted to wide format

mnt_temp_dat <- mnt_temp_dat %>% rename(hycom='mean_monthly',
                                        gak='gak_best_temp',
                                        cfsr='cfsr_temp')

mnt_norange <- mnt_temp_dat[which(mnt_temp_dat$depth!="40-60m"&
                                    mnt_temp_dat$depth!="90-100m"),]
mnt_norange <- mnt_norange[,!names(mnt_norange) %in% 
                          c("AFSC_LLS")]




#mnt_norange <- mnt_norange[which(mnt_norange$Year!=1995 & mnt_norange$Year!=2003),] #REMOVE when hycom issue is resolved

mnt_wide <- mnt_norange %>% pivot_wider(names_from = c(depth), 
                                                  values_from = c(hycom, cfsr,
                                                       gak,
                                                     AFSC_BTS, #AFSC_LLS,
                                                       IPHC_FISS))
unique(duplicated(mnt_wide)) #if false that's good

mnt_wide <- mnt_wide[,!names(mnt_wide) %in% 
     c("cfsr_150m")]

mnt_wide$hycom_50m <- as.numeric(as.character(mnt_wide$hycom_50m ))   
mnt_wide$hycom_100m  <- as.numeric(as.character(mnt_wide$hycom_100m  ))    
mnt_wide$hycom_150m  <- as.numeric(as.character(mnt_wide$hycom_150m  ))      
mnt_wide$cfsr_50m  <- as.numeric(as.character(mnt_wide$cfsr_50m  ))       
mnt_wide$cfsr_100m  <- as.numeric(as.character(mnt_wide$cfsr_100m  )) 
mnt_wide$gak_50m  <- as.numeric(as.character(mnt_wide$gak_50m  )) 
mnt_wide$gak_100m  <- as.numeric(as.character(mnt_wide$gak_100m  ))     
mnt_wide$gak_150m  <- as.numeric(as.character(mnt_wide$gak_150m  )) 
mnt_wide$AFSC_BTS_50m  <- as.numeric(as.character(mnt_wide$AFSC_BTS_50m  ))       
mnt_wide$AFSC_BTS_100m  <- as.numeric(as.character(mnt_wide$AFSC_BTS_100m  ))    
mnt_wide$AFSC_BTS_150m  <- as.numeric(as.character(mnt_wide$AFSC_BTS_150m  ))  
mnt_wide$IPHC_FISS_50m  <- as.numeric(as.character(mnt_wide$IPHC_FISS_50m  ))       
mnt_wide$IPHC_FISS_100m  <- as.numeric(as.character(mnt_wide$IPHC_FISS_100m  ))    
mnt_wide$IPHC_FISS_150m  <- as.numeric(as.character(mnt_wide$IPHC_FISS_150m  )) 


#LOOP THROUGH MONTHS?

vecmonths <- c("jan", "feb", "mar", "apr", 
               "may", "jun", "jul", "aug",
               "sep", "oct", "nov", "dec")

i<-1
for(i in 1:12){
  print(i)
  temp.month <- vecmonths[i]
  temp.cov <- mnt_wide[which(mnt_wide$Month==i),!names(mnt_wide) %in% 
                         c("Year", "Month")]
  temp.cov <- temp.cov %>% remove_empty("cols") #need something additional here to remove really limited cols
  temp.cov <- temp.cov[, sapply(temp.cov, function(col) length(unique(col))) > 3]
cov.cor <- cor(temp.cov, use='pairwise.complete.obs')


pdf(file=paste0(wd,"/figs/temp_metrics_", temp.month, "_corr_plot.pdf"))

corrplot(cov.cor,order='AOE',  type = 'lower', method = 'number', number.cex=0.8, tl.cex=0.5, 
         mar = c(0,0,1,0),  number.digits = 2)

dev.off()


cortest <- rcorr(as.matrix(temp.cov))
cortest_r <- as.data.frame(cortest[1]) #correlations
cortest_n <- as.data.frame(cortest[2]) #sample sizes
cortest_p <- as.data.frame(cortest[3]) #p-values
write_csv(cortest_p, file=paste0(wd,"/output/correlations/temp_metrics_", temp.month, "_corr_test_pvals.csv"))
write_csv(cortest_n, file=paste0(wd,"/output/correlations/temp_metrics_", temp.month, "_corr_test_samplesizes.csv"))
write_csv(cortest_r, file=paste0(wd,"/output/correlations/temp_metrics_", temp.month, "_corr_test_corrs.csv"))
# 
# cortest_p <- read_csv( file=paste0(wd,"/output/correlations/temp_metrics_", temp.month, "_corr_test_pvals.csv"))
# cortest_n <- read_csv(file=paste0(wd,"/output/correlations/temp_metrics_", temp.month, "_corr_test_samplesizes.csv"))
# cortest_r <- read_csv(file=paste0(wd,"/output/correlations/temp_metrics_", temp.month, "_corr_test_corrs.csv"))


#can I add pval to plot? Trying w ggplot based on a stackoverflow

#need to convert to long format first
cors_df <- cortest_r
colnames(cors_df)<-str_remove(colnames(cors_df), "r.")
cors_df <- cbind(rowvars=rownames(cors_df), data.frame(cors_df)) %>% 
  gather(colvars, corr, -rowvars) 

ns_df <- cortest_n
colnames(ns_df)<-str_remove(colnames(ns_df), "n.")
ns_df <- cbind(rowvars=rownames(ns_df), data.frame(ns_df)) %>% 
  gather(colvars, n, -rowvars)

ps_df <- cortest_p
colnames(ps_df)<-str_remove(colnames(ps_df), "P.")
ps_df <- cbind(rowvars=rownames(ps_df), data.frame(ps_df)) %>% 
  gather(colvars, p.value, -rowvars)
plotdf <- left_join(cors_df, ns_df) 
plotdf <- left_join(plotdf, ps_df)

p1 <- ggplot(plotdf, aes(colvars, fct_rev(rowvars))) +
  geom_tile(colour="grey20", aes(fill=corr), 
            size=0.5) +
  #geom_point(aes(size=p.value, colour=cut(abs(corr), c(0, 0.01, 0.05, 1), include.lowest=TRUE)), pch=15) +
  geom_text(aes(label=sprintf("%1.2f", corr)), position=position_nudge(y=0.3), 
            size=3, colour="white") +
  geom_text(aes(label=sprintf("%1.0f", n)), position=position_nudge(y=0), 
            size=3, colour="white") +
  geom_text(aes(label=paste0("(",sprintf("%1.2f", p.value),")")), position=position_nudge(y=-0.3), 
            size=2.5, colour="white") +
  scale_fill_gradient2(low="yellow", mid="red", high="blue", midpoint=0.5, limits=c(0,1), name="Correlation") +
  scale_size_continuous(range=c(8,12)) +
  labs(x="",y="") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_fixed()
ggsave(p1, filename=paste0(wd,"/figs/temp_metrics_", temp.month, "_verbose_gg_corr_plot_shortscale.svg"))

p2 <- ggplot(plotdf, aes(colvars, fct_rev(rowvars))) +
  geom_tile(colour="grey20", aes(fill=corr), 
            size=0.5) +
  #geom_point(aes(size=p.value, colour=cut(abs(corr), c(0, 0.01, 0.05, 1), include.lowest=TRUE)), pch=15) +
  geom_text(aes(label=sprintf("%1.2f", corr)), position=position_nudge(y=0.2), 
            size=3, colour="white") +
  geom_text(aes(label=sprintf("%1.0f", n)), position=position_nudge(y=-0.2), 
            size=3, colour="white") +
  # geom_text(aes(label=paste0("(",sprintf("%1.2f", p.value),")")), position=position_nudge(y=-0.3), 
  #           size=2.5, colour="white") +
  scale_fill_gradient2(low="yellow", mid="red", high="blue", midpoint=0.5, limits=c(0,1), name="Correlation") +
  scale_size_continuous(range=c(8,12)) +
  labs(x="",y="") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_fixed()
ggsave(p2, filename=paste0(wd,"/figs/temp_metrics_", temp.month, "_verbose_gg_corr_plot_shortscale_nopval.svg"))

write_csv(plotdf, file=paste0(wd,"/output/correlations/temp_metrics_", temp.month, "_corr_test_all_together.csv"))

       

}

plotdf <- read_csv(file=paste0(wd,"/output/correlations/temp_metrics_", vecmonths[1], "_corr_test_all_together.csv"))


#read all those plotdfs back in and join together

output_df <- plotdf[0,]

for(g in 1:12){
  print(g)
  temp.month <- vecmonths[g]
  temp.dat <- read.csv(file=paste0(wd,"/output/correlations/temp_metrics_", temp.month, "_corr_test_all_together.csv"))
  temp.dat$month <- paste(temp.month)
  
  output_df <- rbind(output_df, temp.dat)
}

output_df$first_metric <-  substr(output_df$rowvars, start=1, stop=3)
output_df$second_metric <-  substr(output_df$colvars, start=1, stop=3)
output_df$first_depth <- str_sub(output_df$rowvars,-4,-1)
output_df$second_depth <- str_sub(output_df$colvars,-4,-1)
#output_df <- output_df %>% mutate(metric_match=case_when())

output_df$match_metric <- NA
output_df$match_metric <- output_df$first_metric==output_df$second_metric
output_df$match_depth <- output_df$first_depth==output_df$second_depth

output_df$matches <- NA
output_df$matches[which(output_df$match_metric==TRUE&output_df$match_depth==FALSE)] <- "Same metric, different depth"
output_df$matches[which(output_df$match_metric==TRUE&output_df$match_depth==TRUE)] <- "Same metric, same depth"
output_df$matches[which(output_df$match_metric==FALSE&output_df$match_depth==TRUE)] <- "Different metric, same depth"
output_df$matches[which(output_df$match_metric==FALSE&output_df$match_depth==FALSE)] <- "Different metric, different depth"

ggplot(output_df, aes(month, corr, col=matches)) + geom_boxplot() + #geom_point()
  theme_bw()

ggplot(output_df, aes(month, corr, col=matches))  + geom_point(aes(alpha=n), position = position_jitter(width = .2, seed = 0),
                                                               size = 3) + theme_bw()

output_df <- output_df %>%
mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", 
                                        "may", "jun", "jul", "aug",
                                        "sep", "oct", "nov", "dec")))
  
#most developped multipanel 
ggplot(output_df[which(output_df$matches!="Same metric, same depth"),], 
       aes(matches, corr, col=matches))+ 
  geom_point(aes(alpha=n), position = position_jitter(width = .4, seed = 0),
                     size = 2) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~month, nrow=2) + geom_hline(aes(yintercept=0))  

#in between
ggplot(output_df[which(output_df$matches!="Same metric, same depth"),], 
       aes(month, corr, col=matches, group=matches))+ 
  geom_point(aes(alpha=n), position = dodge,
             size = 2) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_hline(aes(yintercept=0))  + facet_grid(~month, scales="free", space="free_x") + 
  theme(#strip.background = element_blank(), #remove background for facet labels
    panel.border = element_rect(colour = "grey", fill = NA), #add black border
    panel.spacing = unit(0, "lines")) #remove space between facets

#single panel 
ggplot(output_df[which(output_df$matches!="Same metric, same depth"),], 
       aes(month, corr, col=matches, group=matches))+ 
  scale_color_brewer(palette = "Dark2")+
  geom_point(aes(alpha=n), position = position_dodge(width=1), 
             size = 2) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
   geom_hline(aes(yintercept=0))  + 
  geom_vline(xintercept = 1:12+0.5, col="dark grey") + ylab("Correlation") + 
  xlab("Month")

  
ggplot(output_df, aes(matches, corr, col=matches))+ geom_boxplot() +  geom_point(aes(alpha=n)) +
  theme_bw() + facet_wrap(~month) 

ggplot(output_df, aes(interaction(matches, month), corr, col=matches))+ geom_boxplot() +  geom_point(aes(alpha=n)) +
  theme_bw() 

#NEED TO DO
#order by month
#turn labels?


#plot sample years?=======

mnt_long <- mnt_norange %>% pivot_longer(-c(Year, Month, depth), names_to = "metric")
mnt_long <- na.omit(mnt_long)
ggplot(mnt_long, aes(Year, metric, group=as.factor(Month), col=as.factor(Month))) + 
  geom_point(position = dodge) +
  theme_bw()

mnt_long <- mnt_long %>%
  mutate(depth = factor(depth, levels = c("50m", "100m", "150m")))

ggplot(mnt_long, aes(as.factor(Month), Year, group=metric, fill=metric)) + 
  geom_point(position=position_dodge(width = 1), pch=21,) +
  theme_bw() + facet_wrap(~depth, ncol=1) + scale_fill_brewer(palette = "RdYlBu") +
  geom_vline(xintercept = 1:12+0.5, col="dark grey") + ylab("Year") + 
  xlab("Month")


#by year==============

#turn mnt_wide and repeat corr by year?

mnt_yr_wide <- mnt_wide %>%
  pivot_wider(names_from=c(Year, Month), values_from = c("hycom_50m", "hycom_100m" , "hycom_150m" , "cfsr_50m"  ,    
                                              "cfsr_100m", "gak_50m" , "gak_100m" , "gak_150m" , "AFSC_BTS_50m"  , "AFSC_BTS_100m", 
                                              "AFSC_BTS_150m", "IPHC_FISS_50m","IPHC_FISS_100m" ,"IPHC_FISS_150m"))
#not working


temp.cov <- mnt_wide[which(mnt_wide$Month==i),!names(mnt_wide) %in% 
                       c("Year", "Month")]
temp.cov <- temp.cov %>% remove_empty("cols") #need something additional here to remove really limited cols
temp.cov <- temp.cov[, sapply(temp.cov, function(col) length(unique(col))) > 3]
cov.cor <- cor(temp.cov, use='pairwise.complete.obs')







