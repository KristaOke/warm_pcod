#==================================================================================
#DFA on temperature metrics

#by Krista, Feb 2024
#==================================================================================
#Notes:
#==================================================================================

library(tidyverse)
library(MARSS)
library(corrplot)
library(Hmisc)


#load data-------

wd <- getwd()
joined_temp_dat <- read.csv(paste0(wd,"/data/scaled_temp_by_season.csv"), row.names = 1)

#this data is already scaled but needs to be converted to wide format

# scaled_dat <- joined_temp_dat %>%     #UPDATE HERE when all indices are in hand IMPORTANT
#   mutate(mean_SST_jun_aug_sep_scaled=scale(mean_SST_jun_aug_sep),
#          mean_SST_oct_nov_dec_scaled=scale(mean_SST_oct_nov_dec),
#          mean_SST_apr_may_jun_scaled=scale(mean_SST_apr_may_jun),
#          mean_SST_jan_feb_mar_scaled=scale(mean_SST_jan_feb_mar),
#          annual_MHWI_scaled=scale(annual_MHWI),
#          summer_MHWI_scaled=scale(summer_MHWI),
#          winter_MHWI_scaled=scale(winter_MHWI),
#          spawning_MHWI_scaled=scale(spawning_MHWI),
#          Spawning_Heatwave_GOA_Model_scaled=scale(Spawning_Heatwave_GOA_Model),
#          Winter_Spring_Pacific_Cod_Spawning_Habitat_Suitability_GAK1_Model_scaled=scale(Winter_Spring_Pacific_Cod_Spawning_Habitat_Suitability_GAK1_Model),
#          Summer_Temperature_Bottom_GOA_Model_scaled=scale(Summer_Temperature_Bottom_GOA_Model),
#          cfsr_jun_dev_0_20_scaled=scale(cfsr_jun_dev_0_20),
#          cfsr_jun_dev_20_40_scaled=scale(cfsr_jun_dev_20_40),
#          cfsr_jun_dev_40_60_scaled=scale(cfsr_jun_dev_40_60),
#          cfsr_jun_dev_60_80_scaled=scale(cfsr_jun_dev_60_80),
#          cfsr_jun_dev_80_scaled=scale(cfsr_jun_dev_80),
#          Summer_Temperature_Bottom_GOA_Survey_scaled=scale(Summer_Temperature_Bottom_GOA_Survey),
#          Summer_Temperature_250m_GOA_Survey_scaled=scale(Summer_Temperature_250m_GOA_Survey))

scaled_wide <- joined_temp_dat[which(joined_temp_dat$depth!="40-60m"&
                                       joined_temp_dat$depth!="90-100m"),] %>% pivot_wider(names_from = c(season, depth), 
                                               values_from = c(seasonal_cfsr_mean, seasonal_hycom_mean,
                                                               seasonal_gak_mean, seasonal_BTS_mean,
                                                               seasonal_LLS_mean, seasonal_IPHC_mean))

#remove the columns for winter-fall surveys that are all NAs
unique(is.na(scaled_wide$seasonal_BTS_mean_winter_100m))
unique(is.na(scaled_wide$seasonal_BTS_mean_fall_100m))
unique(is.na(scaled_wide$seasonal_BTS_mean_spring_100m))
unique(is.na(scaled_wide$seasonal_LLS_mean_spring_100m))

scaled_wide <- scaled_wide[,-c(10:13, 38, 39, 44:46, 49:51, 56:58, 61:63, 68:70, 73)]

#plot=======

scaled_long <- scaled_wide %>% pivot_longer(-season_year, names_to = "metric", values_to = "temp_value")

ggplot(scaled_long, aes(season_year, temp_value)) + geom_point() + geom_line() + facet_wrap(~metric, scale="free")

scaled_season_plot <- scaled_long
scaled_season_plot$season <- NA

scaled_season_plot$season[which(scaled_season_plot$metric %in% "winter")] <- "winter"

ggplot(scaled_long[], aes(season_year, temp_value)) + geom_point() + geom_line() 


ggplot(scaled_long, aes(year, temp_value, col=metric)) + geom_line()

ggplot(scaled_long[which(scaled_long$metric=="cfsr_jun_dev_0_20_scaled"|
                           #scaled_long$metric=="cfsr_jun_dev_20_40_scaled"|
                           scaled_long$metric=="cfsr_jun_dev_40_60_scaled"|
                           #scaled_long$metric=="cfsr_jun_dev_60_80_scaled"|
                           #scaled_long$metric=="cfsr_jun_dev_80_scaled"|
                          # scaled_long$metric=="Summer_Temperature_Bottom_GOA_Model_scaled"|
                           scaled_long$metric=="Summer_Temperature_Bottom_GOA_Survey_scaled"|
                           scaled_long$metric=="Summer_Temperature_250m_GOA_Survey_scaled"),], aes(year, temp_value, col=metric)) + geom_line() + geom_point()

ggplot(unscaled_long, aes(year, temp_value, col=metric)) + geom_line()

#corrs=======
#temp.cov <- data.frame(t(scaled_dat[,c(18:26, 28:33)]))
temp.cov <- scaled_wide[,-1]
#temp.cov <- na.omit(temp.cov) #only 5 rows b/c so many NAs

#fix up names so we can see them in the cor plot
#painful but here we go
temp.cov <- temp.cov %>%
  rename(cfsr_fall_50 = seasonal_cfsr_mean_fall_50m,
         cfsr_fall_100 = seasonal_cfsr_mean_fall_100m,
         cfsr_spr_50 = seasonal_cfsr_mean_spring_50m,  
 cfsr_spr_100 = seasonal_cfsr_mean_spring_100m,
 cfsr_smr_50 = seasonal_cfsr_mean_summer_50m,
 cfsr_smr_100 = seasonal_cfsr_mean_summer_100m, 
cfsr_win_50 = seasonal_cfsr_mean_winter_50m,
cfsr_win_100 = seasonal_cfsr_mean_winter_100m,
hycom_fall_50 = seasonal_hycom_mean_fall_50m,   
hycom_fall_100 = seasonal_hycom_mean_fall_100m,
hycom_spr_50 = seasonal_hycom_mean_spring_50m,
hycom_spr_100 = seasonal_hycom_mean_spring_100m,
hycom_smr_50 = seasonal_hycom_mean_summer_50m,
hycom_smr_100 = seasonal_hycom_mean_summer_100m,
hycom_win_50 = seasonal_hycom_mean_winter_50m, 
hycom_win_100 = seasonal_hycom_mean_winter_100m,
hycom_fall_150 = seasonal_hycom_mean_fall_150m,
hycom_spr_150 = seasonal_hycom_mean_spring_150m,
hycom_smr_150 = seasonal_hycom_mean_summer_150m,
hycom_win_150 = seasonal_hycom_mean_winter_150m,
gak_fall_50 = seasonal_gak_mean_fall_50m,     
gak_fall_100 = seasonal_gak_mean_fall_100m,
gak_spr_50 = seasonal_gak_mean_spring_50m,
gak_spr_100 = seasonal_gak_mean_spring_100m,  
gak_smr_50 = seasonal_gak_mean_summer_50m,
gak_smr_100 = seasonal_gak_mean_summer_100m,
gak_win_50 = seasonal_gak_mean_winter_50m,   
gak_win_100 = seasonal_gak_mean_winter_100m,
gak_fall_150 = seasonal_gak_mean_fall_150m,
gak_spr_150 = seasonal_gak_mean_spring_150m,  
gak_smr_150 = seasonal_gak_mean_summer_150m,
gak_win_150 = seasonal_gak_mean_winter_150m,
BTS_spr_50 = seasonal_BTS_mean_spring_50m,   
BTS_spr_100 = seasonal_BTS_mean_spring_100m,
BTS_smr_50 = seasonal_BTS_mean_summer_50m,
BTS_smr_100 = seasonal_BTS_mean_summer_100m,  
BTS_spr_150 = seasonal_BTS_mean_spring_150m,
BTS_smr_150 = seasonal_BTS_mean_summer_150m,
LLS_spr_50 = seasonal_LLS_mean_spring_50m,   
LLS_spr_100 = seasonal_LLS_mean_spring_100m,
LLS_smr_50 = seasonal_LLS_mean_summer_50m,
LLS_smr_100 = seasonal_LLS_mean_summer_100m,  
LLS_spr_150 = seasonal_LLS_mean_spring_150m,
LLS_smr_150 = seasonal_LLS_mean_summer_150m,
IPHC_spr_50 = seasonal_IPHC_mean_spring_50m,  
IPHC_spr_100 = seasonal_IPHC_mean_spring_100m,
IPHC_smr_50 = seasonal_IPHC_mean_summer_50m,
IPHC_smr_100 = seasonal_IPHC_mean_summer_100m, 
IPHC_spr_150 = seasonal_IPHC_mean_spring_150m,
IPHC_smr_150 = seasonal_IPHC_mean_summer_150m )

cov.cor <- cor(temp.cov, use='pairwise.complete.obs')
corrplot(cov.cor,order='AOE',  type = 'lower', method = 'number', number.cex=0.8, tl.cex=0.5)

cortest <- rcorr(as.matrix(temp.cov))
cortest_r <- as.data.frame(cortest[1]) #correlations
cortest_n <- as.data.frame(cortest[2]) #sample sizes
cortest_p <- as.data.frame(cortest[3]) #p-values
write_csv(cortest_p, file=paste0(wd,"/output/temp_metrics_corr_test_pvals.csv"))
write_csv(cortest_n, file=paste0(wd,"/output/temp_metrics_corr_test_samplesizes.csv"))
write_csv(cortest_r, file=paste0(wd,"/output/temp_metrics_corr_test_corrs.csv"))

corrplot(cortest$r, p.mat=cortest$p, method="number")

corrplot(cortest$r, order = 'AOE', method="number", type = 'upper', tl.pos = 'd')
#corrplot(cortest$r, p.mat=cortest$p, insig="p-value",  type = 'lower', method = 'ellipse')
corrplot(cortest$r, p.mat=cortest$p, insig="p-value", add = TRUE, type = 'lower', method = 'number') #not showing p val yet

#now cor by season====

summer_temps <- select(temp.cov,contains("smr"))
spring_temps <- select(temp.cov,contains("spr"))
winter_temps <- select(temp.cov,contains("win"))
fall_temps <- select(temp.cov,contains("fall"))

corsmr <- rcorr(as.matrix(summer_temps))
corrplot(corsmr$r, order = 'AOE', method="number", type = 'upper', tl.pos = 'd')
mean(corsmr$r)

corspr <- rcorr(as.matrix(spring_temps))
corrplot(corspr$r, order = 'AOE', method="number", type = 'upper', tl.pos = 'd')
mean(corspr$r)

corwin <- rcorr(as.matrix(winter_temps))
corrplot(corwin$r, order = 'AOE', method="number", type = 'upper', tl.pos = 'd')
mean(corwin$r)

corfall <- rcorr(as.matrix(fall_temps))
corrplot(corfall$r, order = 'AOE', method="number", type = 'upper', tl.pos = 'd')
mean(corfall$r)

#now cor by depth====

temps50 <- select(temp.cov,contains("_50"))
temps100 <- select(temp.cov,contains("100"))
temps150 <- select(temp.cov,contains("150"))


cor50 <- rcorr(as.matrix(temps50))
corrplot(cor50$r, order = 'AOE', method="number", type = 'upper', tl.pos = 'd')
mean(cor50$r)

cor100 <- rcorr(as.matrix(temps100))
corrplot(cor100$r, order = 'AOE', method="number", type = 'upper', tl.pos = 'd')
mean(cor100$r)

cor150 <- rcorr(as.matrix(temps150))
corrplot(cor150$r, order = 'AOE', method="number", type = 'upper', tl.pos = 'd')
mean(cor150$r)



#manupulate data for DFA========


scaled_wide <- scaled_wide[order(scaled_wide$season_year),] #otherwise model runs out of order!

s.mat <- t(as.matrix(scaled_wide[which(scaled_wide$season_year>1992),]))
colnames(s.mat) <- s.mat[1,]
s.mat <- s.mat[-1,]




#run DFA====

#copying from some previous code

# now fit DFA models with 1-3 trends and different error structures and compare

# changing convergence criterion to ensure convergence
cntl.list = list(minit=200, maxit=50000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)

# set up forms of R matrices
levels.R = c("diagonal and equal",
             "diagonal and unequal",
             "equalvarcov",
             "unconstrained")
model.data = data.frame()

# fit models & store results
for(R in levels.R) {
  for(m in 1:3) {  # allowing up to 3 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(s.mat, model=dfa.model, control=cntl.list,
                 form="dfa", z.score=TRUE)
    model.data = rbind(model.data,
                       data.frame(R=R,
                                  m=m,
                                  logLik=kemz$logLik,
                                  K=kemz$num.params,
                                  AICc=kemz$AICc,
                                  stringsAsFactors=FALSE))
    assign(paste("kemz", m, R, sep="."), kemz)
  } # end m loop
} # end R loop

# calculate delta-AICc scores, sort in descending order, and compare
model.data$dAICc <- model.data$AICc-min(model.data$AICc)
model.data <- model.data %>%
  arrange(dAICc)
model.data

saveRDS(model.data, file="data/DFA_post93_2024_10_14.RDS")

#diagonal and unequal 3 best, unconstrained not converging

#rotate=======

# now fit best model

model.list.1 = list(A="zero", m=3, R="diagonal and unequal") # 
cntl.list1 = list(minit=200, maxit=100000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)
model.1 = MARSS(s.mat, model=model.list.1, z.score=TRUE, form="dfa", control=cntl.list1) #CONV ISSUES

saveRDS(model.1, file="data/DFA_model1_2024_10_15.RDS")


# and rotate the loadings
Z.est = coef(model.1, type="matrix")$Z
H_inv = varimax(coef(model.1, type="matrix")$Z)$rotmat
Z.rot = as.data.frame(Z.est %*% H_inv)

proc_rot = solve(H_inv) %*% model.1$states #

# reverse trend 2 to plot
Z.rot[,2] <- -Z.rot[,2]

Z.rot$names <- rownames(s.mat)
Z.rot <- arrange(Z.rot, V1)
Z.rot <- gather(Z.rot[,c(1,2)])
Z.rot$names <- rownames(s.mat)
#Z.rot$plot.names <- reorder(Z.rot$names, 1:14)



#plot=========
# get CI and plot loadings...
modCI <- MARSSparamCIs(model.1)

plot.CI <- data.frame(mean=modCI$par$Z, upCI=modCI$par.upCI$Z,
                      lowCI=modCI$par.lowCI$Z)

plot.CI <- arrange(plot.CI, mean)
#plot.CI$names.order <- reorder(plot.CI$names, plot.CI$mean)
dodge <- position_dodge(width=0.9)

rec.plot <- ggplot(Z.rot, aes(names, value, fill=key)) + geom_bar(stat="identity", position="dodge") #+
# theme_bw() + ylab("Loading") + xlab("") + 
# scale_fill_manual(values=c("Trend 1" = cb[2], "Trend 2" = cb[3])) +
# theme(legend.position = c(0.8,0.2), legend.title=element_blank()) + geom_hline(yintercept = 0) +
# theme(axis.text.x  = element_text(angle=45, hjust=1, size=12)) + ylim(-0.6, 0.8)

#based on nwfsc-timeseries.github.io

yr_frst <- 1993

## get number of time series
N_ts <- dim(s.mat)[1]
## get length of time series
TT <- dim(s.mat)[2]

## get the estimated ZZ
Z_est <- coef(model.1, type = "matrix")$Z
## get the inverse of the rotation matrix
H_inv <- varimax(Z_est)$rotmat

## rotate factor loadings
Z_rot = Z_est %*% H_inv
## rotate processes
proc_rot = solve(H_inv) %*% model.1$states

mm <- 3 #2 processes

rec_names <- rownames(s.mat)
ylbl <- rec_names
w_ts <- seq(dim(s.mat)[2])
layout(matrix(c(1, 2, 3,4, 5,6), mm, 2), widths = c(2, 1))
## par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
# jpeg("figs/ugly_DFA_trends_loadings.jpg")
par(mfcol=c(mm,2), mar = c(1.3,1.3,1.3,1.3), omi = c(0.1, 0.1, 0.1, 0.1))
## plot the processes
i<-1
for (i in 1:mm) {
  ylm <- c(-1, 1) * max(abs(proc_rot[i, ]))
  ## set up plot area
  plot(w_ts, proc_rot[i, ], type = "n", bty = "L", #ylim = ylm, 
       xlab = "", ylab = "", xaxt = "n")
  ## draw zero-line
  abline(h = 0, col = "gray")
  ## plot trend line
  lines(w_ts, proc_rot[i, ], lwd = 2)
  lines(w_ts, proc_rot[i, ], lwd = 2)
  ## add panel labels
  mtext(paste("State", i), side = 3, line = 0.5)
  #axis(1, 12 * (0:dim(all.clim.dat)[2]) + 1, yr_frst + 0:dim(all.clim.dat)[2])
  axis(1, 1:33, yr_frst + 0:dim(s.mat)[2])
}
## plot the loadings
clr <- c("brown", 
         "blue", 
         "darkgreen", 
         "darkred", 
         "purple", 
         "darkorange")
minZ <- 0
ylm <- c(-1, 1) * max(abs(Z_rot))
for (i in 1:mm) {
  plot(c(1:N_ts)[abs(Z_rot[, i]) > minZ], as.vector(Z_rot[abs(Z_rot[, i]) > minZ, i]), 
       type = "h", lwd = 2, xlab = "", ylab = "", 
       xaxt = "n", ylim = ylm, xlim = c(0.5, N_ts + 0.5), col=clr)
  for (j in 1:N_ts) {
    if (Z_rot[j, i] > minZ) {
      text(j, -0.03, ylbl[j], srt = 90, adj = 1, cex = 1.2, col=clr[j])
    }
    if (Z_rot[j, i] < -minZ) {
      text(j, 0.03, ylbl[j], srt = 90, adj = 0, cex = 1.2, col=clr[j])
    }
    abline(h = 0, lwd = 1.5, col = "gray")
  }
  mtext(paste("Factor loadings on state", i), side = 3, line = 0.5)
}
#dev.off()

par(mai = c(0.9, 0.9, 0.1, 0.1))
ccf(proc_rot[1, ], proc_rot[2, ], lag.max = 12, main = "")







#plot second best model=========


# now fit second best model

model.list.2 = list(A="zero", m=2, R="diagonal and unequal") # second best model
cntl.list2 = list(minit=200, maxit=10000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)
model.2 = MARSS(s.mat, model=model.list.2, z.score=TRUE, form="dfa", control=cntl.list2)




# and rotate the loadings
Z.est = coef(model.2, type="matrix")$Z
H_inv = varimax(coef(model.2, type="matrix")$Z)$rotmat
Z.rot = as.data.frame(Z.est %*% H_inv)

proc_rot = solve(H_inv) %*% model.2$states #doesn't work

# reverse trend 2 to plot
Z.rot[,2] <- -Z.rot[,2]

Z.rot$names <- rownames(s.mat)
Z.rot <- arrange(Z.rot, V1)
Z.rot <- gather(Z.rot[,c(1,2)])
Z.rot$names <- rownames(s.mat)
#Z.rot$plot.names <- reorder(Z.rot$names, 1:14)



# get CI and plot loadings...
modCI <- MARSSparamCIs(model.2)

plot.CI <- data.frame(mean=modCI$par$Z, upCI=modCI$par.upCI$Z,
                      lowCI=modCI$par.lowCI$Z)

plot.CI <- arrange(plot.CI, mean)
#plot.CI$names.order <- reorder(plot.CI$names, plot.CI$mean)
dodge <- position_dodge(width=0.9)

rec.plot <- ggplot(Z.rot, aes(names, value, fill=key)) + geom_bar(stat="identity", position="dodge") #+
# theme_bw() + ylab("Loading") + xlab("") + 
# scale_fill_manual(values=c("Trend 1" = cb[2], "Trend 2" = cb[3])) +
# theme(legend.position = c(0.8,0.2), legend.title=element_blank()) + geom_hline(yintercept = 0) +
# theme(axis.text.x  = element_text(angle=45, hjust=1, size=12)) + ylim(-0.6, 0.8)

#based on nwfsc-timeseries.github.io

yr_frst <- 1981

## get number of time series
N_ts <- dim(s.mat)[1]
## get length of time series
TT <- dim(s.mat)[2]

## get the estimated ZZ
Z_est <- coef(model.2, type = "matrix")$Z
## get the inverse of the rotation matrix
H_inv <- varimax(Z_est)$rotmat

## rotate factor loadings
Z_rot = Z_est %*% H_inv
## rotate processes
proc_rot = solve(H_inv) %*% model.2$states

mm <- 2 #processes

rec_names <- rownames(s.mat)
ylbl <- rec_names
w_ts <- seq(dim(s.mat)[2])
layout(matrix(c(1, 2, 3, 4), mm, 2), widths = c(2, 1))
## par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
# jpeg("figs/ugly_DFA_trends_loadings.jpg")
par(mfcol=c(mm,2), mar = c(1,1,1,1), omi = c(0, 0, 0, 0))
## plot the processes
for (i in 1:mm) {
  ylm <- c(-1, 1) * max(abs(proc_rot[i, ]))
  ## set up plot area
  plot(w_ts, proc_rot[i, ], type = "n", bty = "L", #ylim = ylm, 
       xlab = "", ylab = "", xaxt = "n")
  ## draw zero-line
  abline(h = 0, col = "gray")
  ## plot trend line
  lines(w_ts, proc_rot[i, ], lwd = 2)
  lines(w_ts, proc_rot[i, ], lwd = 2)
  ## add panel labels
  mtext(paste("State", i), side = 3, line = 0.5)
  #axis(1, 12 * (0:dim(all.clim.dat)[2]) + 1, yr_frst + 0:dim(all.clim.dat)[2])
  axis(1, 1:44, yr_frst + 0:dim(s.mat)[2])
}
## plot the loadings
clr <- c("brown", 
         "blue", 
         "darkgreen", 
         "darkred", 
         "purple", 
         "darkorange")
minZ <- 0
ylm <- c(-1, 1) * max(abs(Z_rot))
for (i in 1:mm) {
  plot(c(1:N_ts)[abs(Z_rot[, i]) > minZ], as.vector(Z_rot[abs(Z_rot[, i]) > minZ, i]), 
       type = "h", lwd = 2, xlab = "", ylab = "", 
       xaxt = "n", ylim = ylm, xlim = c(0.5, N_ts + 0.5), col=clr)
  for (j in 1:N_ts) {
    if (Z_rot[j, i] > minZ) {
      text(j, -0.03, ylbl[j], srt = 90, adj = 1, cex = 1.2, col=clr[j])
    }
    if (Z_rot[j, i] < -minZ) {
      text(j, 0.03, ylbl[j], srt = 90, adj = 0, cex = 1.2, col=clr[j])
    }
    abline(h = 0, lwd = 1.5, col = "gray")
  }
  mtext(paste("Factor loadings on state", i), side = 3, line = 0.5)
}
#dev.off()

par(mai = c(0.9, 0.9, 0.1, 0.1))
ccf(proc_rot[1, ], proc_rot[2, ], lag.max = 12, main = "")














