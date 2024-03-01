#==================================================================================
#DFA on temperature metrics

#by Krista, Feb 2024
#==================================================================================
#Notes:
#==================================================================================

library(tidyverse)
library(MARSS)
library(corrplot)


#load data-------

wd <- getwd()
joined_temp_dat <- read.csv(paste0(wd,"/data/temp_metrics_dataset.csv"))

scaled_dat <- joined_temp_dat %>%
  mutate(mean_SST_jun_aug_sep_scaled=scale(mean_SST_jun_aug_sep),
         mean_SST_oct_nov_dec_scaled=scale(mean_SST_oct_nov_dec),
         mean_SST_apr_may_jun_scaled=scale(mean_SST_apr_may_jun),
         mean_SST_jan_feb_mar_scaled=scale(mean_SST_jan_feb_mar),
         annual_MHWI_scaled=scale(annual_MHWI),
         summer_MHWI_scaled=scale(summer_MHWI),
         winter_MHWI_scaled=scale(winter_MHWI),
         spawning_MHWI_scaled=scale(spawning_MHWI),
         Spawning_Heatwave_GOA_Model_scaled=scale(Spawning_Heatwave_GOA_Model),
         Winter_Spring_Pacific_Cod_Spawning_Habitat_Suitability_GAK1_Model_scaled=scale(Winter_Spring_Pacific_Cod_Spawning_Habitat_Suitability_GAK1_Model),
         Summer_Temperature_Bottom_GOA_Model_scaled=scale(Summer_Temperature_Bottom_GOA_Model),
         cfsr_jun_dev_0_20_scaled=scale(cfsr_jun_dev_0_20),
         cfsr_jun_dev_20_40_scaled=scale(cfsr_jun_dev_20_40),
         cfsr_jun_dev_40_60_scaled=scale(cfsr_jun_dev_40_60),
         cfsr_jun_dev_60_80_scaled=scale(cfsr_jun_dev_60_80),
         cfsr_jun_dev_80_scaled=scale(cfsr_jun_dev_80))

#plot=======

scaled_long <- scaled_dat[,c(1,18:33)] %>% pivot_longer(-year, names_to = "metric", values_to = "temp_value")

ggplot(scaled_long, aes(year, temp_value)) + geom_line() + facet_wrap(~metric, scale="free")

ggplot(scaled_long, aes(year, temp_value, col=metric)) + geom_line()

#corrs=======
#temp.cov <- data.frame(t(scaled_dat[,c(18:26, 28:33)]))
temp.cov <- scaled_dat[,c(18:26, 28:33)]
temp.cov <- na.omit(temp.cov) 

cov.cor <- cor(temp.cov)
corrplot(cov.cor,order='AOE',  type = 'lower', method = 'number')

#correlations are VERY HIGH!

#manupulate data for DFA========

scaled_dat1 <- scaled_dat[,c(1,18:33)] #only scaled cols

scaled_dat1 <- scaled_dat1[order(scaled_dat1$year),] #otherwise model runs out of order!


s.mat <- t(as.matrix(scaled_dat1))
colnames(s.mat) <- s.mat[1,]
s.mat <- s.mat[-1,]




#run DFA====

#copying from some previous code

# now fit DFA models with 1-3 trends and different error structures and compare

# changing convergence criterion to ensure convergence
cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)

# set up forms of R matrices
levels.R = c("diagonal and equal",
             "diagonal and unequal",
             "equalvarcov",
             "unconstrained")
model.data = data.frame()

# fit models & store results
for(R in levels.R) {
  for(m in 1:4) {  # allowing up to 3 trends
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

#diagonal and unequal 2 and 3 VERY close as best

#rotate=======

# now fit best model

model.list.1 = list(A="zero", m=2, R="diagonal and unequal") # best model by a little
model.1 = MARSS(s.mat, model=model.list.1, z.score=TRUE, form="dfa", control=cntl.list1)
cntl.list1 = list(minit=200, maxit=10000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)


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

yr_frst <- 1977

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

mm <- 2 #4 processes

rec_names <- rownames(s.mat)
ylbl <- rec_names
w_ts <- seq(dim(s.mat)[2])
layout(matrix(c(1, 2, 3, 4), mm, 2), widths = c(2, 1))
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
  axis(1, 1:47, yr_frst + 0:dim(s.mat)[2])
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

model.list.2 = list(A="zero", m=3, R="diagonal and unequal") # second best model
model.2 = MARSS(s.mat, model=model.list.2, z.score=TRUE, form="dfa", control=cntl.list2)
#DOES NOT CONVERGE bump up to 40K iter
cntl.list2 = list(minit=200, maxit=10000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)


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

yr_frst <- 1977

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

mm <- 3 #processes

rec_names <- rownames(s.mat)
ylbl <- rec_names
w_ts <- seq(dim(s.mat)[2])
layout(matrix(c(1, 2, 3, 4, 5, 6), mm, 2), widths = c(2, 1))
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
  axis(1, 1:47, yr_frst + 0:dim(s.mat)[2])
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














