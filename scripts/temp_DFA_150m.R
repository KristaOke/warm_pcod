#==================================================================================
#DFA on temperature metrics for 150m layer

#by Krista, Apr 2024
#==================================================================================
#Notes:
#==================================================================================

library(tidyverse)
library(MARSS)
library(corrplot)


#load data-------

wd <- getwd()
joined_150_dat <- read.csv("/Users/krista/Dropbox/Work folder/Pacific cod/Analysis/Temp4Krista/joined_150m_data_for_DFA.csv",
                          row.names = 1)
#joined_150_dat$type[which(joined_150_dat$type=="temp")] <- "CFSR_temp" #no cfsr
joined_150_dat <- joined_150_dat[!duplicated(joined_150_dat),]
wide_150_dat <- pivot_wider(joined_150_dat, names_from = type, values_from = temp)

scaled_dat <- wide_150_dat %>%     #UPDATE HERE when all indices are in hand IMPORTANT
  mutate(mean_jun_temp_hycom_150_scaled=scale(mean_jun_temp_hycom_150),
         #CFSR_temp_scaled=scale(CFSR_temp),
         AFSC_BTS_scaled=scale(AFSC_BTS),
         AFSC_LLS_scaled=scale(AFSC_LLS),
         IPHC_FISS_scaled=scale(IPHC_FISS),
         GAK1_mooring_140m_to_160m_scaled=scale(GAK1_mooring_140m_to_150m))

#plot=======

scaled_long <- scaled_dat[,c(1,7:11)] %>% pivot_longer(-year, names_to = "metric", values_to = "temp_value")

ggplot(scaled_long, aes(year, temp_value)) + geom_line() + facet_wrap(~metric, scale="free")

ggplot(scaled_long, aes(year, temp_value, col=metric)) + geom_line() + 
  geom_point() + theme_bw()

unscaled_long <- scaled_dat[,c(1:6)] %>% pivot_longer(-year, names_to = "metric", values_to = "temp_value")

ggplot(unscaled_long, aes(year, temp_value, col=metric)) + geom_line() + 
  geom_point() + theme_bw()

#corrs=======
temp.cov <- scaled_dat[,c(7:11)]
temp.cov <- na.omit(temp.cov) 

cov.cor <- cor(temp.cov)
corrplot(cov.cor,order='AOE',  type = 'lower', method = 'number')

#correlations are VERY HIGH!

#manupulate data for DFA========

scaled_dat1 <- scaled_dat[,c(1,7:11)] #only scaled cols, update when all cols are in hand
#dropping habitat suitablity

scaled_dat1 <- scaled_dat1[order(scaled_dat1$year),] #otherwise model runs out of order!


s.mat <- t(as.matrix(scaled_dat1))
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

saveRDS(model.data, file="data/model_data_DFA_150m.RDS")

model.data <- readRDS(file="data/model_data_DFA_150m.RDS")

#diagonal and unequal 3 best, unconstrained not converging

#rotate=======

# now fit best model

model.list.1 = list(A="zero", m=1, R="equalvarcov") # 
cntl.list1 = list(minit=200, maxit=100000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)
model.1 = MARSS(s.mat, model=model.list.1, z.score=TRUE, form="dfa", control=cntl.list1) #CONV ISSUES
autoplot(model.1)


# and rotate the loadings
Z.est = coef(model.1, type="matrix")$Z
H_inv = varimax(coef(model.1, type="matrix")$Z)$rotmat
#Z.rot = as.data.frame(Z.est %*% H_inv)
Z.rot <- Z.est

#proc_rot = solve(H_inv) %*% model.1$states #

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

yr_frst <- 1979

## get number of time series
N_ts <- dim(s.mat)[1]
## get length of time series
TT <- dim(s.mat)[2]

## get the estimated ZZ
Z_est <- coef(model.1, type = "matrix")$Z
## get the inverse of the rotation matrix
H_inv <- varimax(Z_est)$rotmat

## rotate factor loadings
#Z_rot = Z_est %*% H_inv
Z_rot = Z_est #%*% H_inv
## rotate processes
#proc_rot = solve(H_inv) %*% model.1$states
proc_rot =  model.1$states

mm <- 1 #1 process

rec_names <- rownames(s.mat)
ylbl <- rec_names
w_ts <- seq(dim(s.mat)[2])
layout(matrix(c(1, 2), mm, 2), widths = c(2, 1))
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
  axis(1, 1:46, yr_frst + 0:dim(s.mat)[2])
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


#plot obs v fitted ======


get_DFA_fits <- function(MLEobj, dd = NULL, alpha = 0.05) {
  ## empty list for results
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type = "matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat
  ## check for covars
  if (!is.null(dd)) {
    DD <- coef(MLEobj, type = "matrix")$D
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd
  } else {
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states
  }
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT
  VV <- NULL
  for (tt in 1:TT) {
    RZVZ <- coef(MLEobj, type = "matrix")$R - ZZ %*% VtT[, 
                                                         , tt] %*% t(ZZ)
    SS <- Ey$yxtT[, , tt] - Ey$ytT[, tt, drop = FALSE] %*% 
      t(MLEobj$states[, tt, drop = FALSE])
    VV <- cbind(VV, diag(RZVZ + SS %*% t(ZZ) + ZZ %*% t(SS)))
  }
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI
  fits$up <- qnorm(1 - alpha/2) * SE + fits$ex
  fits$lo <- qnorm(alpha/2) * SE + fits$ex
  return(fits)
}

#demean data
# y_bar <- apply(all.clim.dat, 1, mean, na.rm = TRUE)
# dat <- all.clim.dat - y_bar
# rownames(dat) <- rownames(all.clim.dat)

# dat <- scale(log.rec.mat)
# 
# head(log.rec.mat)
# 
# log.rec.mat.std <- log.rec.mat

# i <- 1
# for(i in 1:nrow(s.mat)) {
#   s.mat.std[i,] <- (s.mat[i,]-mean(s.mat[i,], na.rm=TRUE))/sd(s.mat[i,], na.rm=TRUE)  
# }
#Double checking
# apply(s.mat.std, 1, mean, na.rm=TRUE)
# apply(s.mat.std, 1, sd, na.rm=TRUE)
dat <- s.mat
#plot demeaned data

driv <- rownames(s.mat)
clr <- c("brown", "blue", "darkgreen", "darkred", "purple",
         "brown", "blue", "darkgreen", "darkred", "purple",
         "brown", "blue", "darkgreen", "darkred", "purple",
         "brown", "blue", "darkgreen", "darkred")
cnt <- 1
# par(mfrow = c(N_ts, 2), mar = c(1, 1,1.5,1), omi = c(0.1, 
#                                                      0.1, 0.1, 0.1))
par(mfrow = c(N_ts, 1), mar = c(1, 1,1.5,1), omi = c(0.1, 
                                                     0.1, 0.1, 0.1))
for (i in driv) {
  plot(dat[i, ], xlab = "", ylab = "", bty = "L", 
       xaxt = "n", pch = 16, col = clr[cnt], type = "b")
  axis(1,  (0:dim(s.mat)[2]) + 1, yr_frst + 0:dim(s.mat)[2])
  title(i)
  cnt <- cnt + 1
}


## get model fits & CI's
mod_fit <- get_DFA_fits(model.1)
## plot the fits
par(mfrow = c(5, 4), mar = c(1, 1, 1, 1), omi = c(0, 
                                                  0, 0, 0))

for (i in 1:N_ts) {
  up <- mod_fit$up[i, ]
  mn <- mod_fit$ex[i, ]
  lo <- mod_fit$lo[i, ]
  plot(w_ts, mn, xlab = "", xaxt = "n", type = "n", 
       cex.lab = 1.2, ylim = c(min(lo), max(up)))
  axis(1,  (0:dim(s.mat)[2]) + 1, yr_frst + 0:dim(s.mat)[2])
  points(w_ts, dat[i, ], pch = 16, col = clr[i])
  lines(w_ts, up, col = "darkgray")
  lines(w_ts, mn, col = "black", lwd = 2)
  lines(w_ts, lo, col = "darkgray")
}






#plot second best model=========


# now fit second best model

model.list.2 = list(A="zero", m=2, R="equalvarcov") # second best model
cntl.list2 = list(minit=200, maxit=200000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)
model.2 = MARSS(s.mat, model=model.list.2, z.score=TRUE, form="dfa", control=cntl.list2)
autoplot(model.2)



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

yr_frst <- 1993

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
  axis(1, 1:29, yr_frst + 0:dim(s.mat)[2])
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





#plot obs v fitted ======


get_DFA_fits <- function(MLEobj, dd = NULL, alpha = 0.05) {
  ## empty list for results
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type = "matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat
  ## check for covars
  if (!is.null(dd)) {
    DD <- coef(MLEobj, type = "matrix")$D
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd
  } else {
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states
  }
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT
  VV <- NULL
  for (tt in 1:TT) {
    RZVZ <- coef(MLEobj, type = "matrix")$R - ZZ %*% VtT[, 
                                                         , tt] %*% t(ZZ)
    SS <- Ey$yxtT[, , tt] - Ey$ytT[, tt, drop = FALSE] %*% 
      t(MLEobj$states[, tt, drop = FALSE])
    VV <- cbind(VV, diag(RZVZ + SS %*% t(ZZ) + ZZ %*% t(SS)))
  }
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI
  fits$up <- qnorm(1 - alpha/2) * SE + fits$ex
  fits$lo <- qnorm(alpha/2) * SE + fits$ex
  return(fits)
}

#demean data
# y_bar <- apply(all.clim.dat, 1, mean, na.rm = TRUE)
# dat <- all.clim.dat - y_bar
# rownames(dat) <- rownames(all.clim.dat)

# dat <- scale(log.rec.mat)
# 
# head(log.rec.mat)
# 
# log.rec.mat.std <- log.rec.mat

# i <- 1
# for(i in 1:nrow(s.mat)) {
#   s.mat.std[i,] <- (s.mat[i,]-mean(s.mat[i,], na.rm=TRUE))/sd(s.mat[i,], na.rm=TRUE)  
# }
#Double checking
# apply(s.mat.std, 1, mean, na.rm=TRUE)
# apply(s.mat.std, 1, sd, na.rm=TRUE)
dat <- s.mat
#plot demeaned data

driv <- rownames(s.mat)
clr <- c("brown", "blue", "darkgreen", "darkred", "purple",
         "brown", "blue", "darkgreen", "darkred", "purple",
         "brown", "blue", "darkgreen", "darkred", "purple",
         "brown", "blue", "darkgreen", "darkred")
cnt <- 1
# par(mfrow = c(N_ts, 2), mar = c(1, 1,1.5,1), omi = c(0.1, 
#                                                      0.1, 0.1, 0.1))
par(mfrow = c(N_ts, 1), mar = c(1, 1,1.5,1), omi = c(0.1, 
                                                     0.1, 0.1, 0.1))
for (i in driv) {
  plot(dat[i, ], xlab = "", ylab = "", bty = "L", 
       xaxt = "n", pch = 16, col = clr[cnt], type = "b")
  axis(1,  (0:dim(s.mat)[2]) + 1, yr_frst + 0:dim(s.mat)[2])
  title(i)
  cnt <- cnt + 1
}


## get model fits & CI's
mod_fit <- get_DFA_fits(model.2)
## plot the fits
par(mfrow = c(3, 2), mar = c(1, 1, 1, 1), omi = c(0, 
                                                  0, 0, 0))
for (i in 1:N_ts) {
  up <- mod_fit$up[i, ]
  mn <- mod_fit$ex[i, ]
  lo <- mod_fit$lo[i, ]
  plot(w_ts, mn, xlab = "", xaxt = "n", type = "n", 
       cex.lab = 1.2, ylim = c(min(lo), max(up)))
  axis(1,  (0:dim(s.mat)[2]) + 1, yr_frst + 0:dim(s.mat)[2])
  points(w_ts, dat[i, ], pch = 16, col = clr[i])
  lines(w_ts, up, col = "darkgray")
  lines(w_ts, mn, col = "black", lwd = 2)
  lines(w_ts, lo, col = "darkgray")
}










