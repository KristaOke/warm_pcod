# script to run temperature links for goa pcod ----
#by Krista, Feb 2025, borrowing heavily from Pete's script
#compare temperatures from different time periods

#BEFORE RUNNING
#CHECK YOUR r4ss VERSION
#this script written with r4ss 1.50.0 for SS3 release 3.30.23
#using r4ss 1.49 or older will result in errors in 'read and evaluate results' section

# load necessary packages ----
## cran packages ----
pkg_cran <- c("data.table",
              "tidyverse",
              "vroom",
              "here",
              "tictoc")

# if not installed, then install
if(length(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE)])
}

# load packages
lapply(pkg_cran, library, character.only = TRUE)

## github packages ----
pkg_git <- c("r4ss")

# if not installed, then install
if(!isTRUE("r4ss" %in% rownames(installed.packages()))) {
  devtools::install_github("r4ss/r4ss", force = TRUE)
}

# load packages
lapply(pkg_git, library, character.only = TRUE)

## load functions ----
source_files <- list.files(here::here("R"), pattern = "*.R$")
purrr::map(here::here("R", source_files), source)


# code example to run a model ----
# this example is to run the base 24 assessment model upon cloning the repository

# first, downlad the ss3 exe file with r4ss function to the model folder
r4ss::get_ss3_exe(dir = here::here('base_mdl'))

# second, get your operating system's exe name to be able to run model
#exename <- ss3_exename(dir = here::here('base_mdl'))
exename <- "ss3"

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'base_mdl',
              exename)



# CFSR link with longline survey catchability ----
#LOOPING THROUGH MONTHS AND DEPTHS=====

depths <- c("0_20", "20_40", "40_60", "60_80", "80plus")

i<-1
j<-1
for(i in 1:6){
  for(j in 1:length(depths)){

    temp_depth <- depths[j]

temp_folder <- paste0("CFSR_month_", i, "-depth_", temp_depth)

# first, start new model folder within 'rsch' folder
start_ss_fldr(from = here::here('base_mdl'), 
              to = here::here('rsch', 'CFSR_months_depths', temp_folder))

# next, read in data file to be able to change env link data
# define datafile name
datafilename <- list.files(here::here('rsch', 'CFSR_months_depths', temp_folder), pattern = "GOAPcod")
# get datafile input
datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'CFSR_months_depths', temp_folder, datafilename))

# and then, change the env data

# get cfsr data
cfsr <- vroom::vroom(here::here('data', 'raw_cfsr.csv'))

# compute env index
cfsr %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(month == i) %>% 
  tidytable::select(year, temp = temp_depth) %>% 
  tidytable::mutate(avg = as.numeric(cfsr %>% 
                                       dplyr::rename_all(tolower) %>% 
                                       tidytable::filter(month == i,
                                                         year >= 1982,
                                                         year <= 2012) %>% 
                                       tidytable::select(year, temp = temp_depth) %>% 
                                       tidytable::summarise(avg_temp = mean(temp)))) %>% 
  tidytable::mutate(value = round(temp - avg, digits = 5)) %>% 
  tidytable::select(year, value) -> env_indx

# format for ss3 data file
ss3indx <- ss3_envlnk(env_indx,
                      var = 1)

# replace the env data in the ss3 dat file with this new data
datafile$envdat <- ss3indx

# and, write datafile
r4ss::SS_writedat_3.30(datafile,
                       here::here(here::here('rsch', 'CFSR_months_depths', temp_folder), datafilename), 
                       overwrite = TRUE)

# almost to end, get your operating system's exe name to be able to run model
#exename <- ss3_exename(dir = here::here('rsch', 'llq_cfsr'))

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = paste0('rsch/','CFSR_months_depths/', temp_folder),
              exename)


  }
}






#read and evaluate results----


#LOOP to extract likelihoods from all those models==============================

cfsr_models_lik_df <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("month", "depth", "total_likelihood"))))

folder_list <- list.files(path = (here::here('rsch', 'CFSR_months_depths')),full.names=FALSE,recursive=FALSE)


g<-1
for(g in 1:length(folder_list)){

  temp_folder <- folder_list[g]
  parts <- str_split(temp_folder, "_")[[1]]
  temp_depth <- paste0(parts[4], "-", parts[5])
  parts2 <- str_split(parts[3], "-")[[1]]
  temp_month <- parts2[1]

# get model output for base cfsr llq model and alt model
output_temp <- r4ss::SSgetoutput(dirvec = c(here::here('rsch','llq'),
                                        here::here('rsch', 'CFSR_months_depths', temp_folder)))

# run function to summarize output into a list, a big list...
summ_out_temp <- r4ss::SSsummarize(output_temp)

cfsr_models_lik_df[g,1] <- temp_month
  cfsr_models_lik_df[g,2] <- temp_depth
cfsr_models_lik_df[g,3] <- summ_out_temp$likelihoods[1,2]

}

ggplot(cfsr_models_lik_df, aes(month, total_likelihood, col=depth)) + geom_point()






# HYCOM link with longline survey catchability ----


#load data-------

wd <- getwd()
mnt_temp_dat <- read.csv(paste0(wd,"/data/demeaned_temp_by_month.csv"), row.names = 1) #updated June 2025 to demeaned data w pre-2012 mean

#this data is already scaled but needs to be converted to wide format

mnt_temp_dat <- mnt_temp_dat %>% rename(hycom='mean_monthly',
                                        gak='gak_best_temp',
                                        cfsr='cfsr_temp')

mnt_norange <- mnt_temp_dat[which(mnt_temp_dat$depth!="40-60m"&
                                    mnt_temp_dat$depth!="90-100m"),]
mnt_norange <- mnt_norange[,!names(mnt_norange) %in% 
                             c("AFSC_LLS")]
mnt_norange <- mnt_norange[which(duplicated(mnt_norange)!=TRUE),]

#mnt_norange <- mnt_norange[which(mnt_norange$Year!=1995 & mnt_norange$Year!=2003 & mnt_norange$Year!=2023 & mnt_norange$Year!=2024),] #REMOVE when hycom issue is resolved

#TODAY Jun 18 try to solve issue with broken means from hycom

mnt_wide <- mnt_norange %>% pivot_wider(names_from = c(depth), 
                                        values_from = c(hycom, cfsr,
                                                        gak,
                                                        AFSC_BTS, #AFSC_LLS,
                                                        IPHC_FISS))
unique(duplicated(mnt_wide)) #if false that's good

hycom_mnt <- mnt_wide[,1:5]
write_csv(hycom_mnt, file="data/hycom_month_data_summary.csv")
hycom_mnt <- read.csv(paste0(wd,"/data/hycom_month_data_summary.csv"))

#have data, now loop

#LOOPING THROUGH MONTHS AND DEPTHS=====

depths <- c("hycom_50m", "hycom_100m", "hycom_150m")

i<-1
j<-1
for(i in 1:6){
  for(j in 1:length(depths)){
    
    temp_depth <- depths[j]
    
    temp_folder <- paste0("HYCOM_month_", i, "-depth_", temp_depth)
    
    # first, start new model folder within 'rsch' folder
    start_ss_fldr(from = here::here('base_mdl'), 
                  to = here::here('rsch', 'HYCOM_months_depths', temp_folder))
    
    # next, read in data file to be able to change env link data
    # define datafile name
    datafilename <- list.files(here::here('rsch', 'HYCOM_months_depths', temp_folder), pattern = "GOAPcod")
    # get datafile input
    datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'HYCOM_months_depths', temp_folder, datafilename))
    
    # and then, change the env data

     
    # compute env index 
    #HYCOM is already z-scored
    # hycom_mnt %>% 
    #   dplyr::rename_all(tolower) %>% 
    #   tidytable::filter(month == i) %>% 
    #   tidytable::select(year, temp = temp_depth) %>% 
    #   tidytable::mutate(avg = as.numeric(hycom_mnt %>% 
    #                                        dplyr::rename_all(tolower) %>% 
    #                                        tidytable::filter(month == i,
    #                                                          year >= 1982,
    #                                                          year <= 2012) %>% 
    #                                        tidytable::select(year, temp = temp_depth) %>% 
    #                                        tidytable::summarise(avg_temp = mean(temp, na.rm=TRUE)))) %>% 
    #   tidytable::mutate(value = round(temp - avg, digits = 5)) %>% 
    #   tidytable::select(year, value) -> env_indx
    
    env_indx <-      hycom_mnt %>%
        dplyr::rename_all(tolower) %>%
        tidytable::filter(month == i) %>%
        tidytable::select(year, temp = temp_depth)
    
    env_indx <- env_indx %>% tidytable::mutate(value = round(temp, digits = 5)) %>% 
      tidytable::select(year, value)
    
    # format for ss3 data file
    ss3indx <- ss3_envlnk(na.omit(env_indx),
                          var = 1)
    
    # replace the env data in the ss3 dat file with this new data
    datafile$envdat[which(datafile$envdat$year>1994),] <- ss3indx[,-c(4:5)] #this is diff, maybe it's the after 94 issue?
    
    # and, write datafile
    r4ss::SS_writedat_3.30(datafile,
                           here::here(here::here('rsch', 'HYCOM_months_depths', temp_folder), datafilename), 
                           overwrite = TRUE)
    
    # almost to end, get your operating system's exe name to be able to run model
    #exename <- ss3_exename(dir = here::here('rsch', 'llq_cfsr'))
    
    # now, run the model (note that there are other arguments in this function to check out)
    run_ss3_model(folder = paste0('rsch/','HYCOM_months_depths/', temp_folder),
                  exename)
    
    
  }
}







#read and evaluate results----


#LOOP to extract likelihoods from all those models==============================

hycom_models_lik_df <- data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("month", "depth", "total_likelihood", "aic", "delta_aic"))))

folder_list <- list.files(path = (here::here('rsch', 'HYCOM_months_depths')),full.names=FALSE,recursive=FALSE)


g<-1
for(g in 1:length(folder_list)){
  
  temp_folder <- folder_list[g]
  parts <- str_split(temp_folder, "_")[[1]]
  temp_depth <- paste0(parts[4], "-", parts[5])
  parts2 <- str_split(parts[3], "-")[[1]]
  temp_month <- parts2[1]
  
  # get model output for base cfsr llq model and alt model
  output_temp <- r4ss::SSgetoutput(dirvec = c(here::here('rsch','llq'),
                                              here::here('rsch', 'HYCOM_months_depths', temp_folder)))
  
  # run function to summarize output into a list, a big list...
  summ_out_temp <- r4ss::SSsummarize(output_temp)
  
  hycom_models_lik_df[g,1] <- temp_month
  hycom_models_lik_df[g,2] <- temp_depth
  hycom_models_lik_df[g,3] <- summ_out_temp$likelihoods[1,2]
  
  #calc AIC to base
  #EDIT HERE!!
  aic_mod1 <- (2*summ_out_temp$npars[1]) - (2*summ_out_temp$likelihoods[1,1]) #need to check these calcs seems weird
  aic_mod2 <- (2*summ_out_temp$npars[2]) - (2*summ_out_temp$likelihoods[1,2])
  delta <- aic_mod2-aic_mod1
  
  hycom_models_lik_df[g,4] <- aic_mod2
    hycom_models_lik_df[g,5] <- delta
  
}

write_csv(hycom_models_lik_df, file="data/hycom_month_comp_models.csv")

hycom_models_lik_df$depth <- factor(hycom_models_lik_df$depth, ordered=TRUE, levels=c("hycom-50m",  "hycom-100m", "hycom-150m"))

#plot liklihoods
p1 <- ggplot(hycom_models_lik_df, aes(month, total_likelihood, col=depth)) + geom_point() + 
  theme_bw() + ylab("Total model negative log(likelihood)") +
  xlab("Month") + guides(fill=guide_legend(title="Depth")) + scale_colour_brewer(palette = "Set2", 
                                                                                 labels=c("HYCOM 50m",
                                                                                          "HYCOM 100m",
                                                                                          "HYCOM 150m"))

ggplot(hycom_models_lik_df, aes(month, aic, col=depth)) + geom_point()

#lets look at best temp
temp_folder <- folder_list[14]#150m may
output_best <- r4ss::SSgetoutput(dirvec = c(here::here('rsch', 'llq'),
                                            here::here('rsch', 'HYCOM_months_depths', temp_folder)))
output_best2 <- r4ss::SSgetoutput(dirvec = 
                                            here::here('rsch', 'HYCOM_months_depths', temp_folder))



# run function to summarize output into a list, a big list...
summ_out_best <- r4ss::SSsummarize(output_best)
r4ss::SStableComparisons(summ_out_best)
  r4ss::SStableComparisons(summ_out_best, names="Calc_Q") #won't add Q row, why???
summ_out_best2 <- r4ss::SSsummarize(output_best2)
#summ_outH <- r4ss::SSsummarize(outputH)
best_out <- r4ss::SS_output(dir = here::here('rsch', 'HYCOM_months_depths', temp_folder))

r4ss::SSplotComparisons(summ_out_best, print=TRUE, plotdir = here::here('rsch', 'HYCOM_months_depths', temp_folder))
r4ss::SSplotIndices(summ_out_best, subplots = 8) #check correct output is in each of these lines
r4ss::SSplotIndices(best_out, subplots=8)
r4ss::SSplotIndices(summ_out_best2, subplots = 8) #I think this is the line but not working on these
#possibly because of the error that shows up in first line of index??

#try indice plots manually



#likely need to grab indices for each model separately, then plot expected and observed 
#for the LLsurv index for each model

out1 <- r4ss::SSgetoutput(dirvec = here::here('rsch', 'llq'))
summ1 <- r4ss::SSsummarize(out1)

out2 <- r4ss::SSgetoutput(dirvec = here::here('rsch', 'HYCOM_months_depths', temp_folder))
summ2 <- r4ss::SSsummarize(out2)

ind1 <- summ1$indices #no error message
ind2 <- summ2$indices #error message in names

ind2 <- as.data.frame(unname(ind2)) #remove the note about Eff Q and Calc Q that messes up titles
colnames(ind2) <- ind2[1,] #note working error is different this time
ind2 <- ind2[-1,]
ind2$Yr <- as.numeric(ind2$Yr)
ind2$Exp <- as.numeric(ind2$Exp)

ind1$upper <- ind1$Obs + 1.96*ind1$Obs*ind1$SE
ind1$lower <- ind1$Obs - 1.96*ind1$Obs*ind1$SE

ggplot(ind1[which(ind1$Fleet_name=="LLSrv"),], aes( Yr, Obs)) + geom_point(colour="black", size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + #fixed CIs
  geom_point(aes(Yr, Exp), colour="blue", data=ind1[which(ind1$Fleet_name=="LLSrv"),]) + #base 1 should be blue
  geom_point(aes(Yr, Exp), colour="red", data=ind2[which(ind2$Fleet_name=="LLSrv"),]) +
  geom_line(aes(Yr, Exp), colour="blue", data=ind1[which(ind1$Fleet_name=="LLSrv"),]) +
  geom_line(aes(Yr, Exp), colour="red", data=ind2[which(ind2$Fleet_name=="LLSrv"),]) +
  theme_bw() + xlab("Year") + ylab("Longline survey index")

#CI from Pete
#tidytable::mutate(uci_obs = obs + 1.96 * se * obs,
 #                 lci_obs = obs - 1.96 * se * obs)
#end form Pete



#repeat above for hycom and gak1
#hycom
out3 <- r4ss::SSgetoutput(dirvec = here::here('rsch', 'llq_hycom'))
summ3 <- r4ss::SSsummarize(out3)

ind3 <- summ3$indices #error message in names

ind3 <- as.data.frame(unname(ind3)) #remove the note about Eff Q and Calc Q that messes up titles
colnames(ind3) <- ind3[1,] #note working error is different this time
ind3 <- ind3[-1,]
ind3$Yr <- as.numeric(ind3$Yr)
ind3$Exp <- as.numeric(ind3$Exp)

#gak
out4 <- r4ss::SSgetoutput(dirvec = here::here('rsch', 'llq_gak'))
summ4 <- r4ss::SSsummarize(out4)

ind4 <- summ4$indices #error message in names

ind4 <- as.data.frame(unname(ind4)) #remove the note about Eff Q and Calc Q that messes up titles
colnames(ind4) <- ind4[1,] #note working error is different this time
ind4 <- ind4[-1,]
ind4$Yr <- as.numeric(ind4$Yr)
ind4$Exp <- as.numeric(ind4$Exp)

ggplot(ind1[which(ind1$Fleet_name=="LLSrv"),], aes( Yr, Obs)) + geom_point(colour="black", size=2) +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + #clearly not working fix here
  geom_point(aes(Yr, Exp), colour="blue", data=ind1[which(ind1$Fleet_name=="LLSrv"),]) +
  geom_point(aes(Yr, Exp), colour="red", data=ind3[which(ind3$Fleet_name=="LLSrv"),]) +
  geom_point(aes(Yr, Exp), colour="purple", data=ind4[which(ind4$Fleet_name=="LLSrv"),]) +
  geom_line(aes(Yr, Exp), colour="blue", data=ind1[which(ind1$Fleet_name=="LLSrv"),]) +
  geom_line(aes(Yr, Exp), colour="red", data=ind3[which(ind3$Fleet_name=="LLSrv"),]) +
  geom_line(aes(Yr, Exp), colour="purple", data=ind4[which(ind4$Fleet_name=="LLSrv"),]) +
  theme_bw() + xlab("Year") + ylab("Longline survey index")



r4ss::SStableComparisons(summ_out_best2)

ind_test <- SS_output(dir = here::here('rsch', 'HYCOM_months_depths', temp_folder))
SSplotIndices(ind_test)

#best_out <- r4ss::SS_output(dir = here::here('rsch', 'HYCOM_months_depths', temp_folder))
r4ss::SS_plots(best_out)
SSplotIndices(best_out)


#trying to plot Q manually
df_outbest_indices <- as.data.frame(unname(summ_out_best$indices)) #remove the note about Eff Q and Calc Q that messes up titles
colnames(df_outbest_indices) <- df_outbest_indices[1,]
df_outbest_indices <- df_outbest_indices[-1,]

df_LLind <- df_outbest_indices[which(df_outbest_indices$Fleet_name=="LLSrv"),]
ggplot(df_LLind, aes(Obs, Yr, col=replist1)) + geom_point()

df_outbest2_indices <- as.data.frame(unname(summ_out_best2$indices)) #remove the note about Eff Q and Calc Q that messes up titles
colnames(df_outbest2_indices) <- df_outbest2_indices[1,] #rename
df_outbest2_indices$Eff_Q <- as.numeric(df_outbest2_indices$Eff_Q)
df_outbest2_indices$Yr <- as.numeric(df_outbest2_indices$Yr)
ggplot(df_outbest2_indices[which(df_outbest2_indices$Fleet_name=="LLSrv"),], aes(Yr, Eff_Q)) + geom_point() + geom_line()

df_outbest2_indices$Calc_Q <- as.numeric(df_outbest2_indices$Calc_Q)
ggplot(df_outbest2_indices[which(df_outbest2_indices$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q)) + geom_point() + geom_line()


#try manual solution on a model that's previously worked
#hycom model which does not have the note
ggplot(summ_outH$indices[which(summ_outH$indices$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q, col=as.factor(imodel))) + geom_point() + geom_line()

#let's plot all models together
#first well behaving models
#then poorly behaving separately so I can manipulate them separately

mega_out <- r4ss::SSgetoutput(dirvec = c(here::here('base_mdl'),
                                         here::here('rsch', 'llq'),
                                         here::here('rsch', 'llq_hycom'),
                                         here::here('rsch', 'llq_gak')))
mega_summ <- r4ss::SSsummarize(mega_out)

#trying to read in separately
meg1 <- r4ss::SSgetoutput(dirvec = c(here::here('base_mdl')))
meg2 <- r4ss::SSgetoutput(dirvec = c(here::here('rsch', 'llq')))
meg3 <- r4ss::SSgetoutput(dirvec = c(here::here('rsch', 'llq_hycom')))
meg4 <- r4ss::SSgetoutput(dirvec = c(here::here('rsch', 'llq_gak')))
meg5 <- r4ss::SSgetoutput(dirvec = c(here::here('rsch', 'HYCOM_months_depths', temp_folder)))
meg1sum <- r4ss::SSsummarize(meg1)
meg2sum <- r4ss::SSsummarize(meg2)
meg3sum <- r4ss::SSsummarize(meg3)
meg4sum <- r4ss::SSsummarize(meg4)
meg5sum <- r4ss::SSsummarize(meg5)

meg3sum_ind <- as.data.frame(unname(meg3sum$indices), col.names=1) 
colnames(meg3sum_ind) <- meg3sum_ind[1,]
meg3sum_ind$Yr <- as.numeric(meg3sum_ind$Yr)
meg3sum_ind$Calc_Q <- as.numeric(meg3sum_ind$Calc_Q)

meg4sum_ind <- as.data.frame(unname(meg4sum$indices), col.names=1) 
colnames(meg4sum_ind) <- meg4sum_ind[1,]
meg4sum_ind$Yr <- as.numeric(meg4sum_ind$Yr)
meg4sum_ind$Calc_Q <- as.numeric(meg4sum_ind$Calc_Q)

meg5sum_ind <- as.data.frame(unname(meg5sum$indices), col.names=1) 
colnames(meg5sum_ind) <- meg5sum_ind[1,]
meg5sum_ind$Yr <- as.numeric(meg5sum_ind$Yr)
meg5sum_ind$Calc_Q <- as.numeric(meg5sum_ind$Calc_Q)





head(mega_summ$indices) #if I include new model, also messed up but note is at end
#meg_indices <- as.data.frame(unname(mega_summ$indices)) #remove the note about Eff Q and Calc Q that messes up titles
#meg_indices <- as.data.frame(mega_summ$indices[,c(1:19)]) #dropping first row didn't work same way this time, note put columns at end
#colnames(meg_indices) <- meg_indices[1,] #rename
#this solution to dropping error note DOES NOT WORK for hycom and gak models
meg_indices <- as.data.frame(mega_summ$indices[,c(1:3, 23:38)])
meg_indices$Calc_Q <- as.numeric(meg_indices$Calc_Q)
#meg_indices$Yr <- as.numeric(meg_indices$Yr)
meg_indices$model_name <- NA
meg_indices$model_name[which(meg_indices$imodel==1)]<-"base"
meg_indices$model_name[which(meg_indices$imodel==2)]<-"cfsr"
meg_indices$model_name[which(meg_indices$imodel==3)]<-"hycom"
meg_indices$model_name[which(meg_indices$imodel==4)]<-"gak"

ggplot(meg_indices[which(meg_indices$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q, col=as.factor(model_name))) + geom_point() + geom_line()

#trying to solve error message issue
#plot Q for three different metrics
ggplot(meg1sum$indices[which(meg1sum$indices$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q)) + geom_point(col="blue") + geom_line(col="blue")+
   geom_point(data=meg3sum_ind[which(meg3sum_ind$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q), col="red") + 
  geom_line(data=meg3sum_ind[which(meg3sum_ind$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q), col="red") +#
  geom_point(data=meg4sum_ind[which(meg4sum_ind$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q), col="purple") + 
  geom_line(data=meg4sum_ind[which(meg4sum_ind$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q), col="purple") +
  theme_bw() + xlab("Year") + ylab("Catchability in the longline survey") #+
  geom_text() + annotate("text", label="CFSR", y=-1, x=2000, colour="black")


#compare base to hycom may 150m
ggplot(meg1sum$indices[which(meg1sum$indices$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q)) + geom_point(col="red") + geom_line(col="red")+
  geom_point(data=meg5sum_ind[which(meg3sum_ind$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q), col="blue") + 
  geom_line(data=meg5sum_ind[which(meg3sum_ind$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q), col="blue") +#
  # geom_point(data=meg4sum_ind[which(meg4sum_ind$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q), col="purple") + 
  # geom_line(data=meg4sum_ind[which(meg4sum_ind$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q), col="purple") +
  theme_bw() + xlab("Year") + ylab("Catchability in the longline survey") 
  
#read in and plot Q for new models
new_out <- r4ss::SSgetoutput(dirvec = c(here::here('rsch', 'HYCOM_months_depths', 'HYCOM_month_4-depth_hycom_50m'),
                                                   here::here('rsch', 'HYCOM_months_depths', 'HYCOM_month_4-depth_hycom_100m'),
                                                              here::here('rsch', 'HYCOM_months_depths', 'HYCOM_month_4-depth_hycom_150m'),
                                        here::here('rsch', 'HYCOM_months_depths', 'HYCOM_month_5-depth_hycom_50m'),
                                                   here::here('rsch', 'HYCOM_months_depths', 'HYCOM_month_5-depth_hycom_100m'),
                                         here::here('rsch', 'HYCOM_months_depths', 'HYCOM_month_5-depth_hycom_150m'),
                                        here::here('rsch', 'HYCOM_months_depths', 'HYCOM_month_6-depth_hycom_50m'),
                                                    here::here('rsch', 'HYCOM_months_depths', 'HYCOM_month_6-depth_hycom_100m'),
                                                               here::here('rsch', 'HYCOM_months_depths', 'HYCOM_month_6-depth_hycom_150m')))
new_summ <- r4ss::SSsummarize(new_out)

new_indices <- as.data.frame(unname(new_summ$indices)) #remove the note about Eff Q and Calc Q that messes up titles
colnames(new_indices) <- new_indices[1,] #rename

new_indices$model_name <- NA
new_indices$model_name[which(new_indices$replist1=="replist1")]<-"april_50m"
new_indices$model_name[which(new_indices$replist1=="replist2")]<-"april_100m"
new_indices$model_name[which(new_indices$replist1=="replist3")]<-"april_150m"
new_indices$model_name[which(new_indices$replist1=="replist4")]<-"may_50m"
new_indices$model_name[which(new_indices$replist1=="replist5")]<-"may_100m"
new_indices$model_name[which(new_indices$replist1=="replist6")]<-"may_150m"
new_indices$model_name[which(new_indices$replist1=="replist7")]<-"june_50m"
new_indices$model_name[which(new_indices$replist1=="replist8")]<-"june_100m"
new_indices$model_name[which(new_indices$replist1=="replist9")]<-"june_150m"

new_indices$month <- NA
new_indices$month[which(new_indices$replist1=="replist1")]<-"april"
new_indices$month[which(new_indices$replist1=="replist2")]<-"april"
new_indices$month[which(new_indices$replist1=="replist3")]<-"april"
new_indices$month[which(new_indices$replist1=="replist4")]<-"may"
new_indices$month[which(new_indices$replist1=="replist5")]<-"may"
new_indices$month[which(new_indices$replist1=="replist6")]<-"may"
new_indices$month[which(new_indices$replist1=="replist7")]<-"june"
new_indices$month[which(new_indices$replist1=="replist8")]<-"june"
new_indices$month[which(new_indices$replist1=="replist9")]<-"june"

new_indices$depth <- NA
new_indices$depth [which(new_indices$replist1=="replist1")]<-"50m"
new_indices$depth [which(new_indices$replist1=="replist2")]<-"100m"
new_indices$depth [which(new_indices$replist1=="replist3")]<-"150m"
new_indices$depth [which(new_indices$replist1=="replist4")]<-"50m"
new_indices$depth [which(new_indices$replist1=="replist5")]<-"100m"
new_indices$depth [which(new_indices$replist1=="replist6")]<-"150m"
new_indices$depth [which(new_indices$replist1=="replist7")]<-"50m"
new_indices$depth [which(new_indices$replist1=="replist8")]<-"100m"
new_indices$depth [which(new_indices$replist1=="replist9")]<-"150m"


new_indices$Eff_Q <- as.numeric(new_indices$Eff_Q)
new_indices$Yr <- as.numeric(new_indices$Yr)

new_indices$month <- factor(new_indices$month, ordered=TRUE, levels=c("april", "may", "june"))
new_indices$depth <- factor(new_indices$depth, ordered=TRUE, levels=c("50m",  "100m", "150m"))


ggplot(new_indices[which(new_indices$Fleet_name=="LLSrv"),], aes(Yr, Eff_Q, col=as.factor(imodel))) + geom_point() + geom_line()

new_indices$Calc_Q <- as.numeric(new_indices$Calc_Q)
ggplot(new_indices[which(new_indices$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q, col=model_name)) + geom_point() + geom_line()

p2 <- ggplot(new_indices[which(new_indices$Fleet_name=="LLSrv"),], aes(Yr, Calc_Q, col=month)) + geom_point() + geom_line() + 
  facet_wrap(~depth) + theme_bw() + xlab("Year") + ylab("Q") + scale_colour_brewer(palette = "Dark2")


library(cowplot)
plot_grid(p1, p2, ncol=1)

#Loop again to plot indices===================================================================================

#LOOPING THROUGH MONTHS AND DEPTHS=====

depths <- c("hycom_50m", "hycom_100m", "hycom_150m")

indx_df <- data.frame()

i<-1
j<-1
for(i in 1:6){
  for(j in 1:length(depths)){
    
    temp_depth <- depths[j]
    
    temp_folder <- paste0("HYCOM_month_", i, "-depth_", temp_depth)
    
    # first, start new model folder within 'rsch' folder
    start_ss_fldr(from = here::here('base_mdl'), 
                  to = here::here('rsch', 'HYCOM_months_depths', temp_folder))
    
    # next, read in data file to be able to change env link data
    # define datafile name
    datafilename <- list.files(here::here('rsch', 'HYCOM_months_depths', temp_folder), pattern = "GOAPcod")
    # get datafile input
    datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'HYCOM_months_depths', temp_folder, datafilename))
    
    # and then, change the env data

    
    env_indx <-      hycom_mnt %>%
      dplyr::rename_all(tolower) %>%
      tidytable::filter(month == i) %>%
      tidytable::select(year, temp = temp_depth)
    
    env_indx <- env_indx %>% tidytable::mutate(value = round(temp, digits = 5)) %>% 
      tidytable::select(year, value)
    
    # format for ss3 data file
    ss3indx <- ss3_envlnk(na.omit(env_indx),
                          var = 1)
    ss3indx$month <- i
    ss3indx$depth <- temp_depth
    
    # replace the env data in the ss3 dat file with this new data
    indx_df <- rbind(indx_df, ss3indx)

  }
}


ggplot(indx_df, aes(year, value, col=depth)) + facet_wrap(~month) + geom_line() + geom_point() #compare depths


ggplot(indx_df, aes(year, value, col=as.factor(month))) + facet_wrap(~depth) + geom_line() + geom_point() #compare months

ggplot(indx_df[which(indx_df$depth=="hycom_50m"),], aes(year, value, col=as.factor(month))) + geom_line() + geom_point() 



#plot LL surv index using Pete's code============================

#get results and save llq_res.RData

# get results

# read base results
res_base <- r4ss::SS_output(dir = here::here('rsch', 'llq'),
                            verbose = FALSE,
                            printstats = FALSE)
# read no covariate results
res_run_nocov <- suppressWarnings(r4ss::SS_output(dir = here::here('rsch', 'llq_hycom'),
                                                  covar = FALSE,
                                                  verbose = FALSE,
                                                  printstats = FALSE))

# set up likelihood results
res_base$likelihoods_used %>% 
  tidytable::mutate(like_compon = rownames(res_base$likelihoods_used)) %>% 
  tidytable::select(like_compon, values) %>% 
  tidytable::filter(like_compon == "TOTAL") %>% 
  tidytable::bind_rows(res_base$likelihoods_by_fleet %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(label, llsrv) %>% 
                         tidytable::filter(label %in% c("Surv_like", "Length_like")) %>% 
                         tidytable::rename(like_compon = "label",
                                           values = "llsrv")) %>% 
  tidytable::mutate(model = "base") %>% 
  tidytable::bind_rows(res_run_nocov$likelihoods_used %>% 
                         tidytable::mutate(like_compon = rownames(res_run_nocov$likelihoods_used)) %>% 
                         tidytable::select(like_compon, values) %>% 
                         tidytable::filter(like_compon == "TOTAL") %>% 
                         tidytable::bind_rows(res_run_nocov$likelihoods_by_fleet %>% 
                                                dplyr::rename_all(tolower) %>% 
                                                tidytable::select(label, llsrv) %>% 
                                                tidytable::filter(label %in% c("Surv_like", "Length_like")) %>% 
                                                tidytable::rename(like_compon = "label",
                                                                  values = "llsrv")) %>% 
                         tidytable::mutate(model = "no_cov")) %>% 
  tidytable::bind_rows(do.call(mapply, c(list, rand_res, SIMPLIFY = FALSE))$likes %>% 
                         tidytable::map_df(., ~as.data.frame(.x), .id = "rand") %>% 
                         tidytable::mutate(model = paste0("rand", rand)) %>% 
                         tidytable::select(like_compon, values, model)) -> likes

# set up predicted ll surv results
res_base$cpue %>% 
  tidytable::filter(Fleet == 5) %>% 
  tidytable::select(Yr, Exp) %>% 
  tidytable::rename(year = 'Yr',
                    values = 'Exp') %>% 
  tidytable::mutate(model = "base") %>% 
  tidytable::bind_rows(res_run_nocov$cpue %>% 
                         tidytable::filter(Fleet == 5) %>% 
                         tidytable::select(Yr, Exp) %>% 
                         tidytable::rename(year = 'Yr',
                                           values = 'Exp') %>% 
                         tidytable::mutate(model = "no_cov")) %>% #update model names
  tidytable::bind_rows(do.call(mapply, c(list, rand_res, SIMPLIFY = FALSE))$pred_ll %>% #what to do with rand_res which I think is simulations
                         tidytable::map_df(., ~as.data.frame(.x), .id = "rand") %>% 
                         tidytable::mutate(model = paste0("rand", rand)) %>% 
                         tidytable::select(year, values, model)) -> pred_ll

# output
llq_res <- list(likes = likes, pred_ll = pred_ll)

# save results
if(isTRUE(full_run)){
  if (!dir.exists(here::here(new_year, "output", "llq"))) {
    dir.create(here::here(new_year, "output", "llq"), recursive = TRUE)
  }
  save(llq_res, file = here::here(new_year, "output", "llq", "llq_res.RData"))
}





#get and plot llq_res

#' function to plot ll survey catchability sensitivity
#' 
#' @param rec_mdl_res list of ss3 results for recommended model (default = NULL)
#' @param new_year current assessment year (default = NULL)  
#' 
plot_llq <- function(rec_mdl_res = NULL,
                     new_year = NULL){
  
  # get plot data
  load(here::here(new_year, "output", "llq", "llq_res.RData"))
  
  llq_res$pred_ll %>% 
    tidytable::filter(model %in% c('base', 'no_cov')) %>% 
    tidytable::pivot_wider(names_from = model, values_from = values) %>% 
    tidytable::left_join(llq_res$pred_ll %>% 
                           tidytable::filter(!(model %in% c('base', 'no_cov'))) %>% 
                           tidytable::summarise(mean_rand = mean(values),
                                                uci_rand = max(values),
                                                lci_rand = min(values), .by = year)) %>% 
    tidytable::left_join(rec_mdl_res$cpue %>% 
                           dplyr::rename_all(tolower) %>% 
                           tidytable::filter(fleet == 5) %>% 
                           tidytable::mutate(uci_obs = obs + 1.96 * se * obs,
                                             lci_obs = obs - 1.96 * se * obs) %>% 
                           tidytable::select(year = yr, obs, uci_obs, lci_obs)) %>% 
    tidytable::left_join(llq_res$pred_ll %>% 
                           tidytable::filter(model %in% c('rand37', 'rand21')) %>% 
                           tidytable::pivot_wider(names_from = model, values_from = values)) -> llq_dat
  
  colors <- c("No covariate" = scico::scico(3, palette = 'roma')[1], 
              "Recommended" = scico::scico(3, palette = 'roma')[3], 
              "Random" = "darkgrey")
  
  # plot
  llq <- ggplot(data = llq_dat, 
                aes(x = year, y = obs, ymin = lci_obs, ymax = uci_obs)) +
    geom_ribbon(aes(ymin = lci_rand, ymax = uci_rand), fill = "grey", alpha = 0.5) +
    geom_line(aes(x = year, y = rand21, color = "Random"), linewidth = 1) +
    geom_line(aes(x = year, y = rand37, color = "Random"), linewidth = 1) + 
    geom_point() +
    geom_linerange() +
    geom_line(aes(x = year, y = no_cov, color = "No covariate"), linewidth = 1) + 
    geom_line(aes(x = year, y = base, color = "Recommended"), linewidth = 1) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "top") +
    labs(x = "Year",
         y = "LL Survey RPN",
         color = "Catchability") +
    scale_color_manual(values = colors)
  
  # save
  ggsave(filename = "llq.png",
         path = here::here(new_year, "output", "safe_plots"),
         width = 6.5,
         height = 4.5,
         units = "in")
  
}















