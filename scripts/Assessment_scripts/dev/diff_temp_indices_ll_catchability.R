# script to run temperature links for goa pcod ----
#by Krista, Nov 2024, borrowing heavily from Pete's script
#compare CFSR, HYCOM, GAK1

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

# first, start new model folder within 'rsch' folder
start_ss_fldr(from = here::here('base_mdl'), 
              to = here::here('rsch', 'llq_cfsr'))

# next, read in data file to be able to change env link data
# define datafile name
datafilename <- list.files(here::here('rsch', 'llq_cfsr'), pattern = "GOAPcod")
# get datafile input
datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'llq_cfsr', datafilename))

# and then, change the env data

# get cfsr data
cfsr <- vroom::vroom(here::here('data', 'raw_cfsr.csv'))

# compute env index
cfsr %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(month == 6) %>% 
  tidytable::select(year, temp = '0_20') %>% 
  tidytable::mutate(avg = as.numeric(cfsr %>% 
                                       dplyr::rename_all(tolower) %>% 
                                       tidytable::filter(month == 6,
                                                         year >= 1982,
                                                         year <= 2012) %>% 
                                       tidytable::select(year, temp = '0_20') %>% 
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
                       here::here(here::here('rsch', 'llq_cfsr'), datafilename), 
                       overwrite = TRUE)

# almost to end, get your operating system's exe name to be able to run model
#exename <- ss3_exename(dir = here::here('rsch', 'llq_cfsr'))

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'rsch/llq_cfsr',
              exename)


# HYCOM link with longline survey catchability ----

# first, start new model folder within 'rsch' folder
start_ss_fldr(from = here::here('base_mdl'), 
              to = here::here('rsch', 'llq_hycom')) #

# next, read in data file to be able to change env link data
# define datafile name
datafilename <- list.files(here::here('rsch', 'llq_hycom'), pattern = "GOAPcod")
# get datafile input
datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'llq_hycom', datafilename))

# and then, change the env data

# get hycom data
temp_dat <- vroom::vroom(here::here('data', 'demeaned_temp_by_month.csv')) #updated June 2025 to demeaned data w pre-2012 mean
temp_dat <- temp_dat %>% rename(hycom='mean_monthly',
                                        gak='gak_best_temp',
                                        cfsr='cfsr_temp')

jun_50_hycom <- temp_dat[which(temp_dat$Month==6&temp_dat$depth=="50m"),names(temp_dat) %in% 
                        c("Year","hycom")]
jun_50_hycom <- na.omit(jun_50_hycom)
# hycom is already monthly mean and z-scored
#here I am using 50m depth and June mean



env_indx_hycom <- jun_50_hycom

#rename to work with function
env_indx_hycom <- env_indx_hycom %>% rename(year='Year',
                                      value='hycom')

# format for ss3 data file
ss3indx_hycom <- ss3_envlnk(env_indx_hycom,
                      var = 1)

# replace the env data in the ss3 dat file with this new data
#stitch hycom (starts in 1995) to cfsr before 1994
datafile$envdat[which(datafile$envdat$year>1994),] <- ss3indx_hycom[which(ss3indx_hycom$year<2025),]

# and, write datafile
r4ss::SS_writedat_3.30(datafile,
                       here::here(here::here('rsch', 'llq_hycom'), datafilename), 
                       overwrite = TRUE) #this step seems to not be working

# almost to end, get your operating system's exe name to be able to run model
#exename <- ss3_exename(dir = here::here('rsch', 'llq_hycom'))

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'rsch/llq_hycom',
              exename)




# GAK link with longline survey catchability ----

# first, start new model folder within 'rsch' folder
start_ss_fldr(from = here::here('base_mdl'), 
              to = here::here('rsch', 'llq_gak'))#

# next, read in data file to be able to change env link data
# define datafile name
datafilename <- list.files(here::here('rsch', 'llq_gak'), pattern = "GOAPcod")
# get datafile input
datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'llq_gak', datafilename))

# and then, change the env data

jun_50_gak <- temp_dat[which(temp_dat$Month==6&temp_dat$depth=="50m"),names(temp_dat) %in% 
                           c("Year","gak")]
#jun_50_gak <- na.omit(jun_50_gak)
# gak is already monthly mean and z-scored
#here I am using 50m depth and June mean

#fill in zeros for NAs, but new gak mostly complete after 1988 (except 1998)
jun_50_gak$gak[which(is.na(jun_50_gak$gak)==TRUE)] <- 0
jun_50_gak <- jun_50_gak[which(jun_50_gak$Year>1987),]

env_indx_gak <- jun_50_gak

#rename to work with function
env_indx_gak <- env_indx_gak %>% rename(year='Year',
                                            value='gak')

# format for ss3 data file
ss3indx_gak <- ss3_envlnk(env_indx_gak,
                            var = 1)

# replace the env data in the ss3 dat file with this new data
#stitch gak (starts CONSISTENTLY in 1988) to cfsr before 1988
datafile$envdat[which(datafile$envdat$year>1987),] <- ss3indx_gak[which(ss3indx_gak$year>1987&ss3indx_gak$year<2025),] #updated June 2025
#

# and, write datafile
r4ss::SS_writedat_3.30(datafile,
                       here::here(here::here('rsch', 'llq_gak'), datafilename), 
                       overwrite = TRUE)

# almost to end, get your operating system's exe name to be able to run model
#exename <- ss3_exename(dir = here::here('rsch', 'llq_gak'))

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'rsch/llq_gak',
              exename)








#read and evaluate results----

# get model output for base model and alt model
outputH <- r4ss::SSgetoutput(dirvec = c(here::here('base_mdl'),
                                       here::here('rsch', 'llq_hycom')))
outputG <- r4ss::SSgetoutput(dirvec = c(here::here('base_mdl'),
                                        here::here('rsch', 'llq_gak')))

#compare HYCOM and GAK to CFSR
outputCH <- r4ss::SSgetoutput(dirvec = c(here::here('rsch', 'llq'),
                                        here::here('rsch', 'llq_hycom')))
outputCG <- r4ss::SSgetoutput(dirvec = c(here::here('rsch', 'llq'),
                                        here::here('rsch', 'llq_gak')))

#test <- r4ss::SS_output(dir = here::here('base_mdl'),
#printstats = FALSE)

h_out <- r4ss::SS_output(dir = here::here('rsch', 'llq_hycom'))
b_out <- r4ss::SS_output(dir = here::here('base_mdl'))
g_out <- r4ss::SS_output(dir = here::here('rsch', 'llq_gak'))
c_out <- r4ss::SS_output(dir = here::here('rsch', 'llq'))

# run function to summarize output into a list, a big list...
summ_outH <- r4ss::SSsummarize(outputH)
summ_outG <- r4ss::SSsummarize(outputG)

summ_outCH <- r4ss::SSsummarize(outputCH)
summ_outCG <- r4ss::SSsummarize(outputCG)

# example to look at likelihoods
summ_outH$likelihoods
summ_outG$likelihoods

summ_outCH$likelihoods
summ_outCG$likelihoods

# example to look at parameter estimates
summ_outH$pars
summ_outG$pars

# example function to plot output
r4ss::SSplotComparisons(summ_outH, print=TRUE, plotdir = here::here('rsch', 'llq_hycom'))
r4ss::SSplotIndices(summ_outH, subplots = 8)

r4ss::SSplotComparisons(summ_outCH, print=TRUE, plotdir = here::here('rsch', 'llq_hycom'))
r4ss::SSplotComparisons(summ_outCG, print=TRUE, plotdir = here::here('rsch', 'llq_gak'))



r4ss::SSplotComparisons(summ_outG, print=TRUE, plotdir = here::here('rsch', 'llq_gak'))

r4ss::SS_plots(h_out)
r4ss::SS_plots(b_out)
r4ss::SS_plots(g_out)
r4ss::SS_plots(c_out)


#calc AIC to base
aic_mod1 <- (2*summ_outH$npars[1]) - (2*summ_outH$likelihoods[1,1])
aic_mod2 <- (2*summ_outH$npars[2]) - (2*summ_outH$likelihoods[1,2])
aic_mod2-aic_mod1

aic_mod1 <- (2*summ_outG$npars[1]) - (2*summ_outG$likelihoods[1,1])
aic_mod3 <- (2*summ_outG$npars[2]) - (2*summ_outG$likelihoods[1,2])
aic_mod3-aic_mod1


#calc AIC to cfsr
aic_mod1 <- (2*summ_outCH$npars[1]) - (2*summ_outCH$likelihoods[1,1])
aic_mod2 <- (2*summ_outCH$npars[2]) - (2*summ_outCH$likelihoods[1,2])
aic_mod2-aic_mod1

aic_mod1 <- (2*summ_outCG$npars[1]) - (2*summ_outCG$likelihoods[1,1])
aic_mod3 <- (2*summ_outCG$npars[2]) - (2*summ_outCG$likelihoods[1,2])
aic_mod3-aic_mod1










# want to try out logistic ----

# first, start new model folder within 'rsch' folder
start_ss_fldr(from = here::here('base_mdl'), 
              to = here::here('rsch', 'logis'))

# next, read in data file to be able to change env link data
# define datafile name
datafilename <- list.files(here::here('rsch', 'logis'), pattern = "GOAPcod")
# get datafile input
datafile <- r4ss::SS_readdat_3.30(here::here('rsch', 'grwth', datafilename))

#use cfsr for now just change link


# next, read in ctl file to be able to change the parameters that you link to growth
# define ctl file name
ctlfilename <- list.files(here::here('rsch', 'logis'), pattern = '.ctl')
# get datafile input
ctlfile <- r4ss::SS_readctl_3.30(here::here('rsch', 'logis', ctlfilename))

# let's play with Lmin, there are 2 important steps, and here's an example to do that
# and, check out https://nmfs-ost.github.io/ss3-doc/SS330_User_Manual_release.html#tvOrder 
# for a description on how to define ss3 parameters and what links are available

ctlfile$Q_parms$`env_var&link`[which(rownames(ctlfile$Q_parms) == 'LnQ_base_LLSrv(5)')] <- 401


# and, write the new ctl file
r4ss::SS_writectl_3.30(ctllist = ctlfile,
                       outfile = here::here('rsch', 'grwth', ctlfilename),
                       verbose = TRUE,
                       overwrite = TRUE)

# almost to end, get your operating system's exe name to be able to run model
#exename <- ss3_exename(dir = here::here('rsch', 'grwth'))

# now, run the model (note that there are other arguments in this function to check out)
run_ss3_model(folder = 'rsch/logis',
              exename)

outputL <- r4ss::SSgetoutput(dirvec = c(here::here('base_mdl'),
                                        here::here('rsch', 'logis')))
summ_outL <- r4ss::SSsummarize(outputL)
summ_outL$likelihoods

r4ss::SSplotComparisons(summ_outL, print=TRUE, plotdir = here::here('rsch', 'llq_hycom'))

l_out <- r4ss::SS_output(dir = here::here('rsch', 'logis'))
r4ss::SS_plots(l_out)

