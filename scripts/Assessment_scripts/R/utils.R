#' utility fcns 
#' 
#' 
#' function to get systems ss3 exe name
#' @param supported_os supported operting systems (default = c("Windows", "Darwin"))
#' 
ss3_exename <- function(dir,
                        supported_os = c("Windows", "Darwin")){
  
  # Get the operating system information
  os_info <- Sys.info()
  
  # figure out the ss3 executable name to run
  # list the possible exes
  executables <- list("Windows" = c("ss.exe", "ss3.exe"), "Darwin" = c("ss3"))
  # set the exe name
  if (os_info["sysname"] %in% names(executables)) {
    # Get the list of executables for the detected operating system
    os_executables <- executables[[os_info["sysname"]]]
    # Check if any of the executables exist
    existing_executables <- os_executables[file.exists(here::here(dir, os_executables))]
    if (length(existing_executables) > 0) {
      # set the name of the executable
      exe_name = existing_executables[1]
    } else {
      print("No executable found for the operating system.")
      stop()
    }
  }
  
  exe_name
  
}

#' function to run ss3 model with recruitment bias ramp adjustment
#' @param folder folder containing model you want run (default = NULL)
#' @param exename name of executable file (default = NULL)
#' @param rec_ramp boolean, whether to run the recruitment ramp bias adjustment (default = FALSE)
#' @param ctl_filename name of ctl file in which to adjust recruitment ramp parameters (default = NULL)
#' @param iters the number of iters to run for rec ramp (default = 2)
#' @param rewrite_starter boolean, whether to rewrite starter file to start params at mle estimates from previous run (default = FALSE)
#' 
run_ss3_model <- function(folder = NULL,
                          exename = NULL, 
                          rec_ramp = FALSE,
                          ctl_filename = NULL,
                          iters = 2,
                          rewrite_starter = FALSE){
  
  # load fcns
  source(here::here("R", "utils.R"), local = TRUE)
  
  # set init values in starter file to 0
  mdl_starter <- r4ss::SS_readstarter(file = here::here(folder, "starter.ss"))
  mdl_starter$init_values_src = 0
  r4ss::SS_writestarter(mdl_starter, 
                        dir = here::here(folder),
                        overwrite = TRUE)
  
  # run model
  r4ss::run(dir = here::here(folder),
            exe = exename,
            skipfinished = FALSE)
  
  # now, iterate model iters times to settle on recruitment bias ramp
  if(isTRUE(rec_ramp)){
    purrr::map(1:iters, ~get_recr_ramp(folder, ctl_filename))
  }
  
  # set init vals to use par file in starter file for further runs
  if(isTRUE(rewrite_starter)){
    mdl_starter <- r4ss::SS_readstarter(file = here::here(folder, "starter.ss"))
    mdl_starter$init_values_src = 1
    r4ss::SS_writestarter(mdl_starter, 
                          dir = here::here(folder),
                          overwrite = TRUE)
  }
}

# function to get recruitment ramp
#' @param asmnt_yr year of assessment (default = NULL)
#' @param mdl name of model to run (default = NULL)
#' 
get_recr_ramp <- function(folder, ctl_filename){
  # update recruitment bias ramp ests in ctl file
  mdl_res <- r4ss::SS_output(dir = here::here(folder),
                             verbose = FALSE,
                             printstats = FALSE)
  rec_ramp <- r4ss::SS_fitbiasramp(mdl_res,
                                   plot = FALSE)
  ctl <- r4ss::SS_readctl_3.30(here::here(folder, ctl_filename))
  ctl$last_early_yr_nobias_adj <- rec_ramp$newbias$par[1]
  ctl$first_yr_fullbias_adj <- rec_ramp$newbias$par[2]
  ctl$last_yr_fullbias_adj <- rec_ramp$newbias$par[3]
  ctl$first_recent_yr_nobias_adj <- rec_ramp$newbias$par[4]
  ctl$max_bias_adj <- rec_ramp$newbias$par[5]
  r4ss::SS_writectl_3.30(ctllist = ctl,
                         outfile = here::here(folder, ctl_filename),
                         overwrite = TRUE)
  # run model
  r4ss::run(dir = here::here(folder),
            skipfinished = FALSE,
            verbose = FALSE)
}

#' function to set up folder with ss3 model files and exe
#' @param from folder containing ss3 files that are to be copied (default = NULL)
#' @param to destination folder for ss3 files (default = NULL)
#' 
start_ss_fldr <- function(from, to){
  
  # set up folder
  if(!dir.exists(to)){
    dir.create(to, recursive = TRUE)
  }
  # get model input files
  r4ss::copy_SS_inputs(dir.old = from, 
                       dir.new = to,
                       overwrite = TRUE,
                       verbose = FALSE)
  # get data_echo file so you can make changes to ctl file
  file.copy(paste0(from, '/data_echo.ss_new'),
            paste0(to, '/data_echo.ss_new'))
  # get exe
  invisible(r4ss::get_ss3_exe(dir = to))
  
}

#' function to format environmental link data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param var variable column for ss3 data file (default = NULL)
#' 
ss3_envlnk <- function(data = NULL,
                       var = NULL){
  data.frame(data %>% 
               tidytable::mutate(variable = var) %>% 
               tidytable::select(year, variable, value))
}

