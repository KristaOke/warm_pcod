# warm_pcod
Analyses on temperature impacts on Pacific cod dynamics

Collaborators:
Krista Oke, FAST Lab, APU; contact: kristaoke@gmail.com;
Pete Hulson, NOAA AFSC;
Steve Barbeaux, NOAA AFSC;
Brad Harris, FAST Lab, APU

SCRIPTS READ ME

Temperature data is loaded and manipulated in script get_temp_data.R
  Data to run analyses is archived but raw temperature data is often too large to archive.
  To rerun analyses, use archived data provided with manuscript.
  Raw data processing performed in:
  --HYCOM_scripts/hycom_download_post2024-ESPC.R
  --- HYCOM data is available publicly and can be downloaded following this script
  --HYCOM_scripts/goa_obs.R
  --- GOA survey observations

Stock assessment model files are in folder scripts/Assessment_scripts
--different metric are compared in diff_temp_indices_II_catchability.R
--different months/depths are compared in diff_month_catchability.R

Figures 3 and 4 created in cor_by_month.R

Figures 2 and S4 created in temp_plots_2024_assess.R

Assessment figures created in diff_month_catchability.R




