library(akgfmaps)

all_obs_w_depth <- readRDS("data/all_obs_w_depth.RDS") %>% 
  filter(latitude > 50, longitude > -180)

# Get GOA stat areas to use for a mask ('auto' returns Alaska Albers)
nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = 'auto') 

all_stn <- all_obs_w_depth %>% distinct(stn_id, latitude, longitude) %>% 
  mutate(lat = latitude, lon = longitude)
dat_sf <- st_as_sf(all_stn, coords = c("lon", "lat"), crs = "EPSG:4326") %>% 
  st_transform(crs(nmfs_areas))

# ggplot(nmfs_areas) + geom_sf() + geom_sf(data = bot_sf)

dat_w_area <- st_join(dat_sf, nmfs_areas) %>% 
  mutate(esr = ifelse((REP_AREA > 609 & !REP_AREA == 649 & !REP_AREA == 659 & longitude > -164.0001 & longitude < 0), "GOA",
                      ifelse(REP_AREA %in% c(518, 519, 541, 542, 543, 610), 'AI',
                             ifelse(!is.na(REP_AREA) & !REP_AREA == 649 & !REP_AREA == 659 , "EBS", NA)))) %>% 
  mutate(mod_reg = ifelse(is.na(esr), "GOA", esr))

ggplot(dat_w_area) + geom_sf(aes(col = esr))
ggplot(dat_w_area %>% filter(esr == "GOA")) + geom_sf(aes(col = esr))
# ggplot(dat_w_area) + geom_sf(aes(col = mod_reg))

goa_obs_w_depth <- left_join(all_obs_w_depth, dat_w_area) %>% 
  filter(esr == "GOA")

ggplot(goa_obs_w_depth) + geom_point(aes(longitude, latitude)) 

saveRDS(goa_obs_w_depth, "data/goa_obs_w_depth.rds")