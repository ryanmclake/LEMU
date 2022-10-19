
#### Libraries #### 
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(vroom, warn.conflicts = FALSE)
library(sf, warn.conflicts = FALSE)
library(units, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(Kendall, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)

#### country partition for running in parallel if needed ####
country <- list.files(path = "./countries")
country <- gsub("\\..*", "", country)

#### Import, select, summarize, export ####
  #imports using 'arrow' 
  # calls columns f#. We maintain this until we rename before exporting
l <- read_csv_arrow(paste0("./countries/",country[1],".csv"),   
                 quote = "\"",
                 escape_double = TRUE,
                 escape_backslash = FALSE,
                 schema = NULL,
                 col_names = F,
                 col_types = NULL,
                 col_select = NULL,
                 na = c("", "NA"),
                 quoted_na = TRUE,
                 skip_empty_rows = TRUE,
                 skip = 0L,
                 parse_options = NULL,
                 convert_options = NULL,
                 read_options = NULL,
                 as_data_frame = TRUE,
                 timestamp_parsers = NULL) %>% rename(year = f0,
                                                      month=f1,
                                                      hylak_id=f2,
                                                      centr_lat=f3,
                                                      centr_lon=f4,
                                                      continent=f5,
                                                      country=f6,
                                                      bsn_lvl=f7,
                                                      hybas_id=f8,
                                                      mean_monthly_precip_mm=f9,
                                                      total_precip_mm=f10,
                                                      mean_annual_temp_k=f11,
                                                      pop_sum=f12,
                                                      seasonal_km2=f13,
                                                      permanent_km2=f14,
                                                      total_km2=f15,
                                                      lake_name=f16,
                                                      lake_type=f17,
                                                      lake_area=f18,
                                                      shore_dev=f19,
                                                      vol_total=f20,
                                                      vol_res=f21,
                                                      vol_src=f22,
                                                      depth_avg=f23,
                                                      res_time=f24,
                                                      elevation=f25,
                                                      slope_100=f26,
                                                      wshd_area=f27,
                                                      pour_long=f28,
                                                      pour_lat=f29,
                                                      sub_area=f30,
                                                      mean_spec_humidity=f31,
                                                      mean_precip_mm=f32,
                                                      sum_precip_mm=f33,
                                                      mean_temp_k=f34,
                                                      mean_totcloud_pct=f35,
                                                      mean_sw_wm2=f36,
                                                      mean_lw_wm2=f37,
                                                      above_ratio_cutoff=f38,
                                                      ice_cover_min=f39,
                                                      ice_cover_max=f40,
                                                      ice_cover_mean=f41,
                                                      ice_cover_median=f42,
                                                      ice_cover_binary_min=f43,
                                                      ice_cover_binary_max=f44,
                                                      ice_cover_binary_mean=f45,
                                                      ice_cover_binary_median=f46,
                                                      ice_cover_count=f47,
                                                      snow_km2=f48) %>%
  select(year, month, hylak_id, centr_lat, centr_lon, elevation, depth_avg, mean_temp_k, mean_sw_wm2)%>%
  mutate(mean_temp_c = mean_temp_k - 273.15,
         horizontal_radiance = mean_sw_wm2 * 730 / 1000 / 30) %>%
  group_by(hylak_id, year, centr_lat, centr_lon, elevation, depth_avg) %>%
  summarize(m_ice = length(year[mean_temp_c<0]),
            annual_temp_c = mean(mean_temp_c),
            annual_temp_c_sd = sd(mean_temp_c),
            annual_horizontal_radiance = mean(horizontal_radiance),
            annual_horizontal_radiance_sd = sd(horizontal_radiance))%>%
  st_as_sf(coords = c("centr_lon", "centr_lat"), crs = 4326) %>%
  #projecting
  st_transform("+proj=eqearth +wktext")

#### Bringing in GeoDAR data ####
dam_data <- vroom::vroom("./GeoDAR_v11_dams_beta_pr1.csv", delim = ",", col_names = T) %>%
  #selecting lat lon
  select(longitude, latitude) %>%
  #making spatial in lat lon
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  #projecting
  st_transform("+proj=eqearth +wktext")

#### Pairing reservoirs to our lakes ####
dam_glcp_link <- l %>%
  #binding the reservoirs to our lakes (d2) with the nearest geometry 
  cbind(dam_data[st_nearest_feature(l, dam_data),]) %>%
  #calculating distance between the nearest reservoir and the nearest lake
  mutate(dist = st_distance(geometry, geometry.1, by_element = T))

#cleaning data for subsequent actions
# units are in meters --> Removing the units so we can work with the column in the left_join function
dam_glcp_link$dist <- drop_units(dam_glcp_link$dist)

#removing the spatial object so it is just a data frame
dam_glcp_link2 <- dam_glcp_link %>% 
  st_drop_geometry() %>%
  select(-geometry.1) %>% unique() %>%
  mutate(water_body_type = ifelse(dist <= 5000, "RESERVOIR", "LAKE")) %>%
  select(-dist)

ebullition_model <- function(Zmax, Zmean, I, M_ice){
  10^(-1.310432 + 0.8515131 * log10((1-(1-3/(Zmax))^((Zmax)/(Zmean )- 1))*100) + 0.051977 * (I * M_ice)) * (16.55*(16/12*34*365/1000))
}

ebullition_model(Zmax = 40,
                 Zmean = 20,
                 I = 3.1,
                 M_ice = 2)

