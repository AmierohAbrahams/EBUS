# Upwelling index and its thresholds

### Use the upwelling classification technique used in chapter 2
# Determine the upwelling index

upwelling <- UI_angle %>%
  dplyr::rename(temp = ui.saws) %>%
  group_by(site) %>%
  # mutate(min_t = min(t),
  #        max_t = max(t)) %>%
  nest() %>% # apply the following functions to all of the variables in te dataset
  mutate(clim = purrr::map(data, ts2clm, climatologyPeriod = c("1997-01-01", "2015-12-31")), # creating a column of climatologies. Column will be named clim
         # NB: A threshold of 3 appeared to be far to strict
         # purr::map - apllies a function to each element of a vector
         exceed = purrr::map(clim, exceedance, minDuration = 1, threshold = 1)) %>%  #Upwelling cannot be descrbed as an event. Upwelling can last for a few hours. Given that we have daily data, upwelling events minimum duration here will be 1day
  # Detect consecutive days in exceedance of a given threshold.
  # mutate() %>%
  select(-data, -clim) %>%
  unnest() %>%
  filter(row_number() %% 2 == 1) %>%
  unnest() %>% # creates a column for each variables
  dplyr::rename(ui.saws = temp) %>% # rename upwelling index vale to temp so that it could work with the function
  select(site, t, ui.saws, exceedance)



### Temperature and wind

detect_event_custom <- function(df){
  res <- detect_event(df, threshClim2 = df$exceedance, minDuration = 1, coldSpells = T)$event
  return(res)
}
# 
ts2clm_custom <- function(df){
  # The climatology base period used here is up for debate...
  # The choice of the 25th percentile threshold also needs to be justified and sensitivty tested
  res <- ts2clm(df, pctile = 25, climatologyPeriod = c("1992-01-01", "2016-12-31"))
  return(res)
}
# 
# Calculate the upwelling event metrics
upwelling_detect_event <- function(df){
  upwell_base <- df %>%
    dplyr::rename(t = date) %>%
    group_by(site, product, heading, distance, lon, lat) %>%
    group_modify(~ts2clm_custom(.x)) %>%
    left_join(upwelling, by = c("site", "t")) %>%
    filter(!is.na(exceedance)) %>%
    group_by(site, product, heading, distance, lon, lat) %>%
    group_modify(~detect_event_custom(.x))
}
# 
OISST_upwell_base <- upwelling_detect_event(df = OISST_fill)
save(OISST_upwell_base, file = "Data/OISST_upwell_base.RData")
