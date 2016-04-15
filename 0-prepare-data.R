#############
# Script to combine data sources used in 'Ecology of conflict: 
# marine food supply affects human-wildlife interactions on land'.
# R code written by Kyle Artelle and Sean Anderson, 2015
# Data provided at Grizzly Bear Population Scale, because some data  
# (human-caused kills of grizzly bears, and bear attacks on humans), are not publicly
# available, and datashare agreements with data owners preclude us from sharing in raw form.
#############

library("plyr")
library("dplyr")

conflict <- read.csv("data/conflict_data_GBPU.csv", stringsAsFactors = FALSE)
conflict$X <- NULL #remove row number

# clean up column names
names(conflict) <- tolower(names(conflict))
conflict <- dplyr::rename(conflict,
                          late_kills = lateackills,
                          total_kills = totalackills)
  
# remove blank gbpu rows, make gbpu a factor
conflict<-conflict%>%  filter(!gbpu %in% c("", " "))%>%
  mutate(gbpu=as.factor(gbpu))


# remove NA's
conflict_all<-conflict[,!names(conflict) %in% c("salmon_biomass_geo_mean", "salmon_biomass")]
na_vals <- plyr::colwise(is.na)(conflict_all)
conflict_all <- conflict_all[!apply(na_vals, 1, any), ]
conflict_salmon<-filter(conflict, salmon_estimated)
na_vals <- plyr::colwise(is.na)(conflict_salmon)
conflict_salmon <- conflict_salmon[!apply(na_vals, 1, any), ]

#centre year measures
conflict_all<-mutate(conflict_all, year_centered = year-mean(year))
conflict_salmon<-mutate(conflict_salmon, year_centered = year-mean(year))

# function to subtract mean and divide by 2 SDs:
scale2sd <- function(x) {
  x <- x - mean(x, na.rm = TRUE)
  x / (2 * sd(x, na.rm = TRUE))
}

#scale prev hunts, conflicts, and salmon to norm within each GBPU
conflict_all<- conflict_all %>%
  group_by(gbpu) %>%
  dplyr::mutate(prev_3yr_hunts = scale2sd(prev_3yr_hunts),
                prev_3yr_conflict = scale2sd(prev_3yr_conflict))%>%
  as.data.frame
conflict_salmon<- conflict_salmon %>%
  group_by(gbpu) %>%
  dplyr::mutate(prev_3yr_hunts = scale2sd(prev_3yr_hunts),
                prev_3yr_conflict = scale2sd(prev_3yr_conflict),
                salmon_biomass_geo_mean = scale2sd(log(salmon_biomass_geo_mean))
  )%>%
  as.data.frame

# calculate and save standard deviation of geometric mean of salmon biomass
# across spatial units, for plotting
salmon_biomass_geo_mean_sd<-sd(conflict_salmon$salmon_biomass_geo_mean) 
saveRDS(salmon_biomass_geo_mean_sd, file="data/salmon_biomass_geo_mean_sd.rds")


# Scale annual predictors 
conflict_salmon<- conflict_salmon %>%
  dplyr::mutate(
    salmon_biomass_geo_mean = scale2sd(salmon_biomass_geo_mean),
    mean_annual_temp_scaled = scale2sd((spring_mean_temp + summer_mean_temp)/2),
    log_annual_precip_scaled = scale2sd(log(spring_precip + summer_precip)),
    prev_3yr_scaled = scale2sd(prev_3yr_hunts),
    prev_3yr_conflict_scaled = scale2sd(prev_3yr_conflict),
    spring_precip_scaled=scale2sd(spring_precip),
    summer_precip_scaled=scale2sd(summer_precip),
    spring_temp_scaled=scale2sd(spring_mean_temp),
    summer_temp_scaled=scale2sd(summer_mean_temp)
  )
conflict_all <- conflict_all %>%
  dplyr::mutate(
    mean_annual_temp_scaled = scale2sd((spring_mean_temp + summer_mean_temp)/2),
    log_annual_precip_scaled = scale2sd(log(spring_precip + summer_precip)),
    prev_3yr_scaled = scale2sd(prev_3yr_hunts),
    prev_3yr_conflict_scaled = scale2sd(prev_3yr_conflict),
    spring_precip_scaled=scale2sd(spring_precip),
    summer_precip_scaled=scale2sd(summer_precip),
    spring_temp_scaled=scale2sd(spring_mean_temp),
    summer_temp_scaled=scale2sd(summer_mean_temp)
  )

# Create group-level (spatial) predictors:
gbpu_predictors_conflict<-function(df){
  df <-
    group_by(df, gbpu) %>%
    dplyr::summarise(
      mean_temp_scaled = mean(c(normals_mean_summer_temp[1], normals_mean_spring_temp[1])),
      total_log_precip_scaled = log(sum(c(normals_mean_spring_precip[1], normals_mean_summer_precip[1]))),
      log_humanpop_scaled = log(human_pop_2011[1]/gbpu_usable_area[1]),
      log_grizzly_pop_est_scaled = log(grizzly_pop_est[1]/gbpu_usable_area[1])
    )
  df
}
conflict_salmon_gbpu<-gbpu_predictors_conflict(conflict_salmon)
conflict_all_gbpu<-gbpu_predictors_conflict(conflict_all)

#scale group-level predictors
conflict_salmon_gbpu <- data.frame(
  dplyr::select(conflict_salmon_gbpu, gbpu),
  dplyr::select(conflict_salmon_gbpu, -gbpu) %>% apply(2, scale2sd)
)
conflict_all_gbpu <- data.frame(
  dplyr::select(conflict_all_gbpu, gbpu),
  dplyr::select(conflict_all_gbpu, -gbpu) %>% apply(2, scale2sd)
)

#combine spatial and annual predictors
conflict_salmon <- left_join(conflict_salmon, conflict_salmon_gbpu) %>% as.data.frame
conflict_all <- left_join(conflict_all, conflict_all_gbpu) %>% as.data.frame

# save for modelling:
saveRDS(conflict_salmon, file = "data/conflict-salmon-for-modelling.rds")
saveRDS(conflict_all, file = "data/conflict-all-for-modelling.rds")
