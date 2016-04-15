#############
# Script to model relationship between grizzly bear-human conflict 
# in 'Ecology of conflict:  
# marine food supply affects human-wildlife interactions on land'.
# R code written by Kyle Artelle and Sean Anderson, 2015
#############

library("dplyr")
library("ggplot2")
library("glmmADMB")
library("MuMIn")

conflict_salmon <- readRDS("data/conflict-salmon-for-modelling.rds")
conflict_all <- readRDS("data/conflict-all-for-modelling.rds")

#function to run glmmADMB models for 'salmon areas' and 'full region' models
run_glmmadmb_salmon<-function(x){
  model<-glmmadmb(x,
                  random = ~ (1 | gbpu),
                  data = conflict_salmon,
                  verbose = TRUE,
                  family = "nbinom")
  model
}
run_glmmadmb_all<-function(x){
  model<-glmmadmb(x,
                  random = ~ (1 | gbpu),
                  data = conflict_all,
                  verbose = TRUE,
                  family = "nbinom")
  model
}

#candidate model list for 'salmon-areas' model
m<-list(late_kills ~ salmon_biomass_geo_mean+prev_3yr_scaled+ prev_3yr_conflict_scaled+  mean_annual_temp_scaled +log_annual_precip_scaled +mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m, late_kills ~ salmon_biomass_geo_mean+prev_3yr_scaled+ mean_annual_temp_scaled +log_annual_precip_scaled +mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ salmon_biomass_geo_mean+mean_annual_temp_scaled +log_annual_precip_scaled +mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ salmon_biomass_geo_mean+mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ salmon_biomass_geo_mean+prev_3yr_scaled+ prev_3yr_conflict_scaled+mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ salmon_biomass_geo_mean+prev_3yr_conflict_scaled+mean_annual_temp_scaled +log_annual_precip_scaled+mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ salmon_biomass_geo_mean+prev_3yr_scaled+mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ salmon_biomass_geo_mean+prev_3yr_conflict_scaled+mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ mean_annual_temp_scaled +log_annual_precip_scaled +mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_scaled+ prev_3yr_conflict_scaled+  mean_annual_temp_scaled +log_annual_precip_scaled +mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_scaled+mean_annual_temp_scaled +log_annual_precip_scaled +mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_conflict_scaled+  mean_annual_temp_scaled +log_annual_precip_scaled +mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_scaled+mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_scaled+prev_3yr_conflict_scaled+mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_conflict_scaled+mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +offset(log(gbpu_usable_area)))

#run 'salmon areas' model, and save model objects
salmon_model_objects<-lapply(m, function(x) run_glmmadmb_salmon(x))
save(salmon_model_objects, file="data/salmon_model_objects.rda")

#candidate model list for 'full region' model
m<-list(late_kills ~ mean_annual_temp_scaled +log_annual_precip_scaled +mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +salmon_estimated+ offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_scaled+ prev_3yr_conflict_scaled+  mean_annual_temp_scaled +log_annual_precip_scaled +mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +salmon_estimated+offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_scaled+mean_annual_temp_scaled +log_annual_precip_scaled +mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +salmon_estimated+offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_conflict_scaled+  mean_annual_temp_scaled +log_annual_precip_scaled +mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +salmon_estimated+offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_scaled+mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +salmon_estimated+offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_scaled+prev_3yr_conflict_scaled+mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +salmon_estimated+offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ prev_3yr_conflict_scaled+mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +salmon_estimated+offset(log(gbpu_usable_area)))
m<-c(m,late_kills ~ mean_temp_scaled +total_log_precip_scaled +year_centered +log_humanpop_scaled +log_grizzly_pop_est_scaled +salmon_estimated+offset(log(gbpu_usable_area)))

#run 'full region' model, and save model objects
all_model_objects<-lapply(m, function(x) run_glmmadmb_all(x))
save(all_model_objects, file="data/all_model_objects.rda")

#create AIC output tables
salmon_selection_output<-model.sel(salmon_model_objects)
write.csv(salmon_selection_output, "figs/salmon_selection_output.csv")
all_selection_output<-model.sel(all_model_objects)
write.csv(all_selection_output, "figs/all_selection_output.csv")
