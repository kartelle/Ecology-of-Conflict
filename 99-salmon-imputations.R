#############
# Script to impute salmon biomass for 'Ecology of conflict:
# marine food supply affects human-wildlife interactions on land'.
# R code written by Sean Anderson, 2015
#
# Creates the salmon_biomass and salmon_biomass_geo_mean
# fields in conflict_data_GBPU.csv. Exact values might differ slightly from
# iteration to iteration.
#############

library(dplyr)
library(ggplot2)

d<-read.csv("data/biomass_summed_by_year.csv")
d<-dplyr::rename(d, STREAM_ID=GEOLOC_ID, Biomass=biomass)
stopifnot(identical(sum(is.na(d$Biomass)), 0L))

min_year <- 1983 # as determined by other data range
n_years <- max(d$year) - min_year + 1

biggest_gap <- function(x){
  suppressWarnings(max(diff(x)) - 1)
}

## filter at species level:
q <- d %>%
  filter(year >= 1983) %>%
  group_by(GBPU, SPECIES, STREAM_ID) %>%
  mutate(max_biomass = max(Biomass), n_na = n_years - length(Biomass)) %>%
  as.data.frame %>% group_by(GBPU, SPECIES, STREAM_ID) %>%
  arrange(year) %>%
  mutate(max_gap = biggest_gap(year)) %>%
  as.data.frame %>%
  filter(n_na <= 8, max_gap <= 3)

# now impute:
library(rstan)
stan_ricker_missing <- stan_model("ricker-logistic-impute.stan")
stan_ricker_missing1 <- stan_model("ricker-logistic-impute1.stan")

impute_ricker <- function(y, id, iter = 1e4, thin = 1) {
  scale_factor <- 1
  y <- y / scale_factor

  missing_id <- which(is.na(y))
  y_missing <- y
  y_missing[missing_id] <- 9999 # Stan can't handle NAs; give it fake data

  if (length(missing_id) > 1) {
    m_missing <- sampling(
      stan_ricker_missing,
      data = list(N = length(y), y = log(y_missing),
                  K_upper = max(y_missing[-missing_id]) * 1.25,
                  r_upper = 5,
                  Nmissing = length(missing_id),
                  missing_id = missing_id),
      iter = iter, thin = thin,
      pars = c("K", "r", "sigma_proc", "y_impute"))
  }

  if (length(missing_id) == 1) {
    m_missing <- sampling(
      stan_ricker_missing1,
      data = list(N = length(y), y = log(y_missing),
                  K_upper = max(y_missing[-missing_id]) * 1.25,
                  r_upper = 5,
                  missing_id = missing_id),
      iter = iter, thin = thin,
      pars = c("K", "r", "sigma_proc", "y_impute"))
  }

  max_rhat <- max(summary(m_missing)$summary[c(1:3, missing_id + 3), "Rhat"])
  min_neff <- min(summary(m_missing)$summary[c(1:3, missing_id + 3), "n_eff"])

  png(paste0("traceplots/trace-", id, ".png"))
  traceplot(m_missing, pars = c("K", "r", "sigma_proc", "lp__"), inc_warmup = FALSE)
  dev.off()

  imp_median <- exp(apply(extract(m_missing)$y_impute, 2, median))
  imp_draw_ids <- sample(seq_len(1200), 8)
  imp_draws <- exp(as.data.frame(t(extract(
    m_missing)$y_impute[imp_draw_ids, ])))
  names(imp_draws) <- paste0("sb_imp_", 1:8)
  data.frame(imp_draws * scale_factor, sb_imp_median = imp_median * scale_factor,
             max_rhat, min_neff)
}

dir.create("traceplots", showWarnings = FALSE)
run_imputation <- function(dat, iter, thin) {
  plyr::ddply(dat, c("GBPU", "SPECIES", "STREAM_ID"), function(x) {
                xx <- left_join(data.frame(year = min_year:max(q$year),
                    SPECIES = x$SPECIES[1],STREAM_ID = x$STREAM_ID[1],
                    WSG_CODE = x$WSG_CODE[1], GBPU = x$GBPU[1]), x) %>%
                  dplyr::arrange(year)
                message(unique(xx$STREAM_ID))
                if (x$n_na[1] > 0) {
                  imputed <- impute_ricker(xx$Biomass, id = xx$STREAM_ID[1],
                                           iter = iter, thin = thin)
                } else {
                  imputed <- data.frame(xx$Biomass, xx$Biomass, xx$Biomass, xx$Biomass,
                                        xx$Biomass, xx$Biomass, x$Biomass, x$Biomass,
                                        x$Biomass, NA, NA)
                  names(imputed) <- c(paste0("sb_imp_", 1:8),
                                      "sb_imp_median", "max_rhat", "min_neff")
                }
                data.frame(xx, imputed)
              })
}
out <- run_imputation(dat = q, iter = 1e4, thin = 1)
saveRDS(out, file = "data/imputations-species.rds")

# try again for those that didn't converge:
aa <-filter(out, max_rhat > 1.05 | min_neff < 100)
unique(aa$STREAM_ID)
out2 <- run_imputation(dat = filter(q, STREAM_ID %in% unique(aa$STREAM_ID)),
                       iter = 2e5, thin = 5)
saveRDS(out2, file = "data/imputations-species2.rds")

bb <-filter(out2, max_rhat > 1.05 | min_neff < 100)
unique(bb$STREAM_ID)
# try again for those that didn't converge:
out3 <- run_imputation(dat = filter(q, STREAM_ID %in% unique(bb$STREAM_ID)),
                       iter = 1e6, thin = 10)
saveRDS(out3, file = "data/imputations-species3.rds")

#COMBINE IMPUTATION RUNS

imp <- readRDS("data/imputations-species.rds")
imp2 <- readRDS("data/imputations-species2.rds")
imp3 <- readRDS("data/imputations-species3.rds")

imp$species_stream<-paste(imp$SPECIES, imp$STREAM_ID)
imp2$species_stream<-paste(imp2$SPECIES, imp2$STREAM_ID)
imp3$species_stream<-paste(imp3$SPECIES, imp3$STREAM_ID)

imp <- filter(imp, !species_stream %in% unique(imp2$species_stream))
imp2 <- filter(imp2, !species_stream %in% unique(imp3$species_stream))
imp <- bind_rows(imp, imp2)
imp <- bind_rows(imp, imp3) %>% as.data.frame()

## fix the filtering:
min_year <- 1983 # as determined by other data range
n_years <- max(imp$year) - min_year + 1
gap_at_end <- function(x){
  x <- rev(x)
  min(which(!is.na(x))) - 1
}
gap_at_beginning <- function(x){
  min(which(!is.na(x))) - 1
}


imp <- imp %>%
  group_by(GBPU, SPECIES, STREAM_ID) %>%
  arrange(year) %>%
  mutate(gap_at_beginning = gap_at_beginning(Biomass),
         gap_at_end = gap_at_end(Biomass),
         max_gap = max(na.omit(max_gap))) %>%
  as.data.frame %>%
  filter(gap_at_end <= 3, gap_at_beginning <= 3, max_gap<=3)

stopifnot(identical(nrow(filter(imp, max_rhat > 1.05 | min_neff < 100)), 0L))

salm_biomass <- imp %>% group_by(GBPU, year) %>%
  summarise(salmon_biomass = sum(sb_imp_median),
            salmon_biomass_geo_mean = exp(mean(log(sb_imp_median))))

saveRDS(salm_biomass, file = "data/salm_biomass.rds")
