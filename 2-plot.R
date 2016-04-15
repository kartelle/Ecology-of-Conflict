#############
# Script to plot figures for 'Ecology of conflict:
# marine food supply affects human-wildlife interactions on land'.
# R code written by Kyle Artelle and Sean Anderson, 2015
#############

library("dplyr")
library("glmmADMB")
library("MuMIn")
library("ggplot2")

load("data/salmon_model_objects.rda") #output from "model conflict.R"
load("data/all_model_objects.rda") #output from "model conflict.R"
attacks<-read.csv("data/all attacks may 2015.csv")
grizzly_attacks<-readRDS("data/all_grizzly_attacks.rds") #all known grizzly bear attacks on humans, 1960-2014
all_yearly_ac_kills_for_plotting<-readRDS("data/all_yearly_ac_kills_for_plotting.rds")
ac_monthly<-readRDS("data/ac_monthly.rds")
conflict_salmon <- readRDS("data/conflict-salmon-for-modelling.rds") #output from prepare data.R
conflict <- read.csv("data/conflict_data_GBPU.csv", stringsAsFactors = FALSE) #variables summarized at GBPU spatial scale
conflict$X <- NULL #remove row number
CID_kill_distances<-read.csv("data/CID_kill_distances_from_towns.csv", stringsAsFactors=FALSE)
yearly_huntKills<-readRDS("data/yearly_huntKills.rds")#all hunter killed grizzlies, as modeled
ACKillsGBPU<-readRDS("data/ACKillsGBPU.rds")

#############################
# Separate salmon vs. non-salmon GBPUs
#############################
salmon_gbpus<-conflict%>%
  dplyr::select(GBPU, salmon_estimated)%>%
  dplyr::group_by(GBPU)%>%
  dplyr::summarize(salmon=salmon_estimated[1])
#############################

#############################
# Fig 1:  Yearly number of conflict kills and attacks
#############################
attack_years<-seq(1960, 2014, 1)
severe_grizz_attacks<-subset(grizzly_attacks, Severity=="Severe")
yearly_attacks<-plyr::ddply(severe_grizz_attacks, c("Year"), summarize, total_severe_attacks=length(Month))

timeplot_ACKillsGBPU<-merge(all_yearly_ac_kills_for_plotting, salmon_gbpus, by="GBPU", all=TRUE)
timeplot_ACKillsGBPU$kills[is.na(timeplot_ACKillsGBPU$kills)]<-0
timeplot_ACKillsGBPU<-subset(timeplot_ACKillsGBPU, Year>1979)
timeplot_ACKillstotal<-plyr::ddply(timeplot_ACKillsGBPU, c("Year", "salmon"), summarize, yearkills=sum(kills))

year_df<-plyr::rename(data.frame(attack_years), c(attack_years="year"))
yearly_attacks<-merge(yearly_attacks, year_df, by.x="Year", by.y="year", all=T)
yearly_attacks$total_severe_attacks[is.na(yearly_attacks$total_severe_attacks)]<-0


pdf("figs/fig 1.pdf", width=9, height=8)
par(mfrow = c(2, 1))
par(las = 1, mar = c(3, 3, 3.5, 2), cex=1.4, cex.main=1, oma = c(1, 1, 1, 1),
    mgp = c(2, 0.5, 0), tck = -0.025, col.axis = "grey40")
plot(yearly_attacks$Year, yearly_attacks$total_severe_attacks, xlab="Year", ylab="Number of severe attacks", main="A) Severe attacks on humans", type='l',  bty="l", lwd=2)
points(yearly_attacks$Year, yearly_attacks$total_severe_attacks, pch=16)
plot(subset(timeplot_ACKillstotal, !salmon)$Year, subset(timeplot_ACKillstotal, !salmon)$yearkills, type='l', bty="l", lty=6, lwd=2, xlab="Year", col="grey50", ylab="Number of conflict-killed bears", main="B) Conflict-killed bears")
points(subset(timeplot_ACKillstotal, !salmon)$Year, subset(timeplot_ACKillstotal, !salmon)$yearkills, pch=16, col="grey50")
lines(subset(timeplot_ACKillstotal, salmon)$Year, subset(timeplot_ACKillstotal, salmon)$yearkills, type='l', lwd=2)
points(subset(timeplot_ACKillstotal, salmon)$Year, subset(timeplot_ACKillstotal, salmon)$yearkills, pch=16)
text(1980, 45, "Areas with salmon", adj=c(0,0), font=2)
text(1980, 38, "Areas without salmon", col="grey50", font=2, adj=c(0,0))
dev.off()

#############################

#############################
# Fig 2:  Timing timing of attacks/conflict kills by month, with label right under bar instead of beside it
# Also report summary statistics
#############################
ac_kills_hist<-merge(ac_monthly, salmon_gbpus, by="GBPU",  all=TRUE)

#summary stats of number of attacks/conflict kills
paste("total conflict kills:", nrow(filter(ac_kills_hist, Year>1979)))
paste("late year kills:", nrow(filter(ac_kills_hist, month>6, Year>1979)))
paste("total conflict kills in salmon areas:", nrow(filter(ac_kills_hist, Year>1979 &  salmon & STATUS !="Extirpated" & STATUS !="Vancouver Island")))
paste("total late year conflict kills in salmon areas:", nrow(filter(ac_kills_hist, month>6 & Year>1979  & salmon & STATUS !="Extirpated" & STATUS !="Vancouver Island")))
paste("total conflict kills in non-salmon areas:", nrow(filter(ac_kills_hist, Year>1979 &  !salmon & STATUS !="Extirpated" & STATUS !="Vancouver Island")))
paste("total late year conflict kills in non-salmon areas:", nrow(filter(ac_kills_hist, month>6 & Year>1979 & !salmon & STATUS !="Extirpated" & STATUS !="Vancouver Island")))


plot_month_hist<-function(x, name, main_text, add_salmon_line=FALSE, export=TRUE){
  if (export) png(paste("figs/", name, ".png"),  width=552, height=424)
  p<-hist(x, breaks=seq(0, 12, 1),col="grey50", border="white",  main=main_text, ylab="", xlab="Month", xaxt="n")
  box(bty='L')
  axis(side=1, at=seq(0.5,10.5,2), labels=seq(1,11,2))  #trick so that all labels show (otherwise R kicks out November)
  axis(side=1, at=seq(1.5,11.5,2), labels=seq(2,12,2))
  if (add_salmon_line) abline(v=6, col="red", lwd=2)
  if (export) dev.off()
}

tiff("figs/fig 2.tif", width=720, height=640)
par(mfrow = c(2, 2))
par(las = 1, mar = c(2.6, 2, 3, 2), cex=1.4, cex.main=1, oma = c(1, 1, 1, 1),
    mgp = c(1.7, 0.5, 0), tck = -0.025, col.axis = "grey40")
plot_month_hist(subset(attacks, Severity=="Severe" & Species=="Grizzly")$Month, "severe attacks by month", "A) Severe attacks on humans, all areas", export=FALSE)
plot_month_hist(as.numeric(substr(ac_kills_hist$Kill_dt, 5, 6)), "conflict kills all areas", "B) Conflict-killed bears, all areas", export=FALSE)
plot_month_hist(as.numeric(substr(subset(ac_kills_hist, salmon)$Kill_dt, 5, 6)), "conflict kills salmon areas", "C) Conflict-killed bears, salmon areas", add_salmon_line=TRUE, export=FALSE)
plot_month_hist(as.numeric(substr(subset(ac_kills_hist,  !salmon)$Kill_dt, 5, 6)), "conflict kills non-salmon areas", "D) Conflict-killed bears, non-salmon areas", export=FALSE)

dev.off()
#############################

#############
# Fig 4:  model coefficients for 'full region' and 'salmon areas' models,
# and gbpu-level coefficient estimates for effect of salmon availability on
# conflict in
#############

all_selection_output<-model.sel(all_model_objects)
avg_all_model<-model.avg(all_selection_output)
all_variable_importance<-data.frame(importance(all_selection_output))
all_variable_importance$effect = rownames(all_variable_importance)
rownames(all_variable_importance)=NULL
all_variable_importance$effect[all_variable_importance$effect == "salmon_estimated"] <-
  "salmon_estimatedTRUE"

salmon_selection_output<-model.sel(salmon_model_objects)
avg_salmon_model<-model.avg(salmon_selection_output)
salmon_variable_importance<-data.frame(importance(salmon_selection_output))
salmon_variable_importance$effect = rownames(salmon_variable_importance)
rownames(salmon_variable_importance)=NULL

#############
# Combined coefficient plot for spatial and annual variables
#############

#function to extract coefficient estimates and confidence intervals
get_cis <- function(x) {
  fe <- coef(x)
  ci <- confint(x, level = .95)
  ci2 <- confint(x, level = .5)
  d <- data.frame(effect = names(fe), fe = as.numeric(fe), l = ci[,1],
                  ll = ci2[,1], uu = ci2[,2], u = ci[,2], stringsAsFactors = FALSE)
  rownames(d) <- NULL
  d <- dplyr::filter(d, effect != "(Intercept)")
  d
}

#extract label names from external CSV (set manually)
labs <- read.csv("data/coef-names.csv", stringsAsFactors = FALSE, strip.white = TRUE)

#combine data to be plotted for each coefficient (label, importance, coefficient estimate, confidence intervals)
salm_coef <- get_cis(avg_salmon_model) %>%
  inner_join(labs) %>%
  inner_join(salmon_variable_importance) %>%
  rename(imp = importance.salmon_selection_output.) %>%
  filter(effect != "year_centered") %>%
  mutate(model = "salmon")
all_coef <- get_cis(avg_all_model) %>%
  inner_join(labs) %>%
  inner_join(all_variable_importance) %>%
  rename(imp = importance.all_selection_output.) %>%
  filter(effect != "year_centered") %>%
  mutate(model = "all")
coefs <- bind_rows(salm_coef, all_coef) %>%
  mutate(imp = sprintf("%.2f", round(imp, 2))) %>%
  arrange(model, type, imp) %>%
  as.data.frame()

#order spatial variables by coefficient value
c_order_sp <- filter(coefs, type == "spatial") %>%
  group_by(type, label) %>%
  summarise(mean_fe = mean(fe), mean_abs_eff = abs(mean(fe))) %>%
  arrange(mean_fe, mean_abs_eff) %>%
  mutate(order = 1:length(mean_fe))

#order annual variables by relative variable importance
c_order_an <- filter(coefs, type == "annual") %>%
  group_by(type, label) %>%
  summarise(salm_imp = as.numeric(imp[model == "salmon"]), mean_abs_eff = abs(mean(fe))) %>%
  arrange(salm_imp, mean_abs_eff) %>%
  mutate(order = 1:length(salm_imp))

#combine annual and spatial orderings
c_order <- bind_rows(c_order_an, c_order_sp)

#add order to coefficients to be plotted
coefs <- inner_join(coefs, c_order) %>%
  mutate(pos = order)

#set colours and vertical positions for salmon vs annual plotting
coefs <- mutate(coefs,
                pos = ifelse(model == "all", order, order + 0.25),
                bg = ifelse(model == "all", "white", "red"),
                imp_col = ifelse(model == "all", "grey10", "red"),
                imp_pos = ifelse(model == "all", pos - 0.1, pos + 0.1),
                show_lab = ifelse(model == "all", TRUE, FALSE)
)

#function to create individual panel (either spatial or annual)
make_panel <- function(dat, xlim = c(-1.3, 1.3), ticks = c(0.2, 0.5, 1, 2, 5),
                       add_importance = FALSE, label = "") {

  plot(1, 1, xlim = xlim, ylim = c(0.8, 5.4), xlab = "", ylab = "", type = "n",
       axes = FALSE)
  par(xpd = FALSE)
  abline(v = 0, col = "grey40", lty = "22")
  with(dat, segments(l, pos, u, pos, lwd = 0.5))
  with(dat, segments(ll, pos, uu, pos, lwd = 1.7))
  with(dat, points(fe, pos, pch = 21, bg = bg, col = "grey20"))
  axis(1, at = log(ticks), labels = ticks, col = "grey40")
  par(xpd = NA)
  text_offset <- 2.65
  with(filter(dat, show_lab), text(rep(xlim[1] * text_offset, length(label)),
                                   pos + 0.1, label, pos = 4, col = "grey40"))
  # quick ugly kludge to get salmon biomass label on:
  if (filter(dat, show_lab) %>% nrow() <= 4)
    text(xlim[1] * text_offset, 5, dat$label[length(dat$label)], pos = 4,
         col = "grey40")
  box(col = "grey40")

  # add importance:
  if (add_importance)
    with(dat, text(xlim[2] * 0.85, imp_pos, imp, col = imp_col, cex = 0.8))

  text(xlim[1] * text_offset, 6, label, pos = 4, col = "grey10")
}

#create overall figure
tiff("figs/fig 4.tif", width = 2.7, height = 3.2, units="in", res=300)

par(mfrow = c(2, 1))
par(las = 1, cex = 0.7, mar = c(2, 0, 0, 0), oma = c(2, 8, 1.5, .5),
    mgp = c(2, 0.25, 0), tck = -0.025, col.axis = "grey40")

make_panel(filter(coefs, type == "spatial"), xlim = c(-3, 3),
           ticks = c(0.1, 0.3, 1, 3, 10), label = "A (Spatial)")

make_panel(filter(coefs, type == "annual"), add_importance = TRUE,
           label = "B (Annual)")

mtext("Effect on number of conflict-killed grizzly bears", side = 1,
      line = -0.5, cex = 0.7, col = "grey20", outer = TRUE, adj = 1.0)
mtext("(per 2 SDs of predictor)", side = 1, line = 0.5, cex = 0.7,
      col = "grey50", outer = TRUE, adj = -0.3)

dev.off()


#############
# Supplementary Fig 1:  Age and distance from nearest human habitation of
# hunter killed vs conflict killed bears
#############
CID_kill_distances<-plyr::rename(CID_kill_distances, c(Kill_cd="KILL_CODE"))
CID_kill_distances$Distance_km<-CID_kill_distances$Distance/1000

tiff("figs/supplementary_fig_1.tif", width = 1440, height = 480)
par(mfrow = c(1, 2))
hist(subset(CID_kill_distances,  Editd_g<30 & KILL_CODE==2)$Editd_g, xlim=c(0, 30), ylim=c(0, 1000),breaks=seq(0, 30, 1), col="black", border="grey", main="", xlab="Age (in years)",  cex.lab=1.5, cex.axis=1.5)
hist(subset(CID_kill_distances,  Editd_g<30 & KILL_CODE==1)$Editd_g, xlim=c(0, 30), ylim=c(0, 1000),breaks=seq(0, 30, 1), col=rgb(0.190, 0.190, 0.190, 0.5), border="white", add=TRUE)
text(30/1.25, 1000/1.25, "A", cex=2)
hist(subset(CID_kill_distances, KILL_CODE==2)$Distance_km,  xlim=c(0, 250), ylim=c(0, 800),breaks=seq(0, 300, 5), col="black", border="grey", main="", xlab="Distance (in km)", cex.lab=1.5, cex.axis=1.5)
hist(subset(CID_kill_distances, KILL_CODE==1)$Distance_km, xlim=c(0, 250), ylim=c(0, 800),breaks=seq(0, 300, 5), col=rgb(0.190, 0.190, 0.190, 0.5), border="white", add=TRUE)
text(250/1.25, 800/1.25, "B", cex=2)
dev.off()
#############


#############
# Supplementary Fig 2:  observed vs predicted for full models (not model-averaged)
#############
m_salmon1 <- glmmadmb(
  late_kills ~
    salmon_biomass_geo_mean+
    prev_3yr_scaled+
    prev_3yr_conflict_scaled+
    mean_annual_temp_scaled +
    log_annual_precip_scaled +
    mean_temp_scaled +
    total_log_precip_scaled +
    year_centered +
    log_humanpop_scaled +
    log_grizzly_pop_est_scaled +
    offset(log(gbpu_usable_area)),
  random = ~ (1 | gbpu),
  data = conflict_salmon,
  verbose = TRUE,
  family = "nbinom")
save(m_salmon1, file = "data/m_salmon1_rand.rda")
sink("data/m_salmon1_rand.txt")
summary(m_salmon1)
sink()

re1 <- data.frame(gbpu = row.names(ranef(m_salmon1)$gbpu), re = ranef(m_salmon1))
re1<-dplyr::rename(re1, re=X.Intercept.)
cs <- inner_join(conflict_salmon, re1)
cs$fe <- predict(m_salmon1)

p_v_o_salmon<-ggplot(cs, aes(exp(fe+re), jitter(late_kills, 1)))+
  geom_point(alpha = 0.4) + ylab("Observed conflict kills") + xlab("Predicted conflict kills") +xlim(0, 3.8)
ggsave('figs/supplementary_fig_2.png')
#############


#############
# Supplementary Fig 3: Effect of salmon on conflict plot by Grizzly Bear Population Unit
# coefficients are back-transformed by dividing by 2*SD of geometric mean
# of salmon availability within each spatial unit, divided again by
# 2*SD across all spatial units, because the salmon variable was centred
# both within and across spatial units
#############

gbpus_modeled<-unique(conflict_salmon$gbpu)#only include spatial units we modeled

m_salmon1<-avg_salmon_model

#import mean and standard deviation of salmon biomass (within spatial units); filter for areas we modeled
btr<-readRDS(file = "data/btr.rds")%>%
  filter(GBPU %in% gbpus_modeled)
#import sd of salmon biomass across spatial units
overall_sd<-readRDS("data/salmon_biomass_geo_mean_sd.rds")

pdf("figs/salmon coef across GBPUs for 2-fold increase.pdf")
par(mar = c(4, 12, 1, 1))
par(cex = 0.8)

# set the "fold" level of change to examine.  For example, scalar_value of 2
# calculates the predicted change in conflict for each 2-fold change in annual geometric
# mean of salmon biomass
scalar_value<-2
scalar <- log(scalar_value)

plot(1 - exp(scalar * (coef(m_salmon1)[[2]] / (2 * btr$sd_geo_mean))/(2*overall_sd)), 1:nrow(btr),
     xlim = c(-0.1, 1), yaxt = "n",
     xlab = ifelse(scalar_value==2, paste("Percent increase in conflict per 50% decrease in geometric mean biomass"),paste("Percent increase in conflict per", scalar_value, "fold decrease in geometric mean biomass")), #manually set to "50% decrease" instead of "2 fold"
     ylab = "", xaxs = "i")
#lower (l) and upper(u) bounds of confidence interval
l <- 1 - exp(scalar * (confint(m_salmon1)[2,1] / (2 * btr$sd_geo_mean))/(2*overall_sd))
u <- 1 - exp(scalar * (confint(m_salmon1)[2,2] / (2 * btr$sd_geo_mean))/(2*overall_sd))

#draw confidence intervals, set axes, include line through x=0
segments(l, 1:nrow(btr), u, 1:nrow(btr))
axis(2, at = 1:nrow(btr), labels = btr$GBPU, las = 1)
abline(v = 0, lty = 2)

# calculate, display, and plot the effect of salmon availability on conflict
# for a spatial unit with average variability of salmon
avg <- 1 - exp(scalar * (coef(m_salmon1)[[2]] / (2 * mean(btr$sd_geo_mean)))/(2*overall_sd))
u <- 1 - exp(scalar * (confint(m_salmon1)[2,1] / (2 * mean(btr$sd_geo_mean)))/(2*overall_sd))
l <- 1 - exp(scalar * (confint(m_salmon1)[2,2] / (2 * mean(btr$sd_geo_mean)))/(2*overall_sd))
abline(v = avg)
abline(v = l, col = "grey50")
abline(v = u, col = "grey50")
round(c(l, avg, u), 2)
dev.off()
#############


#############
# Supplementary Fig 5: Annual  number of conflict-killed and hunter-killed bears
# facetted by spatial unit (GBPU)
#############
yearly_huntKills<-readRDS("data/yearly_huntKills.RDS")
ggplot(yearly_huntKills[!(yearly_huntKills$GBPU==""),])+geom_point(aes(x=Year, y=total_hunts))+facet_wrap(~GBPU, scales="free_y")+ggtitle("A) Hunt kills\n")+ylab("Number of hunter-killed bears")
print(paste("mean annual hunt kills", mean(yearly_huntKills$hunts)))
print(paste("sd annual hunt kills", sd(yearly_huntKills$hunts)))
print(paste("min annual hunt kills", min(yearly_huntKills$hunts)))
print(paste("max annual hunt kills", max(yearly_huntKills$hunts)))

ggplot(ACKillsGBPU)+geom_point(aes(x=Year, y=TotalACKills))+facet_wrap(~GBPU, scales="free_y")+ggtitle("B) Conflict kills\n")+ylab("Number of conflict-killed bears")
print(paste("mean annual conflict kills", mean(ACKillsGBPU$TotalACKills)))
print(paste("sd annual conflict kills", sd(ACKillsGBPU$TotalACKills)))
print(paste("min annual conflict kills", min(ACKillsGBPU$TotalACKills)))
print(paste("max annual conflict kills", max(ACKillsGBPU$TotalACKills)))
##############################
