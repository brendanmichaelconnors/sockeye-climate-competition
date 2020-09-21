## calculate estimated effects of hatchery pink production ----------------------------------------

# Load competitor indices
raw_comp <- read.csv(file = "data/competitor_indices_2019_09_26.csv", header = TRUE)

# Standard deviation unit (SDU) for total pink abundance for full time series
sd_total_pinks <- sd(raw_comp$pink_numbers_np[raw_comp$Year > 1975])

# Average hatchery mean production in recent years as proportion of total pink SDU 
sdu_perc_hatch_pink <-mean(scale(raw_comp$pink_numbers_np_hatch[raw_comp$Year > 1975], scale = sd_total_pinks)[30:40,1])

# Average total pink production in recent years in SDUs 
sdu_perc_pink <- mean(scale(raw_comp$pink_numbers_np[raw_comp$Year > 1975])[30:40,1])

# Average wild pink production in recent years in SDUs
sdu_perc_wild_pink <- sdu_perc_pink-sdu_perc_hatch_pink

# Pink coefficient from model (hypermean)
wc_pink_coef <- summary(hb07r2, pars = "mu_kappa")$summary[1, "mean"]
goa_pink_coef <- summary(hb07r2, pars = "mu_kappa")$summary[2, "mean"]
bs_pink_coef <- summary(hb07r2, pars = "mu_kappa")$summary[3, "mean"]

# SST coefficient from model (hypermean)
wc_sst_coef <- summary(hb07r2, pars = "mu_gamma")$summary[1, "mean"]
goa_sst_coef <- summary(hb07r2, pars = "mu_gamma")$summary[2, "mean"]
bs_sst_coef <- summary(hb07r2, pars = "mu_gamma")$summary[3, "mean"]

# SST x pink coefficient from model (hypermean)
wc_sstxpink_coef <- summary(hb07r2, pars = "mu_chi")$summary[1, "mean"]
goa_sstxpink_coef <- summary(hb07r2, pars = "mu_chi")$summary[2, "mean"]
bs_sstxpink_coef <- summary(hb07r2, pars = "mu_chi")$summary[3, "mean"]

# Average SST anomaly over last 10 years of time series (2005-2015); mean by stock and then mean among stocks within same ocean region
stock_info <- read.csv( "./data/master_stock_info.csv")
raw.clim <- read.csv(file="data/sst_yr_1_stock_anomalies.csv",header=TRUE)
stock_raw_sst <- merge(raw.clim,stock_info, by= "Stock.ID")

region_sst_anomalies <- stock_raw_sst %>%
  group_by(Ocean.Region,Stock)%>%
  filter(Year > 2004 & Year < 2016)%>%
  summarise(mean_SST_SDUs = mean(sst_anomaly))%>%
  group_by(Ocean.Region)%>%
  summarise(mean_SST_SDUs = mean(mean_SST_SDUs))
  
sdu_WC_sst <- region_sst_anomalies[3,2]
sdu_GoA_sst <- region_sst_anomalies[2,2]
sdu_BS_sst <- region_sst_anomalies[1,2]

# Recent hatch pink "effect" given recent SST (need to account for bc can modify pink effect via interaction term)
hatch_pink_perc_wc <- (sdu_perc_hatch_pink * wc_pink_coef)  + (sdu_WC_sst*sdu_perc_hatch_pink*wc_sstxpink_coef)
hatch_pink_perc_goa <- (sdu_perc_hatch_pink * goa_pink_coef) + (sdu_GoA_sst*sdu_perc_hatch_pink*goa_sstxpink_coef)
hatch_pink_perc_bs <- (sdu_perc_hatch_pink * bs_pink_coef) + (sdu_BS_sst*sdu_perc_hatch_pink*bs_sstxpink_coef)

round((exp(hatch_pink_perc_wc) - 1) * 100,digits=1)
round((exp(hatch_pink_perc_goa) - 1) * 100,digits=1)
round((exp(hatch_pink_perc_bs) - 1) * 100,digits=1)

# % change in productivty given recent total pinks and SST
total_pink_perc_wc_combined <- (sdu_perc_pink * wc_pink_coef) + (sdu_WC_sst*wc_sst_coef) + (sdu_WC_sst*sdu_perc_pink*wc_sstxpink_coef)
total_pink_perc_goa_combined <- (sdu_perc_pink * goa_pink_coef) + (sdu_GoA_sst*goa_sst_coef) + (sdu_GoA_sst*sdu_perc_pink*goa_sstxpink_coef)
total_pink_perc_bs_combined <- (sdu_perc_pink * bs_pink_coef) + (sdu_BS_sst*bs_sst_coef) + (sdu_BS_sst*sdu_perc_pink*bs_sstxpink_coef)

round((exp(total_pink_perc_wc_combined) - 1) * 100,digits=1)
round((exp(total_pink_perc_goa_combined) - 1) * 100,digits=1)
round((exp(total_pink_perc_bs_combined) - 1) * 100,digits=1)

# % change in productivty given recent wild pinks and SST
wild_pink_perc_wc_combined <- (sdu_perc_wild_pink * wc_pink_coef) + (sdu_WC_sst*wc_sst_coef) + (sdu_WC_sst*sdu_perc_wild_pink*wc_sstxpink_coef)
wild_pink_perc_goa_combined <- (sdu_perc_wild_pink * goa_pink_coef) + (sdu_GoA_sst*goa_sst_coef) + (sdu_GoA_sst*sdu_perc_wild_pink*goa_sstxpink_coef)
wild_pink_perc_bs_combined <- (sdu_perc_wild_pink * bs_pink_coef) + (sdu_BS_sst*bs_sst_coef) + (sdu_BS_sst*sdu_perc_wild_pink*bs_sstxpink_coef)

round((exp(wild_pink_perc_wc_combined) - 1) * 100,digits=1)
round((exp(wild_pink_perc_goa_combined) - 1) * 100,digits=1)
round((exp(wild_pink_perc_bs_combined) - 1) * 100,digits=1)
