################################################################################
####### R scripts to create the stock specific covariates used in the analysis
################################################################################

#------------------------------------------------------------------------------#
#   Load necessary data
#------------------------------------------------------------------------------#

## Master brood table
bt.raw <- read.csv("data/master_brood_table.csv", header=T)
bt.complete <- bt.raw[complete.cases(bt.raw),]
head(bt.complete)
tail(bt.complete)
sapply(bt.complete, class)

## 1st year climate var
raw.clim <- read.csv(file="data/sst_yr_1_stock_anomalies.csv",header=TRUE)
head(raw.clim)

## 2nd year SST climate var
raw.clim.late <- read.csv(file="data/second_year_ocean_indices.csv",header=TRUE)
head(raw.clim.late)

## 2nd year MLD climate var
raw.clim.mld.late <- read.csv(file="data-downloaded/rob-second-year-indices-2018-02-16.csv",
                              header=TRUE)
head(raw.clim.mld.late)

## Pink competition
raw.comp <- read.csv(file="data/competitor_indices_2019_09_26.csv", header = TRUE)
head(raw.comp)

## Detrended: 1st year climate var
clim.detrend <- read.csv(file = "./data/sst_yr_1_stock_anomalies_detrend.csv", header = TRUE)

## Detrended: pink competition
comp.detrend <- read.csv(file = "./data/pink_abundance_detrend.csv", header = TRUE)



## Age weighted climate indices ----------------------------

## SST during early marine life
early.sst <- clim.wgt.avg(brood.table = bt.complete,
                          env.data = raw.clim,
                          env.covar = "sst_anomaly",
                          type = "first_year",
                          out.covar = "early_sst")

early.sst.lm <- clim.wgt.avg(brood.table = bt.complete,
                             env.data = clim.detrend,
                             env.covar = "sst_lm",
                             type = "first_year",
                             out.covar = "early_sst_lm")


## SST during 2nd year of marine life
late.sst <- clim.wgt.avg(brood.table = bt.complete,
                         env.data = raw.clim.late,
                         env.covar = "sst.winter.djfm",
                         type = "second_year",
                         out.covar = "late_sst")


## Winter MLD during 2nd year of marine life
mld.wi2.wc <- clim.wgt.avg(brood.table = bt.complete[bt.complete$Ocean.Region != "BS", ],
                           env.data = raw.clim.mld.late,
                           env.covar = "GoAmld_winter",
                           type = "second_year",
                           out.covar = "late_mld_winter")
mld.wi2.bs <- clim.wgt.avg(brood.table = bt.complete[bt.complete$Ocean.Region == "BS", ],
                           env.data = raw.clim.mld.late,
                           env.covar = "Amld_winter",
                           type = "second_year",
                           out.covar = "late_mld_winter")
late.mld.winter <- rbind(mld.wi2.wc, mld.wi2.bs)


## Summer MLD during 2nd year of marine life
mld.su2.wc <- clim.wgt.avg(brood.table = bt.complete[bt.complete$Ocean.Region != "BS", ],
                           env.data = raw.clim.mld.late,
                           env.covar = "GoAmld_summer",
                           type = "second_year",
                           out.covar = "late_mld_summer")
mld.su2.bs <- clim.wgt.avg(brood.table = bt.complete[bt.complete$Ocean.Region == "BS", ],
                           env.data = raw.clim.mld.late,
                           env.covar = "Amld_summer",
                           type = "second_year",
                           out.covar = "late_mld_summer")
late.mld.summer <- rbind(mld.su2.wc, mld.su2.bs)



## Age weighted competitor indices -------------------------


## (1) competitors in second year of marine life
np.pink.sec <- pink.wgt.avg(brood.table = bt.complete,
                            pink.data = raw.comp,
                            pink.covar = "pink_numbers_np",
                            type = "second_year",
                            out.covar = "np_pinks_sec")

na.pink.sec <- pink.wgt.avg(brood.table = bt.complete,
                            pink.data = raw.comp,
                            pink.covar = "pink_numbers_na",
                            type = "second_year",
                            out.covar = "na_pinks_sec")

np.pink.lm.sec <- pink.wgt.avg(brood.table = bt.complete,
                               pink.data = comp.detrend,
                               pink.covar = "pink_lm",
                               type = "second_year",
                               out.covar = "np_pinks_lm_sec")

np.pink.exp.sec <- pink.wgt.avg(brood.table = bt.complete,
                                pink.data = comp.detrend,
                                pink.covar = "pink_exp",
                                type = "second_year",
                                out.covar = "np_pinks_exp_sec")

np.pink.hatch.sec <- pink.wgt.avg(brood.table = bt.complete,
                                  pink.data = raw.comp,
                                  pink.covar = "pink_numbers_np_hatch",
                                  type = "second_year",
                                  out.covar = "np_pinks_hatch_sec")


## (2) geographically varying competitors in second year of marine life
np.pink.geo <- pink.wgt.avg(brood.table = bt.complete,
                        pink.data = raw.comp,
                        pink.covar = "pink_numbers_np",
                        type = "geographic",
                        out.covar = "np_pinks_geo")

na.pink.geo <- pink.wgt.avg(brood.table = bt.complete,
                        pink.data = raw.comp,
                        pink.covar = "pink_numbers_na",
                        type = "geographic",
                        out.covar = "na_pinks_geo")

np.pink.lm.geo <- pink.wgt.avg(brood.table = bt.complete,
                           pink.data = comp.detrend,
                           pink.covar = "pink_lm",
                           type = "geographic",
                           out.covar = "np_pinks_lm_geo")

np.pink.exp.geo <- pink.wgt.avg(brood.table = bt.complete,
                            pink.data = comp.detrend,
                            pink.covar = "pink_exp",
                            type = "geographic",
                            out.covar = "np_pinks_exp_geo")

np.pink.hatch.geo <- pink.wgt.avg(brood.table = bt.complete,
                              pink.data = raw.comp,
                              pink.covar = "pink_numbers_np_hatch",
                              type = "geographic",
                              out.covar = "np_pinks_hatch_geo")


## (3) Regionally varying total competitors (local sockeye excluded)
##     geographically varying weights
all_spp_numbers_np_geo <- pink.wgt.avg(brood.table = bt.complete,
                                   pink.data = raw.comp,
                                   pink.covar = c(WC  = "all_spp_numbers_np_wc",
                                                  GOA = "all_spp_numbers_np_goa",
                                                  BS  = "all_spp_numbers_np_bs"),
                                   type = "geographic",
                                   out.covar = "all_spp_numbers_np_geo")

all_spp_biomass_np_geo <- pink.wgt.avg(brood.table = bt.complete,
                                   pink.data = raw.comp,
                                   pink.covar = c(WC  = "all_spp_biomass_np_wc",
                                                  GOA = "all_spp_biomass_np_goa",
                                                  BS  = "all_spp_biomass_np_bs"),
                                   type = "geographic",
                                   out.covar = "all_spp_biomass_np_geo")

all_spp_numbers_na_geo <- pink.wgt.avg(brood.table = bt.complete,
                                   pink.data = raw.comp,
                                   pink.covar = c(WC  = "all_spp_numbers_na_wc",
                                                  GOA = "all_spp_numbers_na_goa",
                                                  BS  = "all_spp_numbers_na_bs"),
                                   type = "geographic",
                                   out.covar = "all_spp_numbers_na_geo")

all_spp_numbers_np_sec <- pink.wgt.avg(brood.table = bt.complete,
                                       pink.data = raw.comp,
                                       pink.covar = c(WC  = "all_spp_numbers_np_wc",
                                                      GOA = "all_spp_numbers_np_goa",
                                                      BS  = "all_spp_numbers_np_bs"),
                                       type = "second_year",
                                       out.covar = "all_spp_numbers_np_sec")

all_spp_biomass_np_sec <- pink.wgt.avg(brood.table = bt.complete,
                                       pink.data = raw.comp,
                                       pink.covar = c(WC  = "all_spp_biomass_np_wc",
                                                      GOA = "all_spp_biomass_np_goa",
                                                      BS  = "all_spp_biomass_np_bs"),
                                       type = "second_year",
                                       out.covar = "all_spp_biomass_np_sec")

all_spp_numbers_na_sec <- pink.wgt.avg(brood.table = bt.complete,
                                       pink.data = raw.comp,
                                       pink.covar = c(WC  = "all_spp_numbers_na_wc",
                                                      GOA = "all_spp_numbers_na_goa",
                                                      BS  = "all_spp_numbers_na_bs"),
                                       type = "second_year",
                                       out.covar = "all_spp_numbers_na_sec")


## Merge datasets
master <- merge(bt.complete, early.sst, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, np.pink.geo, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, np.pink.hatch.geo, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, na.pink.geo, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, np.pink.lm.geo, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, np.pink.exp.geo, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, all_spp_numbers_np_geo, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, all_spp_biomass_np_geo, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, all_spp_numbers_na_geo, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, np.pink.sec, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, np.pink.hatch.sec, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, na.pink.sec, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, np.pink.lm.sec, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, np.pink.exp.sec, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, all_spp_numbers_np_sec, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, all_spp_biomass_np_sec, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, all_spp_numbers_na_sec, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, late.sst, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, late.mld.winter, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, late.mld.summer, by=c("BY","Stock.ID"),all.x=T)
master <- merge(master, early.sst.lm, by=c("BY","Stock.ID"),all.x=T)
master.bt_w_cov1 <- master[order(master$Stock.ID),]
head(master.bt_w_cov1)
tail(master.bt_w_cov1)
summary(master.bt_w_cov1)
sapply(master.bt_w_cov1, class)
unique(master.bt_w_cov1$Stock.ID)


## Add derived columns
master.bt_w_cov2 <- plyr::ddply(master.bt_w_cov1, .(Stock.ID), transform,
                                RS = R/S,
                                RS_stnd = scale(R/S)[ , 1],
                                lnRS = log(R/S),
                                S_stnd = scale(S)[ , 1],
                                early_sst_stnd = scale(early_sst)[ , 1],
                                late_sst_stnd = scale(late_sst)[ , 1],
                                late_mld_winter_stnd = scale(late_mld_winter)[ , 1],
                                late_mld_summer_stnd = scale(late_mld_summer)[ , 1],
                                early_sst_lm_stnd = scale(early_sst_lm)[ , 1],
                                np_pinks_geo_stnd = scale(np_pinks_geo)[ , 1],
                                np_pinks_hatch_geo_stnd = scale(np_pinks_hatch_geo)[ , 1],
                                na_pinks_geo_stnd = scale(na_pinks_geo)[ , 1],
                                np_pinks_lm_geo_stnd = scale(np_pinks_lm_geo)[ , 1],
                                np_pinks_exp_geo_stnd = scale(np_pinks_exp_geo)[ , 1],
                                all_spp_numbers_np_geo_stnd = scale(all_spp_numbers_np_geo)[ , 1],
                                all_spp_biomass_np_geo_stnd = scale(all_spp_biomass_np_geo)[ , 1],
                                all_spp_numbers_na_geo_stnd = scale(all_spp_numbers_na_geo)[ , 1],
                                np_pinks_sec_stnd = scale(np_pinks_sec)[ , 1],
                                np_pinks_hatch_sec_stnd = scale(np_pinks_hatch_sec)[ , 1],
                                na_pinks_sec_stnd = scale(na_pinks_sec)[ , 1],
                                np_pinks_lm_sec_stnd = scale(np_pinks_lm_sec)[ , 1],
                                np_pinks_exp_sec_stnd = scale(np_pinks_exp_sec)[ , 1],
                                all_spp_numbers_np_sec_stnd = scale(all_spp_numbers_np_sec)[ , 1],
                                all_spp_biomass_np_sec_stnd = scale(all_spp_biomass_np_sec)[ , 1],
                                all_spp_numbers_na_sec_stnd = scale(all_spp_numbers_na_sec)[ , 1])


## Fill in missing years that fall w/in min and max BY for each stock
master.bt_w_cov3 <- fill.time.series(master.bt_w_cov2)

## Export to output
master.bt_w_cov <- master.bt_w_cov3
write.csv(master.bt_w_cov, "data/master_brood_table_covar.csv", row.names=FALSE)


sock <- master.bt_w_cov
sock$Stock <- factor(sock$Stock, levels = unique(sock$Stock))
