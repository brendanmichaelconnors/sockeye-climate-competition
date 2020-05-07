## Fit single-stock generalized Ricker models
##
## This script fits and explores single-stock versions of our proposed
## hierarchical models. The parameters in these single-stock models are
## estimated only using data from a single stock, rather than being informed
## from data from multiple stocks in the hierarchical models.
##
## The reasons for exploring these single-stock models include:
##  1. Have a base set of results in which to compare the hierarchical models
##  2. Determine the most appropriate hierarchical structure for the parameters
##  3. Explore model assumptions, e.g., normality, co-linearity, etc.



## Specify model formulas ----------------------------------

m1a.formula <- lnRS ~ S | Stock
m2a.formula <- lnRS ~ S + early_sst_stnd | Stock

m3a.formula <- lnRS ~ S + late_sst_stnd | Stock
m3c.formula <- lnRS ~ S + late_mld_winter_stnd | Stock
m3d.formula <- lnRS ~ S + late_mld_summer_stnd | Stock

m4a.formula <- lnRS ~ S + np_pinks_sec_stnd | Stock
m4b.formula <- lnRS ~ S + np_pinks_geo_stnd | Stock

m5a.formula <- lnRS ~ S + early_sst_stnd + np_pinks_sec_stnd | Stock
m5b.formula <- lnRS ~ S + early_sst_stnd + np_pinks_geo_stnd | Stock

m6a.formula <- lnRS ~ S + late_sst_stnd + np_pinks_sec_stnd | Stock
m6b.formula <- lnRS ~ S + late_sst_stnd + np_pinks_geo_stnd | Stock
m6c.formula <- lnRS ~ S + late_mld_winter_stnd + np_pinks_sec_stnd | Stock
m6d.formula <- lnRS ~ S + late_mld_summer_stnd + np_pinks_sec_stnd | Stock

m7a.formula <- lnRS ~ S + early_sst_stnd*np_pinks_sec_stnd | Stock
m7b.formula <- lnRS ~ S + early_sst_stnd*np_pinks_geo_stnd | Stock
m7e.formula <- lnRS ~ S + early_sst_lm_stnd*np_pinks_exp_sec_stnd | Stock

m8a.formula <- lnRS ~ S + late_sst_stnd*np_pinks_sec_stnd | Stock
m8b.formula <- lnRS ~ S + late_sst_stnd*np_pinks_geo_stnd | Stock
m8c.formula <- lnRS ~ S + late_mld_winter_stnd*np_pinks_sec_stnd | Stock
m8d.formula <- lnRS ~ S + late_mld_summer_stnd*np_pinks_sec_stnd | Stock

m9a.formula <- lnRS ~ S +
    early_sst_stnd +
    late_sst_stnd +
    np_pinks_sec_stnd +
    early_sst_stnd:np_pinks_sec_stnd +
    late_sst_stnd:np_pinks_sec_stnd | Stock
m9b.formula <- lnRS ~ S +
    early_sst_stnd +
    late_sst_stnd +
    np_pinks_geo_stnd +
    early_sst_stnd:np_pinks_geo_stnd +
    late_sst_stnd:np_pinks_geo_stnd | Stock
m9c.formula <- lnRS ~ S +
    early_sst_stnd +
    late_mld_winter_stnd +
    np_pinks_sec_stnd +
    early_sst_stnd:np_pinks_sec_stnd +
    late_mld_winter_stnd:np_pinks_sec_stnd | Stock
m9d.formula <- lnRS ~ S +
    early_sst_stnd +
    late_mld_summer_stnd +
    np_pinks_sec_stnd +
    early_sst_stnd:np_pinks_sec_stnd +
    late_mld_summer_stnd:np_pinks_sec_stnd | Stock

m10a.formula <- lnRS ~ S + early_sst_stnd + late_sst_stnd + np_pinks_sec_stnd | Stock
m10b.formula <- lnRS ~ S + early_sst_stnd + late_sst_stnd + np_pinks_geo_stnd | Stock
m10c.formula <- lnRS ~ S + early_sst_stnd + late_mld_winter_stnd + np_pinks_sec_stnd | Stock
m10d.formula <- lnRS ~ S + early_sst_stnd + late_mld_summer_stnd + np_pinks_sec_stnd | Stock

mod.list <- list(model1a = m1a.formula,
                 model2a = m2a.formula,
                 model3a = m3a.formula,
                 model3c = m3c.formula,
                 model3d = m3d.formula,
                 model4a = m4a.formula,
                 model4b = m4b.formula,
                 model5a = m5a.formula,
                 model5b = m5b.formula,
                 model6a = m6a.formula,
                 model6b = m6b.formula,
                 model6c = m6c.formula,
                 model6d = m6d.formula,
                 model7a = m7a.formula,
                 model7b = m7b.formula,
                 model7e = m7e.formula,
                 model8a = m8a.formula,
                 model8b = m8b.formula,
                 model8c = m8c.formula,
                 model8d = m8d.formula,
                 model9a = m9a.formula,
                 model9b = m9b.formula,
                 model9c = m9c.formula,
                 model9d = m9d.formula,
                 model10a = m10a.formula,
                 model10b = m10b.formula,
                 model10c = m10c.formula,
                 model10d = m10d.formula)



## Fit single-stock models ---------------------------------

## All years of data
ss.all.yrs <- single.stock.fit(mod.list,
                               years = seq(min(sock$BY), max(sock$BY)),
                               plot.path = "./figures/single-stock/")


## Explore models fit only using years 1977:maxBY
ss.post1976 <- single.stock.fit(mod.list[!grepl("^model9", names(mod.list))],
                                years = seq(1977, max(sock$BY)), ## inclusive
                                plot.path = "./figures/single-stock-post1976/")



## Plot R-squared values -----------------------------------
# rsq.df <- plyr::rbind.fill(ss.all.yrs$rsq)
# rsq.avg <- plyr::ddply(rsq.df, .(model), summarize,
#                        avg = mean(r.squared))
# barchart(model ~ avg, data = rsq.avg)



## Compare single stock model coefficients -----------------
compare.dir <- "./figures/single-stock/compare"
if(!dir.exists(compare.dir)) {
    dir.create(compare.dir, recursive = TRUE)
}


## Pink effect: second yr + geographic
pdf(paste0(compare.dir, "model7_pink_effect.pdf"), width = 6, height = 8)
single.stock.compare(ss.all.yrs$coef, "model7a", "model7b",
                     compare = c("np_pinks_sec_stnd", "np_pinks_geo_stnd"),
                     levels = c("Geographic", "Second yr"),
                     main = "pink coefficients")
dev.off()

## Pink x SST interaction: second yr + geographic
pdf(paste0(compare.dir, "model7_pink_sst_interaction.pdf"), width = 6, height = 8)
single.stock.compare(ss.all.yrs$coef, "model7a", "model7b",
                     compare = c("early_sst_stnd:np_pinks_sec_stnd", "early_sst_stnd:np_pinks_geo_stnd"),
                     levels = c("Early SST x Second yr", "Early SST x Geographic"),
                     main = "interaction")
dev.off()




## Pink effect conditional on early SST --------------------
## Here we explore the pink salmon effect conditional on early SST either being
## below or above average. This mini-analysis attempts to explore the dynamics
## of the interaction term SST x Pinks by looking at the pink effect conditional
## on the values of SST.

cond.dir <- "./figures/single-stock/conditional_pink_effects/"
if(!dir.exists(cond.dir)) {
    dir.create(cond.dir, recursive = TRUE)
}


## Fit model 4 separately using data where early SST is above
## the long-term average and below the long-term average.
sock.sst.pos <- sock[sock$early_sst_stnd >= 0, ]
sock.sst.neg <- sock[sock$early_sst_stnd < 0, ]
m.fit.pos <- lmList(m4a.formula, data = sock.sst.pos, na.action = na.omit)
m.fit.neg <- lmList(m4a.formula, data = sock.sst.neg, na.action = na.omit)

## get unique ocean.regions and stock names
reg1 <- subset(sock, select = c("Stock", "Ocean.Region"))
regions <- reg1[!duplicated(reg1), ]

## Create coefficient data.frame
m.coef.pos <- coef(m.fit.pos)
m.coef.pos$Stock <- names(m.fit.pos)
m.coef.pos <- melt(m.coef.pos, id.vars = "Stock")
m.coef.pos$Stock <- factor(m.coef.pos$Stock, levels = names(m.fit.pos))
m.coef.pos <- merge(m.coef.pos, regions, by = "Stock", all.x = TRUE, sort = FALSE)
m.coef.pos$sst_cond <- "SST positive"
#
m.coef.neg <- coef(m.fit.neg)
m.coef.neg$Stock <- names(m.fit.neg)
m.coef.neg <- melt(m.coef.neg, id.vars = "Stock")
m.coef.neg$Stock <- factor(m.coef.neg$Stock, levels = names(m.fit.neg))
m.coef.neg <- merge(m.coef.neg, regions, by = "Stock", all.x = TRUE, sort = FALSE)
m.coef.neg$sst_cond <- "SST negative"
#
m.coef <- rbind(m.coef.pos, m.coef.neg)

cond.file <- paste0(cond.dir, "model4a_conditional_sst_coef_dot.pdf")
pdf(cond.file, width = 7, height = 8)
g <- dotplot(as.factor(Stock) ~ value | variable, data = m.coef,
             subset = variable == "np_pinks_sec_stnd",
             groups = sst_cond,
             par.settings = theme.mjm(),
             layout = c(NA, 1),
             scales = list(x = list(relation = "free")),
             ylab = "",
             xlab = "Coefficient",
             main = "Geographic pink coefficients conditional on early SST values",
             sub = paste0("model4: ", deparse(m4a.formula)),
             auto.key = list(space = "right"),
             panel = function(x, y, ...) {
                 panel.abline(v = 0, col = "grey60", lty = 2)
                 panel.dotplot(x, y, ...)
             })
print(g)
dev.off()


## Fit model 2 and plot residuals ~ np_pinks conditional on SST
## being above or below the long-term average: look for non-linearities
dat.c <- sock[!is.na(sock$lnRS), ]
fit <- lmList(m2a.formula, data = dat.c)
dat.c$resid <- resid(fit)
dat.c$sst_lo_hi <- ifelse(dat.c$early_sst_stnd < 0, "SST negative", "SST positive")

cond.file <- paste0(cond.dir, "model2a_conditional_residual_pinks.pdf")
pdf(cond.file, width = 16, height = 9)
g <- xyplot(resid ~ np_pinks_sec | Stock, data = dat.c,
            groups = sst_lo_hi,
            type = c("p", "smooth"),
            # type = c("p", "r"),
            main = "Geographic pink relationship conditional on early SST values",
            xlab = "NP pink index",
            ylab = "Residuals: lnRS = a + bS + early_sst",
            lwd = 2,
            span = 1.1,
            cex = 0.5,
            par.settings = theme.mjm(),
            auto.key = list(space = "right",
                            padding.text = 2.5,
                            between = 0.7),
            panel = function(x, y, ...) {
                panel.abline(v = 400, lty = 2, col = "grey50")
                panel.xyplot(x, y, ...)
            })
print(g)
dev.off()
