## De-trend covariates

if(!dir.exists("./figures/detrend-covars/"))
    dir.create("./figures/detrend-covars/")

## Pink abundance ------------------------------------------

## Get pink abundance data
comp.dat <- read.table(file = "./data/competitor_indices_2019_09_26.csv",
                       stringsAsFactors = FALSE, sep = ",",
                       header=TRUE)
comp.dat <- subset(comp.dat, select = c(Year, pink_numbers_np))
names(comp.dat) <- c("Year", "pink_raw")


## Fit time-trend models
pink.lm <- lm(pink_raw ~ Year, data = comp.dat)

pink.exp <- nls(pink_raw ~ I(exp(1)^(a + b * Year)),
               data = comp.dat, start = c(a = 0, b = 0.001), trace = FALSE)

pink.gam <- mgcv::gam(pink_raw ~ s(Year, k = 3), data = comp.dat, gamma = 2)


## Save de-trended data
comp.dat$pink_lm  <- residuals(pink.lm)
comp.dat$pink_exp <- residuals(pink.exp)
comp.dat$pink_gam <- residuals(pink.gam)
write.csv(comp.dat, "./data/pink_abundance_detrend.csv", row.names = FALSE)


## MSE
sum(comp.dat$pink_lm^2)
sum(comp.dat$pink_exp^2)
sum(comp.dat$pink_gam^2)


## Vis model fits
pdf("./figures/detrend-covars/pink_fits.pdf", width = 8, height = 9)
par(mfrow = c(2, 1))
plot(comp.dat$Year, comp.dat$pink_raw,
     type = "o",
     lty = 2,
     pch = 19,
     xlab = "Year",
     ylab = "Pink salmon abundance",
     axes = FALSE)
box(col = "grey50")
axis(side = 1, lwd = 0, lwd.tick = 1, col = "grey65")
axis(side = 2, lwd = 0, lwd.tick = 1, las = 1, col = "grey65")
lines(comp.dat$Year, predict(pink.lm), lwd = 2, col = "blue3")
lines(comp.dat$Year, predict(pink.exp),lwd = 2,  col = "red3")
lines(comp.dat$Year, predict(pink.gam),lwd = 2,  col = "green3")
legend("topleft", legend = c("data", "lm", "exp", "gam"), lwd = 2,
       col = c("black", "blue3", "red3", "green3"),
       bty = 'n')
##
plot(comp.dat$Year, resid(pink.lm),
     ylim = c(-200, 200),
     xlab = "Year",
     ylab = "Residual pink index",
     axes = FALSE,
     type = "o",
     pch = 19,
     col = "blue3",
     panel.first = abline(h = 0, col = "grey50", lty = 2))
box(col = "grey50")
axis(side = 1, lwd = 0, lwd.tick = 1, col = "grey65")
axis(side = 2, lwd = 0, lwd.tick = 1, las = 1, col = "grey65")
lines(comp.dat$Year, resid(pink.exp), type = "o", col = "red3", pch = 19)
lines(comp.dat$Year, resid(pink.gam), type = "o", col = "green3", pch = 19)
dev.off()


## Vis residual series correlations
pdf("./figures/detrend-covars/pink_cor.pdf", width = 8, height = 8)
g <- splom(comp.dat[ , names(comp.dat) != "Year"],
           par.settings = theme.mjm(),
           pch = 19, las = 1,
           panel = function(x, y, ...) {
               panel.splom(x, y, ...)
                panel.text(min(x), max(y),
                           paste("r =", round(cor(x, y, use = "pairwise.complete.obs"), 2)),
                           cex = 0.8, adj = 0, col = "black")
           })
print(g)
dev.off()


## Vis ACF of residual series
pdf("./figures/detrend-covars/pink_acf.pdf", width = 9, height = 8)
par(mfrow = c(2, 2), las = 1)
acf(comp.dat$pink_raw, main = "Pink data")
acf(resid(pink.lm), main = "Linear model residuals")
acf(resid(pink.exp), main = "Exponential model residuals")
acf(resid(pink.gam), main = "GAM model residuals")
dev.off()

# acf(comp.dat$pink_raw, main = "Pink data")$acf
# acf(resid(pink.lm), main = "Linear model residuals")$acf
# acf(resid(pink.exp), main = "Exponential model residuals")$acf
# acf(resid(pink.gam), main = "GAM model residuals")$acf



## SST 1st year --------------------------------------------
## Here, I use the raw SST values, rather than the anomalies

## Get SST data
sst.dat <- read.table("./data/sst_yr_1_stock_anomalies.csv",
                      stringsAsFactors = FALSE, sep = ",",
                      header = TRUE)


## Fit models
sst.lst  <- vector("list", length(unique(sst.dat$Stock.ID)))
sst.pred <- vector("list", length(unique(sst.dat$Stock.ID)))
ind <- 1
for(i in unique(sst.dat$Stock.ID)) {
    sst.dat.i  <- sst.dat[sst.dat$Stock.ID == i, ]
    sst.pred.i <- sst.dat[sst.dat$Stock.ID == i, ]
    sst.lm <- lm(sst_raw ~ Year, data = sst.dat.i)
    sst.exp <- nls(sst_raw ~ I(exp(1)^(a + b * Year)),
                   control = list(maxiter = 500),
                   data = sst.dat.i, start = c(a = -1, b = 0.001), trace = FALSE)
    sst.gam <- mgcv::gam(sst_raw ~ s(Year, k = 3), data = sst.dat.i, gamma = 2)

    sst.pred.i$sst_lm  <- predict(sst.lm)
    sst.pred.i$sst_exp <- predict(sst.exp)
    sst.pred.i$sst_gam <- predict(sst.gam)
    sst.pred[[ind]] <- sst.pred.i

    sst.dat.i$sst_lm  <- residuals(sst.lm)
    sst.dat.i$sst_exp <- residuals(sst.exp)
    sst.dat.i$sst_gam <- residuals(sst.gam)
    sst.lst[[ind]] <- sst.dat.i
    ind <- ind + 1
}
sst.detrend <- plyr::rbind.fill(sst.lst)
sst.predict <- plyr::rbind.fill(sst.pred)
write.csv(sst.detrend, "./data/sst_yr_1_stock_anomalies_detrend.csv", row.names = FALSE)


## Vis model fits
pdf("./figures/detrend-covars/sst_fits.pdf", width = 8, height = 9)
for(i in unique(sst.detrend$Stock.ID)) {
    sst.i <- sst.detrend[sst.detrend$Stock.ID == i, ]
    pred.i <- sst.predict[sst.predict$Stock.ID == i, ]
    par(mfrow = c(2, 1))
    plot(sst.i$Year, sst.i$sst_raw,
         type = "o",
         main = i,
         lty = 2,
         pch = 19,
         xlab = "Year",
         ylab = "SST raw",
         axes = FALSE,
         panel.first = abline(h = 0, col = "grey50", lty = 2))
    box(col = "grey50")
    axis(side = 1, lwd = 0, lwd.tick = 1, col = "grey65")
    axis(side = 2, lwd = 0, lwd.tick = 1, las = 1, col = "grey65")
    lines(pred.i$Year, pred.i$sst_lm, lwd = 2, col = "blue3")
    lines(pred.i$Year, pred.i$sst_exp, lwd = 2,  col = "red3")
    lines(pred.i$Year, pred.i$sst_gam, lwd = 2,  col = "green3")
    legend("topleft", legend = c("data", "lm", "exp", "gam"), lwd = 2,
           col = c("black", "blue3", "red3", "green3"),
           bty = 'n')
    ##
    plot(sst.i$Year, sst.i$sst_lm,
         ylim = c(-2, 2),
         xlab = "Year",
         ylab = "Residual sst index",
         axes = FALSE,
         type = "o",
         pch = 19,
         col = "blue3",
         panel.first = abline(h = 0, col = "grey50", lty = 2))
    box(col = "grey50")
    axis(side = 1, lwd = 0, lwd.tick = 1, col = "grey65")
    axis(side = 2, lwd = 0, lwd.tick = 1, las = 1, col = "grey65")
    lines(sst.i$Year, sst.i$sst_exp, type = "o", col = "red3", pch = 19)
    lines(sst.i$Year, sst.i$sst_gam, type = "o", col = "green3", pch = 19)
}
dev.off()


## Vis residual series correlations
pdf("./figures/detrend-covars/sst_cor.pdf", width = 8, height = 8)
for(i in unique(sst.detrend$Stock.ID)) {
    sst.i <- sst.detrend[sst.detrend$Stock.ID == i, ]
    g <- splom(sst.i[ , !names(sst.i) %in% c("Year", "sst_anomaly", "Stock.ID")],
               par.settings = theme.mjm(),
               main = as.character(i),
               pch = 19, las = 1,
               panel = function(x, y, ...) {
                   panel.splom(x, y, ...)
                    panel.text(min(x), max(y),
                               paste("r =", round(cor(x, y, use = "pairwise.complete.obs"), 2)),
                               cex = 0.8, adj = 0, col = "black")
               })
    print(g)
}
dev.off()


## Vis ACF of residual series
pdf("./figures/detrend-covars/sst_acf.pdf", width = 9, height = 8)
for(i in unique(sst.detrend$Stock.ID)) {
    sst.i <- sst.detrend[sst.detrend$Stock.ID == i, ]
    par(mfrow = c(2, 2), las = 1)
    acf(sst.i$sst_raw, main = "SST raw")
    acf(sst.i$sst_lm, main = "Linear model residuals")
    acf(sst.i$sst_exp, main = "Exponential model residuals")
    acf(sst.i$sst_gam, main = "GAM model residuals")
    title(as.character(i), line = -1.5, outer = TRUE)
}
dev.off()
