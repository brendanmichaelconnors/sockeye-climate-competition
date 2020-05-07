## Fit hierarchical bayesian models

if(!dir.exists("./figures/hbm_fit/"))
    dir.create("./figures/hbm_fit/")
if(!dir.exists("./output/models/"))
    dir.create("./output/models/")

pars.gen.quant <- c("log_lik", "yhat", "yrep", "yresid") ## Generated quantities to monitor


## hb07a.pr1 -----------------------------------------------
## Total pink North Pacific

## Monitor params
pars.hb07 <- c("alpha", "beta", "sigma", "phi", "mu_alpha", "sigma_alpha",
               "gamma", "mu_gamma", "sigma_gamma",
               "kappa", "mu_kappa", "sigma_kappa",
               "chi", "mu_chi", "sigma_chi")
save(pars.hb07, file = "./output/pars_hb07.RData")


## Run MCMC
stan.dat.hb07a <- stan_data(sock,
                            scale.x1 = TRUE,
                            var.x2 = "early_sst_stnd",
                            var.x3 = "np_pinks_sec_stnd")
hb07a <- rstan::stan(file = "./stan/hb07_pr1.stan",
                     data = stan.dat.hb07a,
                     pars = c(pars.hb07, pars.gen.quant),
                     warmup = 1000,
                     iter = 4000,
                     cores = 4,
                     chains = 4,
                     thin = 2,
                     seed = 123,
                     control = list(adapt_delta = 0.90,
                                    max_treedepth = 10))
save(hb07a, file = "./output/models/hb07a.RData")

pdf("./figures/hbm_fit/hb07a_diag.pdf", width = 7, height = 5)
    coda_neff(get_neff(hb07a, pars = pars.hb07), total_draws(hb07a))
    coda_rhat(get_rhat(hb07a, pars = pars.hb07))
    coda_diag(As.mcmc.list(hb07a, pars = pars.hb07))
dev.off()

plot_post_pc(hb07a, stan.dat.hb07a$y, "./figures/hbm_fit/hb07a_yrep.pdf")

loo.hb07a <- rstan::loo(hb07a, cores = 4)
save(loo.hb07a, file = "./output/loo_hb07a.RData")
waic.hb07a <- loo::waic(loo::extract_log_lik(hb07a, "log_lik"))
save(waic.hb07a, file = "./output/waic_hb07a.RData")
pdf("./figures/hbm_fit/hb07a_loo.pdf", width = 7, height = 5)
    plot(loo.hb07a, label_points = TRUE)
dev.off()

r2.hb07a <- bayes_R2(sock$lnRS, as.matrix(hb07a, pars = "yhat"))
save(r2.hb07a, file = "./output/r2_hb07a.RData")

pdf("./figures/hbm_fit/hb07a_resid.pdf", width = 8, height = 8)
    plot_hbm_resids(hb07a, sock)
dev.off()



## hb07g.pr1 -----------------------------------------------
## Total pink North Pacific geographic weighting, post '77 BYs

## Monitor params
pars.hb07 <- c("alpha", "beta", "sigma", "phi", "mu_alpha", "sigma_alpha",
               "gamma", "mu_gamma", "sigma_gamma",
               "kappa", "mu_kappa", "sigma_kappa",
               "chi", "mu_chi", "sigma_chi")
save(pars.hb07, file = "./output/pars_hb07.RData")


## Run MCMC
stan.dat.hb07g <- stan_data(sock[sock$BY >= 1975,],
                            scale.x1 = TRUE,
                            var.x2 = "early_sst_stnd",
                            var.x3 = "np_pinks_geo_stnd")
hb07g <- rstan::stan(file = "./stan/hb07_pr1.stan",
                     data = stan.dat.hb07g,
                     pars = c(pars.hb07, pars.gen.quant),
                     warmup = 1000,
                     iter = 4000,
                     cores = 4,
                     chains = 4,
                     thin = 2,
                     seed = 123,
                     control = list(adapt_delta = 0.90,
                                    max_treedepth = 10))
save(hb07g, file = "./output/models/hb07g.RData")

pdf("./figures/hbm_fit/hb07g_diag.pdf", width = 7, height = 5)
    coda_neff(get_neff(hb07g, pars = pars.hb07), total_draws(hb07g))
    coda_rhat(get_rhat(hb07g, pars = pars.hb07))
    coda_diag(As.mcmc.list(hb07g, pars = pars.hb07))
dev.off()

plot_post_pc(hb07g, stan.dat.hb07g$y, data = sock[sock$BY >= 1975,], "./figures/hbm_fit/hb07g_yrep.pdf")

loo.hb07g <- rstan::loo(hb07g, cores = 4)
save(loo.hb07g, file = "./output/loo_hb07g.RData")
waic.hb07g <- loo::waic(loo::extract_log_lik(hb07g, "log_lik"))
save(waic.hb07g, file = "./output/waic_hb07g.RData")
pdf("./figures/hbm_fit/hb07g_loo.pdf", width = 7, height = 5)
    plot(loo.hb07g, label_points = TRUE)
dev.off()

r2.hb07g <- bayes_R2(sock$lnRS[sock$BY >= 1975], as.matrix(hb07g, pars = "yhat"))
save(r2.hb07g, file = "./output/r2_hb07g.RData")

pdf("./figures/hbm_fit/hb07g_resid.pdf", width = 8, height = 8)
    plot_hbm_resids(hb07g, sock[sock$BY >= 1975,])
dev.off()



## hb07e.pr1 -----------------------------------------------
## detrended, post '77 BYs

## Monitor params
pars.hb07 <- c("alpha", "beta", "sigma", "phi", "mu_alpha", "sigma_alpha",
               "gamma", "mu_gamma", "sigma_gamma",
               "kappa", "mu_kappa", "sigma_kappa",
               "chi", "mu_chi", "sigma_chi")
save(pars.hb07, file = "./output/pars_hb07.RData")


## Run MCMC
stan.dat.hb07e <- stan_data(sock[sock$BY >= 1975,],
                            scale.x1 = TRUE,
                            var.x2 = "early_sst_lm_stnd",
                            var.x3 = "np_pinks_exp_sec_stnd")
hb07e <- rstan::stan(file = "./stan/hb07_pr1.stan",
                     data = stan.dat.hb07e,
                     pars = c(pars.hb07, pars.gen.quant),
                     warmup = 1000,
                     iter = 4000,
                     cores = 4,
                     chains = 4,
                     thin = 2,
                     seed = 123,
                     control = list(adapt_delta = 0.90,
                                    max_treedepth = 10))
save(hb07e, file = "./output/models/hb07e.RData")

pdf("./figures/hbm_fit/hb07e_diag.pdf", width = 7, height = 5)
    coda_neff(get_neff(hb07e, pars = pars.hb07), total_draws(hb07e))
    coda_rhat(get_rhat(hb07e, pars = pars.hb07))
    coda_diag(As.mcmc.list(hb07e, pars = pars.hb07))
dev.off()

plot_post_pc(hb07e, stan.dat.hb07e$y, data = sock[sock$BY >= 1975,], "./figures/hbm_fit/hb07e_yrep.pdf")

loo.hb07e <- rstan::loo(hb07e, cores = 4)
save(loo.hb07e, file = "./output/loo_hb07e.RData")
waic.hb07e <- loo::waic(loo::extract_log_lik(hb07e, "log_lik"))
save(waic.hb07e, file = "./output/waic_hb07e.RData")
pdf("./figures/hbm_fit/hb07e_loo.pdf", width = 7, height = 5)
    plot(loo.hb07e, label_points = TRUE)
dev.off()

r2.hb07e <- bayes_R2(sock$lnRS[sock$BY >= 1975], as.matrix(hb07e, pars = "yhat"))
save(r2.hb07e, file = "./output/r2_hb07e.RData")

pdf("./figures/hbm_fit/hb07e_resid.pdf", width = 8, height = 8)
    plot_hbm_resids(hb07e, sock[sock$BY >= 1975,])
dev.off()


## hb07r.pr1 -----------------------------------------------
## regime: only brood years post 88/89

## Monitor params
pars.hb07 <- c("alpha", "beta", "sigma", "phi", "mu_alpha", "sigma_alpha",
               "gamma", "mu_gamma", "sigma_gamma",
               "kappa", "mu_kappa", "sigma_kappa",
               "chi", "mu_chi", "sigma_chi")
save(pars.hb07, file = "./output/pars_hb07.RData")


## Run MCMC
stan.dat.hb07r <- stan_data(sock[sock$BY >= 1987, ],
                            scale.x1 = TRUE,
                            var.x2 = "early_sst_stnd",
                            var.x3 = "np_pinks_sec_stnd")
hb07r <- rstan::stan(file = "./stan/hb07_pr1.stan",
                     data = stan.dat.hb07r,
                     pars = c(pars.hb07, pars.gen.quant),
                     warmup = 1000,
                     iter = 4000,
                     cores = 4,
                     chains = 4,
                     thin = 2,
                     seed = 123,
                     control = list(adapt_delta = 0.90,
                                    max_treedepth = 10))
save(hb07r, file = "./output/models/hb07r.RData")

pdf("./figures/hbm_fit/hb07r_diag.pdf", width = 7, height = 5)
    coda_neff(get_neff(hb07r, pars = pars.hb07), total_draws(hb07r))
    coda_rhat(get_rhat(hb07r, pars = pars.hb07))
    coda_diag(As.mcmc.list(hb07r, pars = pars.hb07))
dev.off()

plot_post_pc(hb07r, stan.dat.hb07r$y, data = sock[sock$BY >= 1987, ],
             pdf.path = "./figures/hbm_fit/hb07r_yrep.pdf")

loo.hb07r <- rstan::loo(hb07r, cores = 4)
save(loo.hb07r, file = "./output/loo_hb07r.RData")
waic.hb07r <- loo::waic(loo::extract_log_lik(hb07r, "log_lik"))
save(waic.hb07r, file = "./output/waic_hb07r.RData")
pdf("./figures/hbm_fit/hb07r_loo.pdf", width = 7, height = 5)
    plot(loo.hb07r, label_points = TRUE)
dev.off()

r2.hb07r <- bayes_R2(sock$lnRS[sock$BY >= 1987], as.matrix(hb07r, pars = "yhat"))
save(r2.hb07r, file = "./output/r2_hb07r.RData")

pdf("./figures/hbm_fit/hb07r_resid.pdf", width = 8, height = 8)
    plot_hbm_resids(hb07r, sock[sock$BY >= 1987, ])
dev.off()



## hb07r2.pr1 ----------------------------------------------
## regime: only brood years post 76/77

## Monitor params
pars.hb07 <- c("alpha", "beta", "sigma", "phi", "mu_alpha", "sigma_alpha",
               "gamma", "mu_gamma", "sigma_gamma",
               "kappa", "mu_kappa", "sigma_kappa",
               "chi", "mu_chi", "sigma_chi")
save(pars.hb07, file = "./output/pars_hb07.RData")


## Run MCMC
stan.dat.hb07r2 <- stan_data(sock[sock$BY >= 1975, ],
                             scale.x1 = TRUE,
                             var.x2 = "early_sst_stnd",
                             var.x3 = "np_pinks_sec_stnd")
hb07r2 <- rstan::stan(file = "./stan/hb07_pr1.stan",
                     data = stan.dat.hb07r2,
                     pars = c(pars.hb07, pars.gen.quant),
                     warmup = 1000,
                     iter = 4000,
                     cores = 4,
                     chains = 4,
                     thin = 2,
                     seed = 123,
                     control = list(adapt_delta = 0.90,
                                    max_treedepth = 10))
save(hb07r2, file = "./output/models/hb07r2.RData")

pdf("./figures/hbm_fit/hb07r2_diag.pdf", width = 7, height = 5)
coda_neff(get_neff(hb07r2, pars = pars.hb07), total_draws(hb07r2))
coda_rhat(get_rhat(hb07r2, pars = pars.hb07))
coda_diag(As.mcmc.list(hb07r2, pars = pars.hb07))
dev.off()

plot_post_pc(hb07r2, stan.dat.hb07r2$y, data = sock[sock$BY >= 1975,],
             pdf.path = "./figures/hbm_fit/hb07r2_yrep.pdf")

loo.hb07r2 <- rstan::loo(hb07r2, cores = 4)
save(loo.hb07r2, file = "./output/loo_hb07r2.RData")
waic.hb07r2 <- loo::waic(loo::extract_log_lik(hb07r2, "log_lik"))
save(waic.hb07r2, file = "./output/waic_hb07r2.RData")
pdf("./figures/hbm_fit/hb07r2_loo.pdf", width = 7, height = 5)
plot(loo.hb07r2, label_points = TRUE)
dev.off()

r2.hb07r2 <- bayes_R2(sock$lnRS[sock$BY >= 1975], as.matrix(hb07r2, pars = "yhat"))
save(r2.hb07r2, file = "./output/r2_hb07r2.RData")

pdf("./figures/hbm_fit/hb07r2_resid.pdf", width = 8, height = 8)
plot_hbm_resids(hb07r2, sock[sock$BY >= 1975,])
dev.off()




## hb07na.pr1 ----------------------------------------------
## North america pinks only, post '77 BYs

## Monitor params
pars.hb07 <- c("alpha", "beta", "sigma", "phi", "mu_alpha", "sigma_alpha",
               "gamma", "mu_gamma", "sigma_gamma",
               "kappa", "mu_kappa", "sigma_kappa",
               "chi", "mu_chi", "sigma_chi")
save(pars.hb07, file = "./output/pars_hb07.RData")


## Run MCMC
stan.dat.hb07na <- stan_data(sock[sock$BY >= 1975, ],
                             scale.x1 = TRUE,
                            var.x2 = "early_sst_stnd",
                            var.x3 = "na_pinks_sec_stnd")
hb07na <- rstan::stan(file = "./stan/hb07_pr1.stan",
                      data = stan.dat.hb07na,
                      pars = c(pars.hb07, pars.gen.quant),
                      warmup = 1000,
                      iter = 4000,
                      cores = 4,
                      chains = 4,
                      thin = 2,
                      seed = 123,
                      control = list(adapt_delta = 0.90,
                                     max_treedepth = 10))
save(hb07na, file = "./output/models/hb07na.RData")

pdf("./figures/hbm_fit/hb07na_diag.pdf", width = 7, height = 5)
    coda_neff(get_neff(hb07na, pars = pars.hb07), total_draws(hb07na))
    coda_rhat(get_rhat(hb07na, pars = pars.hb07))
    coda_diag(As.mcmc.list(hb07na, pars = pars.hb07))
dev.off()

plot_post_pc(hb07na, stan.dat.hb07na$y, data = sock[sock$BY >= 1975, ], "./figures/hbm_fit/hb07na_yrep.pdf")

loo.hb07na <- rstan::loo(hb07na, cores = 4)
save(loo.hb07na, file = "./output/loo_hb07na.RData")
waic.hb07na <- loo::waic(loo::extract_log_lik(hb07na, "log_lik"))
save(waic.hb07na, file = "./output/waic_hb07na.RData")
pdf("./figures/hbm_fit/hb07na_loo.pdf", width = 7, height = 5)
    plot(loo.hb07na, label_points = TRUE)
dev.off()

r2.hb07na <- bayes_R2(sock$lnRS[sock$BY >= 1975], as.matrix(hb07na, pars = "yhat"))
save(r2.hb07na, file = "./output/r2_hb07na.RData")

pdf("./figures/hbm_fit/hb07na_resid.pdf", width = 8, height = 8)
    plot_hbm_resids(hb07na, sock[sock$BY >= 1975,])
dev.off()



## hb07tn.pr1 ----------------------------------------------
## Total competitors, region specific, North Pacific: numbers of fish, post '77 by

## Monitor params
pars.hb07 <- c("alpha", "beta", "sigma", "phi", "mu_alpha", "sigma_alpha",
               "gamma", "mu_gamma", "sigma_gamma",
               "kappa", "mu_kappa", "sigma_kappa",
               "chi", "mu_chi", "sigma_chi")
save(pars.hb07, file = "./output/pars_hb07.RData")


## Run MCMC
stan.dat.hb07tn <- stan_data(sock[sock$BY >= 1975,],
                             scale.x1 = TRUE,
                            var.x2 = "early_sst_stnd",
                            var.x3 = "all_spp_numbers_np_sec_stnd")
hb07tn <- rstan::stan(file = "./stan/hb07_pr1.stan",
                      data = stan.dat.hb07tn,
                      pars = c(pars.hb07, pars.gen.quant),
                      warmup = 1000,
                      iter = 4000,
                      cores = 4,
                      chains = 4,
                      thin = 2,
                      seed = 123,
                      control = list(adapt_delta = 0.90,
                                     max_treedepth = 10))
save(hb07tn, file = "./output/models/hb07tn.RData")

pdf("./figures/hbm_fit/hb07tn_diag.pdf", width = 7, height = 5)
    coda_neff(get_neff(hb07tn, pars = pars.hb07), total_draws(hb07tn))
    coda_rhat(get_rhat(hb07tn, pars = pars.hb07))
    coda_diag(As.mcmc.list(hb07tn, pars = pars.hb07))
dev.off()

plot_post_pc(hb07tn, stan.dat.hb07tn$y, data = sock[sock$BY >= 1975, ], "./figures/hbm_fit/hb07tn_yrep.pdf")

loo.hb07tn <- rstan::loo(hb07tn, cores = 4)
save(loo.hb07tn, file = "./output/loo_hb07tn.RData")
waic.hb07tn <- loo::waic(loo::extract_log_lik(hb07tn, "log_lik"))
save(waic.hb07tn, file = "./output/waic_hb07tn.RData")
pdf("./figures/hbm_fit/hb07tn_loo.pdf", width = 7, height = 5)
    plot(loo.hb07tn, label_points = TRUE)
dev.off()

r2.hb07tn <- bayes_R2(sock$lnRS[sock$BY >= 1975], as.matrix(hb07tn, pars = "yhat"))
save(r2.hb07tn, file = "./output/r2_hb07tn.RData")

pdf("./figures/hbm_fit/hb07tn_resid.pdf", width = 8, height = 8)
    plot_hbm_resids(hb07tn, sock[sock$BY >= 1975,])
dev.off()



## hb07tb.pr1 ----------------------------------------------
## Total competitors, region specific, North Pacific: biomass, post '77 BYs

## Monitor params
pars.hb07 <- c("alpha", "beta", "sigma", "phi", "mu_alpha", "sigma_alpha",
               "gamma", "mu_gamma", "sigma_gamma",
               "kappa", "mu_kappa", "sigma_kappa",
               "chi", "mu_chi", "sigma_chi")
save(pars.hb07, file = "./output/pars_hb07.RData")


## Run MCMC
stan.dat.hb07tb <- stan_data(sock[sock$BY >= 1975,],
                             scale.x1 = TRUE,
                            var.x2 = "early_sst_stnd",
                            var.x3 = "all_spp_biomass_np_sec_stnd")
hb07tb <- rstan::stan(file = "./stan/hb07_pr1.stan",
                      data = stan.dat.hb07tb,
                      pars = c(pars.hb07, pars.gen.quant),
                      warmup = 1000,
                      iter = 4000,
                      cores = 4,
                      chains = 4,
                      thin = 2,
                      seed = 123,
                      control = list(adapt_delta = 0.90,
                                     max_treedepth = 10))
save(hb07tb, file = "./output/models/hb07tb.RData")

pdf("./figures/hbm_fit/hb07tb_diag.pdf", width = 7, height = 5)
    coda_neff(get_neff(hb07tb, pars = pars.hb07), total_draws(hb07tb))
    coda_rhat(get_rhat(hb07tb, pars = pars.hb07))
    coda_diag(As.mcmc.list(hb07tb, pars = pars.hb07))
dev.off()

plot_post_pc(hb07tb, stan.dat.hb07tb$y, data = sock[sock$BY >= 1975,], "./figures/hbm_fit/hb07tb_yrep.pdf")

loo.hb07tb <- rstan::loo(hb07tb, cores = 4)
save(loo.hb07tb, file = "./output/loo_hb07tb.RData")
waic.hb07tb <- loo::waic(loo::extract_log_lik(hb07tb, "log_lik"))
save(waic.hb07tb, file = "./output/waic_hb07tb.RData")
pdf("./figures/hbm_fit/hb07tb_loo.pdf", width = 7, height = 5)
    plot(loo.hb07tb, label_points = TRUE)
dev.off()

r2.hb07tb <- bayes_R2(sock$lnRS[sock$BY >= 1975], as.matrix(hb07tb, pars = "yhat"))
save(r2.hb07tb, file = "./output/r2_hb07tb.RData")

pdf("./figures/hbm_fit/hb07tb_resid.pdf", width = 8, height = 8)
    plot_hbm_resids(hb07tb, sock[sock$BY >= 1975,])
dev.off()



## hb07tna.pr1 ---------------------------------------------
## Total competitors, region specific, North America: numbers of fish, post '77 BY

## Monitor params
pars.hb07 <- c("alpha", "beta", "sigma", "phi", "mu_alpha", "sigma_alpha",
               "gamma", "mu_gamma", "sigma_gamma",
               "kappa", "mu_kappa", "sigma_kappa",
               "chi", "mu_chi", "sigma_chi")
save(pars.hb07, file = "./output/pars_hb07.RData")


## Run MCMC
stan.dat.hb07tna <- stan_data(sock[sock$BY >= 1975, ],
                              scale.x1 = TRUE,
                              var.x2 = "early_sst_stnd",
                              var.x3 = "all_spp_numbers_na_sec_stnd")
hb07tna <- rstan::stan(file = "./stan/hb07_pr1.stan",
                       data = stan.dat.hb07tna,
                       pars = c(pars.hb07, pars.gen.quant),
                       warmup = 1000,
                       iter = 4000,
                       cores = 4,
                       chains = 4,
                       thin = 2,
                       seed = 123,
                       control = list(adapt_delta = 0.90,
                                      max_treedepth = 10))
save(hb07tna, file = "./output/models/hb07tna.RData")

pdf("./figures/hbm_fit/hb07tna_diag.pdf", width = 7, height = 5)
    coda_neff(get_neff(hb07tna, pars = pars.hb07), total_draws(hb07tna))
    coda_rhat(get_rhat(hb07tna, pars = pars.hb07))
    coda_diag(As.mcmc.list(hb07tna, pars = pars.hb07))
dev.off()

plot_post_pc(hb07tna, stan.dat.hb07tna$y, data = sock[sock$BY >= 1975,], "./figures/hbm_fit/hb07tna_yrep.pdf")

loo.hb07tna <- rstan::loo(hb07tna, cores = 4)
save(loo.hb07tna, file = "./output/loo_hb07tna.RData")
waic.hb07tna <- loo::waic(loo::extract_log_lik(hb07tna, "log_lik"))
save(waic.hb07tna, file = "./output/waic_hb07tna.RData")
pdf("./figures/hbm_fit/hb07tna_loo.pdf", width = 7, height = 5)
    plot(loo.hb07tna, label_points = TRUE)
dev.off()

r2.hb07tna <- bayes_R2(sock$lnRS[sock$BY >= 1975], as.matrix(hb07tna, pars = "yhat"))
save(r2.hb07tna, file = "./output/r2_hb07tna.RData")

pdf("./figures/hbm_fit/hb07tna_resid.pdf", width = 8, height = 8)
    plot_hbm_resids(hb07tna, socksock[sock$BY >= 1975, ])
dev.off()



## hb07oc.pr1 ----------------------------------------------
## Region groupings --> SEAK in GOA, post '77 BYs

## Monitor params
pars.hb07 <- c("alpha", "beta", "sigma", "phi", "mu_alpha", "sigma_alpha",
               "gamma", "mu_gamma", "sigma_gamma",
               "kappa", "mu_kappa", "sigma_kappa",
               "chi", "mu_chi", "sigma_chi")
save(pars.hb07, file = "./output/pars_hb07.RData")


## Run MCMC
stan.dat.hb07oc <- stan_data(sock[sock$BY >= 1975,],
                             scale.x1 = TRUE,
                             var.region = "Ocean.Region2",
                             var.x2 = "early_sst_stnd",
                             var.x3 = "np_pinks_sec_stnd")
hb07oc <- rstan::stan(file = "./stan/hb07_pr1.stan",
                      data = stan.dat.hb07oc,
                      pars = c(pars.hb07, pars.gen.quant),
                      warmup = 1000,
                      iter = 4000,
                      cores = 4,
                      chains = 4,
                      thin = 2,
                      seed = 123,
                      control = list(adapt_delta = 0.90,
                                     max_treedepth = 10))
save(hb07oc, file = "./output/models/hb07oc.RData")

pdf("./figures/hbm_fit/hb07oc_diag.pdf", width = 7, height = 5)
coda_neff(get_neff(hb07oc, pars = pars.hb07), total_draws(hb07oc))
coda_rhat(get_rhat(hb07oc, pars = pars.hb07))
coda_diag(As.mcmc.list(hb07oc, pars = pars.hb07))
dev.off()

plot_post_pc(hb07oc, stan.dat.hb07oc$y, data = sock[sock$BY >= 1975,], "./figures/hbm_fit/hb07oc_yrep.pdf")

loo.hb07oc <- rstan::loo(hb07oc, cores = 4)
save(loo.hb07oc, file = "./output/loo_hb07oc.RData")
waic.hb07oc <- loo::waic(loo::extract_log_lik(hb07oc, "log_lik"))
save(waic.hb07oc, file = "./output/waic_hb07oc.RData")
pdf("./figures/hbm_fit/hb07oc_loo.pdf", width = 7, height = 5)
plot(loo.hb07oc, label_points = TRUE)
dev.off()

r2.hb07oc <- bayes_R2(sock$lnRS[sock$BY >= 1975], as.matrix(hb07oc, pars = "yhat"))
save(r2.hb07oc, file = "./output/r2_hb07oc.RData")

pdf("./figures/hbm_fit/hb07oc_resid.pdf", width = 8, height = 8)
plot_hbm_resids(hb07oc, sock[sock$BY >= 1975,])
dev.off()



## Check pathology -----------------------------------------
rstan::check_hmc_diagnostics(hb07a)
rstan::check_hmc_diagnostics(hb07g)
rstan::check_hmc_diagnostics(hb07e)
rstan::check_hmc_diagnostics(hb07r)
rstan::check_hmc_diagnostics(hb07r2)
rstan::check_hmc_diagnostics(hb07na)
rstan::check_hmc_diagnostics(hb07tn)
rstan::check_hmc_diagnostics(hb07tb)
rstan::check_hmc_diagnostics(hb07tna)
rstan::check_hmc_diagnostics(hb07oc)

rstan::get_elapsed_time(hb07a)
rstan::get_elapsed_time(hb07g)
rstan::get_elapsed_time(hb07e)
rstan::get_elapsed_time(hb07r)
rstan::get_elapsed_time(hb07r2)
rstan::get_elapsed_time(hb07na)
rstan::get_elapsed_time(hb07tn)
rstan::get_elapsed_time(hb07tb)
rstan::get_elapsed_time(hb07tna)
rstan::get_elapsed_time(hb07oc)

summary(hb07a, pars = pars.hb07)
summary(hb07g, pars = pars.hb07)
summary(hb07e, pars = pars.hb07)
summary(hb07r, pars = pars.hb07)
summary(hb07r2, pars = pars.hb07)
summary(hb07na, pars = pars.hb07)
summary(hb07tn, pars = pars.hb07)
summary(hb07tb, pars = pars.hb07)
summary(hb07tna, pars = pars.hb07)
summary(hb07oc, pars = pars.hb07)

neff_lowest(hb07a, pars = pars.hb07)
neff_lowest(hb07g, pars = pars.hb07)
neff_lowest(hb07e, pars = pars.hb07)
neff_lowest(hb07r, pars = pars.hb07)
neff_lowest(hb07r2, pars = pars.hb07)
neff_lowest(hb07na, pars = pars.hb07)
neff_lowest(hb07tn, pars = pars.hb07)
neff_lowest(hb07tb, pars = pars.hb07)
neff_lowest(hb07tna, pars = pars.hb07)
neff_lowest(hb07oc, pars = pars.hb07)

rhat_highest(hb07a, pars = pars.hb07)
rhat_highest(hb07g, pars = pars.hb07)
rhat_highest(hb07e, pars = pars.hb07)
rhat_highest(hb07r, pars = pars.hb07)
rhat_highest(hb07r2, pars = pars.hb07)
rhat_highest(hb07na, pars = pars.hb07)
rhat_highest(hb07tn, pars = pars.hb07)
rhat_highest(hb07tb, pars = pars.hb07)
rhat_highest(hb07tna, pars = pars.hb07)
rhat_highest(hb07oc, pars = pars.hb07)

pairs_lowest(hb07a, pars = pars.hb07)
pairs_lowest(hb07g, pars = pars.hb07)
pairs_lowest(hb07e, pars = pars.hb07)
pairs_lowest(hb07r, pars = pars.hb07)
pairs_lowest(hb07r2, pars = pars.hb07)
pairs_lowest(hb07na, pars = pars.hb07)
pairs_lowest(hb07tn, pars = pars.hb07)
pairs_lowest(hb07tb, pars = pars.hb07)
pairs_lowest(hb07tna, pars = pars.hb07)
pairs_lowest(hb07oc, pars = pars.hb07)

