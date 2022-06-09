# Load packages ----

library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(tmap)
library(lme4)
library(PrevMap)
library(scico)

# load clean sth and grid data 

sth <- read.csv("data/ETH_sth.csv")
sth$Asc_examined <- as.numeric(sth$Asc_examined)

ETH_grid <- read.csv("data/ETH_grid.csv")

length(unique(sth$ID.location))

sth %>% 
  count(Year)

# Plot variables against outcome ----

sth$HK_e.logit <- log((sth$HK_positive+0.5)/(sth$HK_examined-sth$HK_positive+0.5))
sth$Asc_e.logit <- log((sth$Asc_positive+0.5)/(sth$Asc_examined-sth$Asc_positive+0.5))
sth$TT_e.logit <- log((sth$TT_positive+0.5)/(sth$TT_examined-sth$TT_positive+0.5))

species <- c("HK", "Asc", "TT")
vars <- names(sth)[c(11,18:23)]

plot.list <- list()

for (i in 1:length(species)) {
  
  for (j in 1:length(vars)) {
    
    plot.list[[species[i]]][[vars[j]]] <-
      ggplot(sth, aes_string(x = vars[j],
                             y = paste0(species[i], "_e.logit")))+
      geom_point()+
      geom_smooth(method = lm, se = FALSE, col = "red")+
      geom_smooth(se=FALSE)
    
  }
  
}

plot.list$HK
plot.list$HK$travel_w+scale_x_continuous(trans = "log")
plot.list$HK$fric_w + scale_x_continuous(trans = "log")
plot.list$HK$fric_w + geom_smooth(method = lm,
                                  formula = y ~ x + I((x-0.0175)*(x>0.0175)),
                                  col = "green", se = FALSE)

plot.list$Asc

plot.list$TT

# Fit GLMM ----

sth$ID.location <- create.ID.coords(sth, coords = ~utm_x+utm_y)

## HK ----

glmer.fit.HK <- glmer(cbind(HK_positive, HK_examined-HK_positive) ~ 
                        altitude + riv_dist + 
                        fric_w +
                        (1|ID.location),
                      data = sth,
                      family = binomial)

summary(glmer.fit.HK)

# as sigma2 > 0, overdispersion present 

## Asc ----

glmer.fit.Asc <- glmer(cbind(Asc_positive, Asc_examined-Asc_positive) ~
                         altitude + riv_dist + travel_w +
                         (1|ID.location),
                       data = sth,
                       family = binomial)

summary(glmer.fit.Asc)

## TT ====

glmer.fit.TT <- glmer(cbind(TT_positive, TT_examined-TT_positive) ~
                        altitude + riv_dist + travel_w +
                        (1|ID.location),
                      data = sth,
                      family = binomial)

summary(glmer.fit.TT)

# Check residual spatial correlation ----

range(dist(sth[,c("utm_x", "utm_y")]))

spat.corr.diagnostic(formula = HK_positive ~ 
                       altitude + riv_dist + fric_w, 
                     units.m = ~HK_examined,
                     coords = ~I(utm_x/1000)+I(utm_y/1000), # change to km 
                     data = sth, 
                     likelihood = "Binomial",
                     uvec = seq(0, 600, length = 15), 
                     n.sim = 10000,
                     which.test = "variogram")

spat.corr.diagnostic(formula = Asc_positive ~ 
                       altitude + riv_dist + travel_w, 
                     units.m = ~Asc_examined,
                     coords = ~I(utm_x/1000)+I(utm_y/1000), # change to km 
                     data = sth[complete.cases(sth),], 
                     likelihood = "Binomial",
                     uvec = seq(0, 600, length = 15), 
                     n.sim = 10000,
                     which.test = "variogram")

spat.corr.diagnostic(formula = TT_positive ~ 
                       altitude + riv_dist + travel_w, 
                     units.m = ~TT_examined,
                     coords = ~I(utm_x/1000)+I(utm_y/1000), # change to km 
                     data = sth[complete.cases(sth),], 
                     likelihood = "Binomial",
                     uvec = seq(0, 600, length = 15), 
                     n.sim = 10000,
                     which.test = "variogram")

  # all 3 variograms show evidence of residual spatial correlation 

# Fit geostatistical model ----

mcml <- control.mcmc.MCML(n.sim=10000, # number of simulations
                          burnin=2000, # the amount of samples to throw away
                          # the first simulations are not representative
                          # so these are thrown away
                          # 2000 is the minimum to throw away
                          thin=8) # Only retain every nth sample

sth <- sth[complete.cases(sth),]

## HK ----
# get beta guesses
HK.beta.guess <- coef(glm(cbind(HK_positive, HK_examined-HK_positive) ~
                            altitude + riv_dist + fric_w,
                          data = sth, family = binomial))

# get other params guesses

spat.corr.diagnostic(formula = HK_positive ~ 
                       altitude + riv_dist + fric_w, 
                     units.m = ~HK_examined,
                     coords = ~I(utm_x/1000)+I(utm_y/1000), # change to km 
                     data = sth, 
                     likelihood = "Binomial",
                     uvec = seq(0, 600, length = 15), 
                     n.sim = 10000,
                     which.test = "variogram",
                     lse.variogram = TRUE)

HK.sigma2.guess <- 2.481438
HK.phi.guess <- 43.527821
HK.tau2.guess <- 0.718507

par0.HK <- c(HK.beta.guess, HK.sigma2.guess, HK.phi.guess, HK.tau2.guess)

# Fit geostat model 

geostat.fit.HK <- binomial.logistic.MCML(formula = HK_positive ~
                                           altitude + riv_dist + fric_w,
                                         units.m = ~ HK_examined,
                                         coords = ~utm_x+utm_y,
                                         data = sth,
                                         par0 = par0.HK,
                                         control.mcmc = mcml,
                                         kappa = 0.5,
                                         start.cov.pars = c(HK.phi.guess,
                                                            HK.tau2.guess/HK.sigma2.guess),
                                         method = "nlminb")

# Create predicted prevalence raster 
HK.pred.prev <- spatial.pred.binomial.MCML(geostat.fit.HK,
                                           grid.pred = ETH_grid[,1:2],
                                           predictors = ETH_grid[,3:8],
                                           control.mcmc = mcml,
                                           scale.predictions = c("logit", 
                                                                 "prevalence"))

plot(HK.pred.prev, "prevalence", "predictions")
plot(HK.pred.prev, type = "prevalence", summary = "predictions")
# plot(HK.pred.prev, type = "logit", summary = "standard.errors")
  # didn't include 'standard.errors=TRUE' in spatial.pred.binomial.MCML()

# extract samples and back transform from logit to prevalence 
HK.pred.samples <- 1/(1+exp(-HK.pred.prev$samples))

# mean predicted prevalence
HK.prev.mean <- apply(HK.prev.samples, 1, mean)
ETH_grid$HK.prev.mean <- HK.prev.mean

# "anti-prevalence", i.e. prevalence of NO INFECTION 
HK.antiprev.mean <- apply(HK.prev.samples, 1, function(x) mean(1-x))
ETH_grid$HK.antiprev.mean <- HK.antiprev.mean

## Asc ----

# get beta.guess
Asc.beta.guess <- coef(glm(cbind(Asc_positive, Asc_examined-Asc_positive)~
                             altitude + riv_dist + travel_w,
                           data = sth, family = binomial))
# get other params guess 

spat.corr.diagnostic(formula = Asc_positive ~ altitude + riv_dist + travel_w,
                     units.m = ~Asc_examined,
                     coords = ~I(utm_x/1000)+I(utm_y/1000), # change to km 
                     data = sth, 
                     likelihood = "Binomial",
                     uvec = seq(0, 600, length = 15), 
                     n.sim = 10000,
                     which.test = "variogram",
                     lse.variogram = TRUE)

Asc.sigma2.guess <- 1.701543
Asc.phi.guess <- 902.873336
Asc.tau2.guess <- 1.166467

par0.Asc <- c(Asc.beta.guess, Asc.sigma2.guess, Asc.phi.guess, Asc.tau2.guess)

# Fit geostat model 

geostat.fit.Asc <- binomial.logistic.MCML(formula = Asc_positive ~
                                            altitude + riv_dist + travel_w,
                                          units.m = ~Asc_examined,
                                          coords = ~utm_x+utm_y, # change to km 
                                          data = sth,
                                          par0 = par0.Asc,
                                          control.mcmc = mcml,
                                          kappa = 0.5,
                                          start.cov.pars = c(Asc.phi.guess,
                                                             Asc.tau2.guess/Asc.sigma2.guess),
                                          method = "nlminb")

# Create predicted prevalence 

Asc.pred.prev <- spatial.pred.binomial.MCML(geostat.fit.Asc,
                                            grid.pred = ETH_grid[,1:2],
                                            predictors = ETH_grid[,3:8],
                                            control.mcmc = mcml,
                                            scale.predictions = c("logit",
                                                                  "prevalence"))

plot(Asc.pred.prev, type="prevalence", summary = "predictions")

# extract MCMC samples 

Asc.pred.samples <- 1/(1+exp(-Asc.pred.prev$samples))

# Mean Predicted prevalence
Asc.prev.mean <- apply(Asc.pred.samples, 1, mean)
ETH_grid$Asc.prev.mean <- Asc.prev.mean

# Mean anti-prevalence 
Asc.antiprev.mean <- apply(Asc.pred.samples, 1, function(x) mean(1-x))
ETH_grid$Asc.antiprev.mean <- Asc.antiprev.mean

## TT ----

# get beta.guess

TT.beta.guess <- coef(glm(cbind(TT_positive, TT_examined-TT_positive) ~
                            altitude + riv_dist + travel_w,
                          data = sth, family = binomial))

# other params guesses 

spat.corr.diagnostic(formula = TT_positive ~ altitude + riv_dist + travel_w,
                     units.m = ~TT_examined,
                     coords = ~I(utm_x/1000)+I(utm_y/1000), # change to km 
                     data = sth, 
                     likelihood = "Binomial",
                     uvec = seq(0, 600, length = 15), 
                     n.sim = 10000,
                     which.test = "variogram",
                     lse.variogram = TRUE)

TT.sigma2.guess <- 0.9118511
TT.phi.guess <- 61.6129379
TT.tau2.guess <- 0.6349420

par0.TT <- c(TT.beta.guess, TT.sigma2.guess, TT.phi.guess, TT.tau2.guess)

# Fit geostat model 

geostat.fit.TT <- binomial.logistic.MCML(formula = TT_positive ~
                                           altitude + riv_dist + travel_w,
                                         units.m = ~TT_examined,
                                         coords = ~I(utm_x/1000)+
                                                   I(utm_y/1000), # change to km 
                                         data = sth,
                                         par0 = par0.TT,
                                         control.mcmc = mcml,
                                         kappa = 0.5,
                                         start.cov.pars = c(TT.phi.guess,
                                                            TT.tau2.guess/TT.sigma2.guess),
                                         method = "nlminb")

# predicted prevalence 

TT.pred.prev <- spatial.pred.binomial.MCML(geostat.fit.TT,
                                           grid.pred = ETH_grid[,1:2],
                                           predictors = ETH_grid[,3:8],
                                           control.mcmc = mcml,
                                           scale.predictions = c("logit",
                                                                 "prevalence"))

plot(TT.pred.prev, type = "prevalence", summary = "predictions")

# extract MCMC samples 

TT.pred.samples <- 1/(1+exp(-TT.pred.prev$samples))

# Mean predicted prevalence 

TT.prev.mean <- apply(TT.pred.samples, 1, mean)
ETH_grid$TT.prev.mean <- TT.prev.mean

# Mean anti-prevalence 

TT.antiprev.mean <- apply(TT.pred.samples, 1, function(x) mean(1-x))
ETH_grid$TT.antiprev.mean <- TT.antiprev.mean

# Raster of "AT LEAST ONE SPECIES" ----

# take the samples directly into this equation, not the mean
any.sth.prev.mean <- 1-(HK.antiprev.mean*Asc.antiprev.mean*TT.antiprev.mean)
ETH_grid$any.sth.prev.mean <- any.sth.prev.mean

any.sth.r <- rasterFromXYZ(cbind(ETH_grid[,1:2],
                                 (1-(HK.antiprev.mean*Asc.antiprev.mean*TT.antiprev.mean))))

plot(any.sth.r)

# calculate samples for AT LEAST ONE STH prevalence 
any.sth.samples <- 1-((1-HK.pred.samples)*(1-Asc.pred.samples)*(1-TT.pred.samples))

# probability of exceeding 50% prevalence 
any.sth.ex.50 <- apply(any.sth.samples, 1, function(x) mean(x>0.5))
ETH_grid$any.sth.exceed50 <- any.sth.ex.50

# probability of exceeding 20% prevalence 
any.sth.exceed20 <- apply(any.sth.samples, 1, function(x) mean(x>0.2))
ETH_grid$any.sth.exceed20 <- any.sth.exceed20

# probability of below 20% prev 
any.sth.below.20 <- apply(any.sth.samples, 1, function(x) mean(x<0.2))
ETH_grid$any.sth.below20 <- any.sth.below.20

# probability of being between 20% and 50% prevalence 
any.sth.between.20_50 <- apply(any.sth.samples, 1, function(x) mean(x>0.2 & x<0.5))
ETH_grid$any.sth.between.20_50 <- any.sth.between.20_50


# Store predictions in ETH_grid and save ----

write.csv(ETH_grid, file = "data/ETH_grid.csv", row.names = FALSE)

# ggplots ----

ggplot()+
  geom_raster(data=ETH_grid, aes(x=X, y=Y, fill=any.sth.exceed50))+
  geom_sf(data = ETH_adm1, col = "grey", fill = NA, size = 0.2)+
  geom_sf(data=ETH_adm0, col = "black", fill = NA)+
  scale_fill_scico(palette = "vik", limits = c(0, 1))+
  theme_void()

ggplot()+
  geom_raster(data=ETH_grid, aes(x=X, y=Y, fill=any.sth.between.20_50))+
  geom_sf(data = ETH_adm1, col = "grey", fill = NA, size = 0.2)+
  geom_sf(data=ETH_adm0, col = "black", fill = NA)+
  scale_fill_scico(palette = "vik", limits = c(0, 1))+
  theme_void()

# try creating discrete raster ----

ETH_grid$discrete.90 <- ifelse(ETH_grid$any.sth.exceed50>0.9, 1, 
                              ifelse(ETH_grid$any.sth.between.20_50>0.9, 2, 
                                     ifelse(ETH_grid$any.sth.below20>0.9, 3, 4)))

ETH_grid$discrete.90 <- factor(ETH_grid$discrete.90, levels = c(1:4), 
                               labels = c("Once a year", "Twice a year",
                                          "Case-by-case", "More data needed"))

ETH_grid$discrete.75 <- ifelse(ETH_grid$any.sth.exceed50>0.75, 1, 
                               ifelse(ETH_grid$any.sth.between.20_50>0.75, 2, 
                                      ifelse(ETH_grid$any.sth.below20>0.75, 3, 4)))

ETH_grid$discrete.75 <- factor(ETH_grid$discrete.75, levels = c(1:4), 
                               labels = c("Once a year", "Twice a year",
                                          "Case-by-case", "More data needed"))

ETH_grid$discrete.60 <- ifelse(ETH_grid$any.sth.exceed50>0.60, 1, 
                               ifelse(ETH_grid$any.sth.between.20_50>0.60, 2, 
                                      ifelse(ETH_grid$any.sth.below20>0.60, 3, 4)))

ETH_grid$discrete.60 <- factor(ETH_grid$discrete.60, levels = c(1:4), 
                               labels = c("Once a year", "Twice a year",
                                          "Case-by-case", "More data needed"))



ggplot()+
  geom_raster(data=ETH_grid, aes(x=X, y=Y, fill=discrete.90))+
  geom_sf(data = ETH_adm1, col = "grey", fill = NA, size = 0.2)+
  geom_sf(data=ETH_adm0, col = "black", fill = NA)+
  scale_fill_manual(values = c("red", "orange", "green", "grey"), drop = FALSE)+
  theme_void()

ggplot()+
  geom_raster(data=ETH_grid, aes(x=X, y=Y, fill=discrete.75))+
  geom_sf(data = ETH_adm1, col = "grey", fill = NA, size = 0.2)+
  geom_sf(data=ETH_adm0, col = "black", fill = NA)+
  scale_fill_manual(values = c("red", "orange", "green", "grey"), drop = FALSE)+
  theme_void()

ggplot()+
  geom_raster(data=ETH_grid, aes(x=X, y=Y, fill=discrete.60))+
  geom_sf(data = ETH_adm1, col = "grey", fill = NA, size = 0.2)+
  geom_sf(data=ETH_adm0, col = "black", fill = NA)+
  scale_fill_manual(values = c("red", "orange", "green", "grey"), drop = FALSE)+
  theme_void()
