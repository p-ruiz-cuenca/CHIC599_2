# Load packages ----

library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(tmap)
library(lme4)
library(PrevMap)

# load clean sth and grid data 

sth <- read.csv("data/ETH_sth.csv")
sth$Asc_examined <- as.numeric(sth$Asc_examined)

ETH_grid <- read.csv("data/ETH_grid.csv")

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

HK.pred.prev <- spatial.pred.binomial.MCML(geostat.fit.HK,
                                           grid.pred = ETH_grid,
                                           predictors = ETH_grid[,3:8],
                                           control.mcmc = mcml,
                                           scale.predictions = c("logit", 
                                                                 "prevalence"))
plot(HK.pred.prev, "prevalence", "predictions")
