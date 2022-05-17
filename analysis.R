# Load packages ----

library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(tmap)
library(lme4)
library(PrevMap)

# load clean sth data 

sth <- read.csv("data/ETH_sth.csv")
sth$Asc_examined <- as.numeric(sth$Asc_examined)

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
                        fric_w + I((fric_w-0.0175)*(fric_w>0.0175))+
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


