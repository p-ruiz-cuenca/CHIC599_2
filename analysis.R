# Load packages ----

library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(tmap)
library(lme4)

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
      geom_smooth(method = lm)
    
  }
  
}

plot.list$HK
plot.list$HK$travel_w+scale_x_continuous(trans = "log")

plot.list$Asc

plot.list$TT
