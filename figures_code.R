library(dplyr)
library(ggplot2)
library(cowplot)
library(sf)
library(raster)
library(tmap)
library(lme4)
library(PrevMap)
library(scico)

sth <- read.csv("data/ETH_sth.csv")
sth$Asc_examined <- as.numeric(sth$Asc_examined)
sth_sf <- st_as_sf(sth, coords = c("utm_x", "utm_y"))
st_crs(sth_sf) <- 32638

ETH_grid <- read.csv("data/ETH_grid.csv")

ETH_adm0 <- st_read("ETH_files/ETH_adm/ETH_adm0.shp")
ETH_adm0 <- st_transform(ETH_adm0, crs = 32638)

ETH_adm1 <- st_read("ETH_files/ETH_adm/ETH_adm1.shp")
ETH_adm1 <- st_transform(ETH_adm1, crs = 32638)

r1 <- ggplot()+
  geom_raster(data = ETH_grid, aes(x=X, y=Y, fill = altitude))+
  geom_sf(data = ETH_adm1, col = "grey80", fill = NA, size = 0.15)+
  geom_sf(data = ETH_adm0, col = "black", fill = NA)+
  geom_sf(data = sth_sf, col = "white", size = 0.15)+
  scale_fill_scico(palette = "bamako", name = "Altitude (m)")+
  theme_void()+
  theme(legend.text = element_text(size = 5),
        legend.title = element_text(size = 7))

r2 <- ggplot()+
  geom_raster(data = ETH_grid, aes(x=X, y=Y, fill = riv_dist))+
  geom_sf(data = ETH_adm1, col = "grey80", fill = NA, size = 0.15)+
  geom_sf(data = ETH_adm0, col = "black", fill = NA)+
  geom_sf(data = sth_sf, col = "white", size = 0.15)+
  scale_fill_scico(palette = "davos", name = "Distance (km)")+
  theme_void()+
  theme(legend.text = element_text(size = 5),
        legend.title = element_text(size = 7))

r3 <- ggplot()+
  geom_raster(data = ETH_grid, aes(x=X, y=Y, fill = fric_w))+
  geom_sf(data = ETH_adm1, col = "grey80", fill = NA, size = 0.15)+
  geom_sf(data = ETH_adm0, col = "black", fill = NA)+
  geom_sf(data = sth_sf, col = "white", size = 0.15)+
  scale_fill_scico(palette = "acton", name = "Minutes")+
  theme_void()+
  theme(legend.text = element_text(size = 5),
        legend.title = element_text(size = 7))

r4 <- ggplot()+
  geom_raster(data = ETH_grid, aes(x=X, y=Y, fill = fric_m))+
  geom_sf(data = ETH_adm1, col = "grey80", fill = NA, size = 0.15)+
  geom_sf(data = ETH_adm0, col = "black", fill = NA)+
  geom_sf(data = sth_sf, col = "white", size = 0.15)+
  scale_fill_scico(palette = "acton", name = "Minutes")+
  theme_void()+
  theme(legend.text = element_text(size = 5),
        legend.title = element_text(size = 7))

r5 <- ggplot()+
  geom_raster(data = ETH_grid, aes(x=X, y=Y, fill = travel_w))+
  geom_sf(data = ETH_adm1, col = "grey80", fill = NA, size = 0.15)+
  geom_sf(data = ETH_adm0, col = "black", fill = NA)+
  geom_sf(data = sth_sf, col = "white", size = 0.15)+
  scale_fill_scico(palette = "oslo", name = "Minutes")+
  theme_void()+
  theme(legend.text = element_text(size = 5),
        legend.title = element_text(size = 7))

r6 <- ggplot()+
  geom_raster(data = ETH_grid, aes(x=X, y=Y, fill = travel_m))+
  geom_sf(data = ETH_adm1, col = "grey80", fill = NA, size = 0.15)+
  geom_sf(data = ETH_adm0, col = "black", fill = NA)+
  geom_sf(data = sth_sf, col = "white", size = 0.15)+
  scale_fill_scico(palette = "oslo", name = "Minutes")+
  theme_void()+
  theme(legend.text = element_text(size = 5),
        legend.title = element_text(size = 7))

plot_grid(r1, r3, r5, r2, r4, r6,
          labels = c("A", "C", "E", "B", "D", "F"),
          align = "hv")

ggplot()+
  geom_sf(data = ETH_adm1, col = "grey60", fill = NA, size = 0.15)+
  geom_sf(data = ETH_adm0, col = "black", fill = NA)+
  geom_sf(data=sth_sf, col = "red")+
  theme_void()

sth %>% 
  count(Year)


sth$HK_e.logit <- log((sth$HK_positive+0.5)/(sth$HK_examined-sth$HK_positive+0.5))
sth$Asc_e.logit <- log((sth$Asc_positive+0.5)/(sth$Asc_examined-sth$Asc_positive+0.5))
sth$TT_e.logit <- log((sth$TT_positive+0.5)/(sth$TT_examined-sth$TT_positive+0.5))

species <- c("HK", "Asc", "TT")
vars <- names(sth)[c(18:23)]
vars_labels <- c("Altitude (m)", "Distance (km)", "Time (min)",
                 "Time (min)",
                 "Time (min)",
                 "Time (min)")

plot.list <- list()

for (i in 1:length(species)) {
  
  for (j in 1:length(vars)) {
    
    plot.list[[species[i]]][[vars[j]]] <-
      ggplot(sth, aes_string(x = vars[j],
                             y = paste0(species[i], "_e.logit")))+
      geom_point()+
      labs(x = vars_labels[j], y = "Empirical logit")
    
  }
  
}

plot.list$HK
