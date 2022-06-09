# Load packages ----

library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(tmap)

# Load data ----

##├ STH data ----
sth <- read.csv("raw_data/data-ET-STH-sitelevel.csv")

str(sth)

sth_1 <- sth %>% 
  filter(Georeliability %in% c(1,2,3)&
           Quality %in% c(1,2))

# set as sf data 

sth_sf <- st_as_sf(sth_1, coords = c("Longitude", "Latitude"))
st_crs(sth_sf) <- 4326

sth_sf <- st_transform(sth_sf, crs = 32638)

sth$utm_x <- as.data.frame(st_coordinates(sth_sf))$X
sth$utm_y <- as.data.frame(st_coordinates(sth_sf))$Y

##├ ADM data ----

ETH_adm0 <- st_read("ETH_files/ETH_adm/ETH_adm0.shp")
ETH_adm0 <- st_transform(ETH_adm0, crs = 32638)

ETH_adm1 <- st_read("ETH_files/ETH_adm/ETH_adm1.shp")
ETH_adm1 <- st_transform(ETH_adm1, crs = 32638)

ETH_adm2 <- st_read("ETH_files/ETH_adm/ETH_adm2.shp")
ETH_adm2 <- st_transform(ETH_adm2, crs = 32638)

# plot boundaries with site level points 

ggplot()+
  geom_sf(data = ETH_adm1, fill = NA, col = "grey")+
  geom_sf(data = ETH_adm0, fill = NA, col = "black")+
  geom_sf(data = sth_sf, size = 0.5, aes(col = as.factor(Year)))+
  coord_sf()+
  theme_void()

## ├ waterways shapefile ----

ETH_riv <- st_read("ETH_files/ETH_wat/ETH_water_lines_dcw.shp")
ETH_riv <- st_transform(ETH_riv, crs = 32638)

# ETH_lakes <- st_read("ETH_files/ETH_wat/ETH_water_areas_dcw.shp")
# ETH_lakes <- st_transform(ETH_lakes, crs = 32638)

ggplot()+
  geom_sf(data = ETH_adm1, fill = NA, col = "grey")+
  geom_sf(data = ETH_adm0, fill = NA, col = "black")+
  geom_sf(data = ETH_riv, col = "blue")+
  geom_sf(data = sth_sf, col = "red", size = 0.5)+
  coord_sf()+
  theme_void()

# calculate minimum distance to river in km 

sth_1$riv_dist <- apply(st_distance(sth_sf, ETH_riv), 1, min)/1000

## ├ raster data ----

# Altitude
ETH_alt <- raster("ETH_files/ETH_msk_alt/ETH_msk_alt.gri")
ETH_alt <- projectRaster(ETH_alt, crs = 32638)
ETH_alt.df <- as.data.frame(ETH_alt, xy = TRUE)
names(ETH_alt.df)[3] <- "Altitude"
ETH_alt.df <- ETH_alt.df[complete.cases(ETH_alt.df),]

# Land cover (from 2000, so not very useful)
#ETH_cov <- raster("ETH_files/ETH_msk_cov/ETH_msk_cov.gri")
#ETH_cov <- projectRaster(ETH_cov, crs = 32638)
#ETH_cov.df <- as.data.frame(ETH_cov, xy=TRUE)
#ETH_cov.df <- ETH_cov.df[complete.cases(ETH_cov.df),]
#names(ETH_cov.df)[3] <- "Land_cover"

# friction
ETH_fric_w <- raster("ETH_files/2020_walking_only_friction_surface_ETH.tif")
ETH_fric_w <- projectRaster(ETH_fric_w, crs = 32638)
ETH_fric_w.df <- as.data.frame(ETH_fric_w, xy=TRUE)
ETH_fric_w.df <- ETH_fric_w.df[complete.cases(ETH_fric_w.df),]
names(ETH_fric_w.df)[3] <- "Friction_surface"

ETH_fric_m <- raster("ETH_files/2020_motorized_friction_surface_ETH.tif")
ETH_fric_m <- projectRaster(ETH_fric_m, crs = 32638)
ETH_fric_m.df <- as.data.frame(ETH_fric_m, xy=TRUE)
ETH_fric_m.df <- ETH_fric_m.df[complete.cases(ETH_fric_m.df),]
names(ETH_fric_m.df)[3] <- "Friction_surface"

# travel to health center 
ETH_travel_w <- raster("ETH_files/2020_walking_only_travel_time_to_healthcare_ETH.tif")
ETH_travel_w <- projectRaster(ETH_travel_w, crs = 32638)
ETH_travel_w.df <- as.data.frame(ETH_travel_w, xy=TRUE)
ETH_travel_w.df <- ETH_travel_w.df[complete.cases(ETH_travel_w.df),]
names(ETH_travel_w.df)[3] <- "Travel_time"

ETH_travel_m <- raster("ETH_files/2020_motorized_travel_time_to_healthcare_ETH.tif")
ETH_travel_m <- projectRaster(ETH_travel_m, crs = 32638)
ETH_travel_m.df <- as.data.frame(ETH_travel_m, xy=TRUE)
ETH_travel_m.df <- ETH_travel_m.df[complete.cases(ETH_travel_m.df),]
names(ETH_travel_m.df)[3] <- "Travel_time"

### plot rasters ----

ggplot()+
  geom_raster(data = ETH_fric_m.df, aes(x = x, y = y, fill = Friction_surface))+
  geom_sf(data = ETH_adm1, fill = NA, col = "grey")+
  geom_sf(data = ETH_adm0, fill = NA, col = "black")+
  #scale_fill_gradient(low = )+
  theme_void()

tm_shape(ETH_alt)+
  tm_raster(title = "Altitude")+
  tm_shape(ETH_adm1)+
  tm_borders(col = "grey")+
  tm_shape(ETH_adm0)+
  tm_borders(col= "black")

#tm_shape(ETH_cov)+
#  tm_raster(title = "Land Cover")+
#  tm_shape(ETH_adm1)+
#  tm_borders(col = "grey")+
#  tm_shape(ETH_adm0)+
#  tm_borders(col= "black")

tm_shape(ETH_travel_w)+
  tm_raster(title = "Travel to healthcare - walking")+
  tm_shape(ETH_adm1)+
  tm_borders(col = "grey")+
  tm_shape(ETH_adm0)+
  tm_borders(col= "black")

# Extract raster data to STH points ----

sth_1$altitude <- terra::extract(ETH_alt, sth_sf)

sth_1$fric_w <- terra::extract(ETH_fric_w, sth_sf)
sth_1$fric_m <- terra::extract(ETH_fric_m, sth_sf)

sth_1$travel_w <- terra::extract(ETH_travel_w, sth_sf)
sth_1$travel_m <- terra::extract(ETH_travel_m, sth_sf)

sth_1 <- sth_1 %>% 
  dplyr::select(ADMIN1_NAME, ADMIN1_CODE, ADMIN2_NAME, ADMIN2_CODE,
         IU_NAME, IU_ID, Location, Longitude, Latitude, Georeliability,
         Year, HK_examined, HK_positive, Asc_examined, Asc_positive,
         TT_examined, TT_positive, 
         altitude, riv_dist, fric_w, fric_m, travel_w, travel_m)



write.csv(sth_1, file = "data/ETH_sth.csv", row.names = FALSE)

# ETH grid for prediction ----

ETH_grid.sf <- st_make_grid(ETH_adm0,
                            cellsize = 10000, # 10km resolution 
                            what = "centers")

ETH_grid <- as.data.frame(st_coordinates(ETH_grid.sf))

ETH_grid$altitude <- terra::extract(ETH_alt, ETH_grid[,1:2])

ETH_grid$fric_w <- terra::extract(ETH_fric_w, ETH_grid[,1:2])
ETH_grid$fric_m <- terra::extract(ETH_fric_m, ETH_grid[,1:2])

ETH_grid$travel_w <- terra::extract(ETH_travel_w, ETH_grid[,1:2])
ETH_grid$travel_m <- terra::extract(ETH_travel_m, ETH_grid[,1:2])

ETH_grid <- ETH_grid[complete.cases(ETH_grid),]

ETH_grid.2 <- st_as_sf(ETH_grid, coords = c("X", "Y"))
st_crs(ETH_grid.2) <- 32638

ETH_grid$riv_dist <- apply(st_distance(ETH_grid.2, ETH_riv), 1, min)/1000

write.csv(ETH_grid, file = "data/ETH_grid.csv", row.names = FALSE)

# messy ----

sth_1 %>% 
  count(Year)

sth_1 %>% 
  filter(Year != 0 & Year != "null") %>% 
  count(Year)

sth %>% 
  count(Quality, Year)
# Quality: 1 = good quality, 2 = middle quality, 3 = poor quality

sth %>% 
  count(Year)
