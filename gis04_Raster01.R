if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars)

##Read/Export---------
(spr_ex <- rast("data/spr_example.tif"))



writeRaster(x = spr_ex, 
            filename = "data/spr_elev.tif",
            overwrite = TRUE)

##Mapping--------
ggplot() +
  geom_spatraster(data = spr_ex)
#Convert to Stars---
star_ex <- st_as_stars(spr_ex)
mapview(star_ex)


##Data Types: Continous vs. Discrete-------


##Continuous 
v_elev <- values(spr_ex)
head(v_elev)

extract(spr_ex, y = cbind(6.0000, 50.0000))

### Note that argument y must be supplied as a matrix 
### or a dataframe. If you wish to extract at multiple 
## locations, you may supply a dataframe for the second argument.

(df_point <- tibble(lon = c(6, 5.9), lat = c(50, 49.96)))


extract(spr_ex, y = df_point)


## Discrete: Binary & Code Value 

## load forest raster
(spr_for <- rast("data/spr_forest_nc.tif"))

ggplot() +
  geom_spatraster(data = spr_for)
unique(spr_for)

v_binary <- values(spr_for)
(p_forest <- mean(v_binary))


## Code Value 
(spr_land <- rast("data/spr_land_reclass.tif"))
unique(spr_land)


extract(spr_land, cbind(-79.8063, 36.0701))



##Reclass
# write a conversion matrix
# left, original value
# right, value after conversion
(cm <- cbind(c(0, 1001, 1010, 1100),
             c(0, 1, 0, 0)))

spr_bin <- classify(spr_land,
                    rcl = cm)

v_bin <- values(spr_bin)
mean(v_bin)


##Exercise-----------------

#1. 
spr_prec_ncne <- terra::rast("data/spr_prec_ncne.tif")

#2. 

print(spr_prec_ncne)


nrow(spr_prec_ncne)
ncol(spr_prec_ncne)


terra::res(spr_prec_ncne)


terra::ext(spr_prec_ncne)


terra::crs(spr_prec_ncne, describe = TRUE)


terra::global(spr_prec_ncne, fun = "min", na.rm = TRUE)
terra::global(spr_prec_ncne, fun = "max", na.rm = TRUE)

#3.-------

ggplot() +
  tidyterra::geom_spatraster(data = spr_prec_ncne) +
  labs(
    title = "Precipitation in Northeastern North Carolina",
    x = "Longitude",
    y = "Latitude",
    fill = "Precipitation"
  ) +
  theme_minimal()

#4. 

spr_land <- terra::rast("data/spr_land_reclass.tif")
sf_site <- readRDS("data/sf_finsync_nc.rds")

df_xy <- as.data.frame(sf::st_coordinates(sf_site))

df_land <- terra::extract(spr_land, df_xy)

head(df_land)

land_counts <- sort(table(df_land[[1]]), decreasing = TRUE)
land_counts


most_common <- land_counts[land_counts == max(land_counts)]
most_common

#5-----

cm <- cbind(
  c(0, 1001, 1010, 1100),
  c(0,    0,    0,    1)
)

spr_urban <- terra::classify(spr_land, rcl = cm)

unique(spr_urban)
p_urban <- mean(terra::values(spr_urban), na.rm = TRUE)
p_urban
percent_urban <- p_urban * 100
percent_urban


##Data Manipulation 

##Crop ___
(spr_prec <- rast("data/spr_prec_us.tif"))
ext(spr_prec)


## crop to:
## longitude range: -80 to -75
## latitude range: 34 to 37
spr_prec_crop <- crop(x = spr_prec,
                      y = c(-80, -75, 34, 37))


## load county vector 
sf_nc_county <- readRDS("data/sf_nc_county.rds")

ggplot() +
  geom_spatraster(data = spr_prec_crop) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25) ## alpha = 0.25 makes the polygon layer transparent



##Vector Extent 
spr_prec_nc <- crop(x = spr_prec,
                    y = sf_nc_county)
ggplot() +
  geom_spatraster(data = spr_prec_nc) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25) ## alpha = 0.25 makes the polygon layer transparent


##Merge------

##Two Tiles
spr_nw <- rast("data/spr_prec_ncnw.tif") # Northwest NC
spr_ne <- rast("data/spr_prec_ncne.tif") # Northeast NC
spr_sw <- rast("data/spr_prec_ncsw.tif") # Southwest NC
spr_se <- rast("data/spr_prec_ncse.tif") # Southeast NC


ggplot() +
  geom_spatraster(data = spr_nw) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)
spr_n <- merge(spr_nw, spr_ne)

ggplot() +
  geom_spatraster(data = spr_n) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)
##More Than Two Tiles 
list_spr <- list(spr_nw,
                 spr_ne,
                 spr_sw,
                 spr_se)
spr_col <- sprc(list_spr)
spr_merge <- merge(spr_col)
ggplot() +
  geom_spatraster(data = spr_merge) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

writeRaster(spr_merge, 
            filename = "data/spr_prec_nc.tif",
            overwrite = TRUE)


##Stack-------
spr_prec_nc <- rast("data/spr_prec_nc.tif")
spr_tmp_nc <- rast("data/spr_tmp_nc.tif")

# print precipitation layer - do the same for spr_tmp_nc as well!
print(spr_prec_nc)

(spr_pt_nc <- c(spr_prec_nc,
                spr_tmp_nc))

# precipitation
spr_pt_nc$precipitation


##Reprojection-----
print(spr_prec_nc)
(spr_prec_nc_proj <- project(x = spr_prec_nc,
                             y = "EPSG:32617"))

###Exercise2-------


library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyterra)


# 1. Merge-------------------------

spr_tmp_nw <- terra::rast("data/spr_tmp_ncnw.tif")
spr_tmp_ne <- terra::rast("data/spr_tmp_ncne.tif")
spr_tmp_sw <- terra::rast("data/spr_tmp_ncsw.tif")
spr_tmp_se <- terra::rast("data/spr_tmp_ncse.tif")


list_spr <- list(
  spr_tmp_nw,
  spr_tmp_ne,
  spr_tmp_sw,
  spr_tmp_se
)


spr_col <- terra::sprc(list_spr)


spr_merge <- terra::merge(spr_col)


sf_nc_county <- readRDS("data/sf_nc_county.rds")


sf_nc_county <- sf::st_transform(
  sf_nc_county,
  crs = terra::crs(spr_merge)
)


ggplot() +
  geom_spatraster(data = spr_merge) +
  geom_sf(data = sf_nc_county, alpha = 0.25) +
  labs(
    title = "Merged North Carolina Temperature Raster",
    fill = "Temperature"
  ) +
  theme_minimal()


# 2. Crop ---------------------------


sf_camden <- sf_nc_county |>
  filter(county == "camden")


terra::ext(sf_camden)

spr_tmp_camden <- terra::crop(
  x = spr_merge,
  y = sf_camden
)

ggplot() +
  geom_spatraster(data = spr_tmp_camden) +
  geom_sf(data = sf_nc_county, alpha = 0.25) +
  labs(
    title = "Temperature Raster Cropped to Camden County's Extent",
    fill = "Temperature"
  ) +
  theme_minimal()


# 3. Reproject  --------------------------------

spr_tmp_camden_proj <- terra::project(
  x = spr_tmp_camden,
  y = "EPSG:32618",
  method = "bilinear"
)

print(spr_tmp_camden_proj)

terra::crs(spr_tmp_camden_proj, describe = TRUE)
terra::res(spr_tmp_camden_proj)
