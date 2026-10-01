if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview)

library(sf)

(sf_nc_county <- st_read(dsn = "data/nc.shp",
                         quiet = TRUE))

 
shapefile<- st_write(sf_nc_county, 
         dsn = "data/sf_nc_county.shp",
         append = FALSE)


Geopack<-st_write(sf_nc_county, 
         dsn = "data/sf_nc_county.gpkg",
         append = FALSE)


saveRDS(sf_nc_county,
        file = "data/sf_nc_county.rds")

sf_nc_county<- readRDS(file = "data/sf_nc_county.rds")


## Points 

(sf_site <- readRDS("data/sf_finsync_nc.rds"))

mapview(sf_site,
        col.regions = "black", 
        legend = FALSE) 
##first ten sites / Visualize 
(sf_site_f10 <- sf_site %>% 
    slice(1:10))
mapview(sf_site_f10,
        col.regions = "black",
        legend = FALSE)


##LINE

(sf_str <- readRDS("data/sf_stream_gi.rds"))
mapview(sf_str,
        color = "steelblue", # line's color
        legend = FALSE) # disable legend


## first 10 lines / Visual
(sf_str_f10 <- sf_str %>% 
    slice(1:10))
mapview(sf_str_f10,
        color = "steelblue", # line's color
        legend = FALSE) # disable legend


## Polygon 
(sf_nc_county <- readRDS("data/sf_nc_county.rds"))
mapview(sf_nc_county,
        col.regions = "grey", 
        legend = FALSE) 


##Slice / Filter Visual
(sf_nc_gi <- sf_nc_county %>% 
    filter(county == "guilford"))
mapview(sf_nc_gi,
        col.regions = "grey", 
        legend = FALSE) 


## Mapping Vector Data
ggplot() +
  geom_sf(data = sf_nc_county)+
  geom_sf(data = sf_str)+
  geom_sf(data = sf_site)
sf_site

## Exercise 
sf_str_as <- readRDS("data/sf_stream_as.rds")
2.
(sf_str_as)
(sf_nc_county)

st_crs(sf_str_as)==st_crs(sf_nc_county)

3.
ggplot() +
  geom_sf(data = sf_nc_county)+
  geom_sf(data = sf_str_as)

4.
sf_nc_as <- sf_nc_county %>% 
  filter(county == "Ashe")
ggplot()+
  geom_sf(data = sf_nc_as)+
  geom_sf(data = sf_str_as)
