#Vector 2: Spatial JOIN

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview)

#erase all objects in the enviromernt
rm(list = ls())

sf_site <- readRDS("data/sf_finsync_nc.rds")
sf_nc_county <- readRDS("data/sf_nc_county.rds")

mapview(sf_nc_county, legend = FALSE) + mapview(sf_site, legend= FALSE)



# Join county info to sf_site
sf_site_join<- st_join(x = sf_site,
        y = sf_nc_county)
print(sf_site)
print(sf_site_join)

#Count number fo fish survey sites in guilford county
sf_site_guilford <- sf_site_join %>% 
  filter(county == "guilford")
mapview(sf_site_guilford)

#Number of sites Clay County 
sf_site_clay <- sf_site_join %>% 
  filter(county =="clay")

#re-read stream layer
(sf_str <- readRDS("data/sf_stream_gi.rds"))


#Produce a map with guilford county poly, sites with guilford
# and stream lines in guilford 
# Use ggplot() mapping functions

sf_nc_guilford<- sf_nc_county %>% 
  filter(county == "guilford")

ggplot() +
  geom_sf(data = sf_nc_guilford,
          color = "yellow") +
  geom_sf(data = sf_str,
          color = "blue") +
  geom_sf(data = sf_site_guilford,
          color = "red") +
  theme_bw()


# Geometric Analysis ----------------------

# Length 
(sf_str_proj <- st_transform(sf_str, crs = 32617))

#calculate length of each line segment
v_str_l <- st_length(sf_str_proj)
head(v_str_l)

sf_str_w_len <- sf_str %>% 
    mutate(length = v_str_l)

# area #
sf_nc_county_proj <- st_transform(sf_nc_county, crs = 32617)

v_area<- st_area(sf_nc_county_proj)

sf_nc_county_w_area <- sf_nc_county_proj %>% 
    mutate(area = as.numeric(v_area)/ 1E+6) %>% 
  arrange(desc(area))

sf_nc_county1K<- sf_nc_county_w_area %>% 
    filter(area > 1000)

ggplot()+
  geom_sf(data = sf_nc_county1K)



## Exercise
1
sf_quakes<-readRDS("data/sf_quakes.rds")
sf_nz<-readRDS("data/sf_nz.rds")

mapview(sf_nz)+ mapview(sf_quakes)
sf_quakes_join<-st_join(x=sf_quakes,
        y= sf_nz)

sf_quakes_nz<- drop_na(sf_quakes_join, fid)

nrow(sf_quakes_nz)
#3

2
sf_site_join<- as_tibble(sf_site_join) 
df_n <- sf_site_join %>% 
  group_by(county)
df_n <- sf_site_join %>% 
  group_by(county) %>% 
  summarise(n=n())

3
sf_n_site<-left_join(x=sf_nc_county, 
                     y= df_n, by = "county")

sf_n10<-sf_n_site %>% 
  filter(n>10)

4
ggplot()+
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_n_site %>% 
            filter(n>1),
            fill = "grey")+
  geom_sf(data = sf_n10,
          fill = "salmon") 
  
