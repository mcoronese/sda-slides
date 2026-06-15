library(sf)
library(tidyverse)
library(magrittr)
#library(spData)



polygon_matrix <- cbind(
  x = c(0, 0, 1, 1,   0),
  y = c(0, 1, 1, 0.5, 0)
)
polygon_sfc <- st_sfc(st_polygon(list(polygon_matrix)))
#we could have created an sf object. This is just to show you that sf commants works also without attributes, as long as they have a geometry

point_df <- data.frame(
  x = c(0.2, 0.7, 0.4),
  y = c(0.1, 0.2, 0.8)
)
point_sf <- st_as_sf(point_df, coords = c("x", "y"))

int <- st_intersects(point_sf, polygon_sfc) 
int
class(int) #sgbp object
length(int) #1 list of lenght 3 (1 polygon, 3 points)
int[[1]] #tells you whether point 1 intersect with which polygon (in this case, nuumber 1)

int_nsp <- st_intersects(point_sf, polygon_sfc, sparse=FALSE)
class(int_nsp) #matrix object
dim(int_nsp)# 1 column, 3 rows (1 polygon, r poins)
int_nsp[2,1] #is point 2 intersecting with point number 1? 


st_within(point_sf, polygon_sfc, sparse=FALSE)
st_touches(point_sf, polygon_sfc, sparse=FALSE)
st_disjoint(point_sf, polygon_sfc, sparse=FALSE)

plot(polygon_sfc,
     axes = TRUE,
     col = "lightblue",
     border = "black",
     lwd = 2,
     main = "MULTIPOLYGON")

plot(point_sf,
     add = TRUE,
     pch = 1,
     col = "red",
     bg=NA,
     cex = 2,
     lwd = 2)

text(
  st_coordinates(point_sf)[,1],
  st_coordinates(point_sf)[,2],
  labels = 1:nrow(point_sf),
  pos = 3
)

#other predicates:



### a more concrete excersice

prov <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = 3,
  country = "IT",
  resolution = "01",
)


regions <- giscoR::gisco_get_nuts(
  year = "2021",
  nuts_level = 2,
  country = "IT",
  resolution = "01"
) %>%
  st_drop_geometry() %>%
  select(
    region_id = NUTS_ID,
    region_name = NUTS_NAME
  )

prov %<>%
  mutate(
    region_id = substr(NUTS_ID, 1, 4)
  ) %>% 
  left_join(.,
            regions,
            by="region_id")

prov %<>% select(NUTS_NAME, region_name)



data("world.cities", package = "maps")

cities <- world.cities %>% 
  filter(country.etc=="Italy")

cities <- st_as_sf(
  cities,
  coords = c("long", "lat"),
  crs = 4326
)

print(prov, n = 0)
print(cities, n = 0)

ct_int <- st_intersects(
  cities,
  prov
)

ct_int[[6]] #city number 6 is located in province 33
cities[6,]
prov[33,]
table(lengths(ct_int)) # each element contains at max 1 value... because a city can be only in one country
#but how is it possible that some cities do not have a province? 
missing <- cities[lengths(ct_int) == 0, ]
missing$name #resolution matters!
#we will have to rely on "distance" to fix this...
# later we will use st_nearest_feature() to fix this!

par(fig = c(0.0000001, 1, 0.0000001, 1))

plot(st_geometry(prov),
     axes = TRUE,
     col = "lightblue",
     border = "black",
     lwd = 0.2)

plot(st_geometry(missing),
     add = TRUE,
     pch = 1,
     col = "red",
     bg=NA,
     cex = 0.2,
     lwd = 1)

box()

#zoom
par(
  fig = c(0.1, 0.38, 0.02, 0.28),
  mar = c(0,0,0,0),
  new = TRUE
)



plot(
  st_geometry(prov %>% filter(NUTS_NAME=="Sassari")),
  col = "grey95",
  border = "grey70",
  #axes = TRUE,
  xlab = "",
  ylab = "",
  main = "",
  xlim = c(8.34, 8.36),
  ylim = c(40.53, 40.59)
)


plot(st_geometry(missing[2,]),
     add = TRUE,
     pch = 16,
     col = "blue",
     cex = 1.5)

text(
  x = 8.355,
  y = 40.58,
  labels = "Sassari",
  cex = 1.2,
  font = 1
)

text(
  x = 8.315,
  y = 40.56,
  labels = "Alghero",
  cex = 0.8,
  font = 1
)


box()


## Spatial joins

ct_int_sp <- st_intersects(
  cities,
  prov,
  sparse = FALSE
)

dim(ct_int_sp) #985 rows, one per each city
#107 columns, one per each province

st_drop_geometry(cities[6,])
st_drop_geometry(prov[which(ct_int_sp[6, ]),"NUTS_NAME"])

prov_id <- apply(ct_int_sp, 1, function(x){
  w <- which(x)
  if(length(w) == 1) w else NA
})


cities$province <- prov$NUTS_NAME[prov_id]

st_drop_geometry(cities[1:5,])

cities_prov <- st_join(
  cities,
  prov
)

st_drop_geometry(cities_prov[1:5,c("name","NUTS_NAME")])

## subsetting

pisa <- prov %>% filter(NUTS_NAME == "Pisa")
cities[pisa,]

st_drop_geometry(cities[pisa,][1:4,])



#### Distances
rome <- cities %>%
  filter(name == "Rome")

milan <- cities %>%
  filter(name == "Milan")

st_distance(
  rome,
  milan
)

sf_use_s2(FALSE)

st_distance(
  rome,
  milan
)

sf_use_s2(TRUE)

cities_32632 <- st_transform(
  cities,
  32632
)

st_distance(
  cities_32632[cities_32632$name=="Rome",],
  cities_32632[cities_32632$name=="Milan",]
)



d <- st_distance(
  cities[1:5, ]
)
rownames(d) <- cities$name[1:5]
colnames(d) <- cities$name[1:5]


d2 <- st_distance(
  cities[1:5, ],
  cities[6:9, ]
)
rownames(d2) <- cities$name[1:5]
colnames(d2) <- cities$name[6:9]



## points to polygon 

agropoli <- missing[1, ]

d <- st_distance(
  agropoli,
  prov
)

prov$NUTS_NAME[which.min(d)]


st_nearest_feature(
  missing,
  prov
)

#fix missing cities!
missing_prov <- st_join(
  missing,
  prov,
  join = st_nearest_feature
)

cities_prov_fixed <- bind_rows(
  filter(cities_prov,
         !is.na(NUTS_NAME)),
  missing_prov
)


#two polygons
pisa <- prov %>%
  filter(NUTS_NAME == "Pisa")

livorno <- prov %>%
  filter(NUTS_NAME == "Livorno")

st_distance(
  pisa,
  livorno
)


plot(st_geometry(pisa),
     axes = TRUE,
     xlim = c(9.5, 11.3),
     ylim = c(42.5, 43.9),
     col = "lightblue",
     border = "black",
     lwd = 0.2)

plot(st_geometry(livorno),
     axes = TRUE,
     col = "lightgreen",
     border = "black",
     lwd = 0.2,
     add=TRUE)


plot(st_centroid(st_geometry(pisa)),
     add = TRUE,
     pch = 1,
     cex = 0.2,
     lwd = 4)

plot(st_point_on_surface(st_geometry(livorno)),
     add = TRUE,
     pch = 1,
     cex = 0.2,
     lwd = 4)


prov %>% 
  mutate(
    area = st_area(geometry)
  ) 


rome_buffer <- st_buffer(
  st_transform(rome, 32632),
  50000
) %>% 
  mutate(is_close_rome = TRUE)

cities_utm <- st_transform(
  cities,
  32632
)

st_join(
  cities_utm,
  rome_buffer %>% select(is_close_rome),
  join = st_intersects
) %>% filter(is_close_rome)

st_is_within_distance(cities_utm, st_transform(rome, 32632), dist = units::set_units(50000, "m"), sparse=FALSE)


#attribute aggregation
cities_prov_fixed #this dataset has a geometry which is a point. Hard to merge points... we need contigous polygons!


cities_prov_new <- cities_prov_fixed %>%
  st_drop_geometry() %>%
  left_join(
    prov %>% select(NUTS_NAME, geometry),
    by = "NUTS_NAME"
  ) %>%
  st_as_sf()

plot(cities_prov_new[,"pop"])

prov_pop <- cities_prov_new %>%
  group_by(NUTS_NAME) %>%
  summarise(
    pop = sum(pop),
    region_name= first(region_name)
  ) 

head(prov_pop,2)

plot(prov_pop[,"pop"])

reg_pop <- prov_pop %>%
  group_by(region_name) %>%
  summarise(
    pop = sum(pop)
  ) 

plot(reg_pop[,"pop"])



## Raster-vector interaction: raster extraction 
library(terra)
tavg <- geodata::worldclim_global(
  var = "tavg",
  res = 5,
  path = tempdir()
)

plot(tavg[[1]])

italy <- prov %>% 
  summarise()

tavg_crop <- crop(tavg, italy)

tavg_it <- mask(tavg_crop, italy)

plot(tavg_it[[1]])


#By default, terra uses the cell center. More sophisticated weighted approaches exist and are useful when raster resolution is coarse.

temp_prov <- terra::extract(
  tavg_it,
  prov,
  fun = mean,
  na.rm = TRUE
)

prov <- cbind(prov, temp_prov[,-1])

prov <- prov %>% 
  rename_with(
    ~ paste0("temp_", tolower(month.abb)),
    starts_with("wc2.1_5m_tavg_")
  )

#Which one do you think will show a stronger North-South gradient?

prov %>%
  pivot_longer(
    cols = starts_with("temp_"),
    names_to = "month",
    values_to = "temperature"
  ) %>% 
  mutate(
    month = factor(
      month,
      levels = paste0(
        "temp_",
        c("jan","feb","mar","apr","may","jun",
          "jul","aug","sep","oct","nov","dec")
      )
    )
  ) %>% 
  ggplot() + 
  geom_sf(aes(fill=temperature))+ 
  facet_wrap(month~., nrow=3) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 10,
    na.value = "aliceblue",
    name = "Temperature (°C)"
  ) + 
  theme_void() +
  theme(
    legend.position = "bottom"
  )

#Raster extraction can be interpreted as a form of zonal aggregation. The zones are defined by vector geometries rather than by another raster.

exactextractr::exact_extract(
  tavg_it,
  prov,
  fun="mean"
)
