library(sf)
library(tidyverse)
library(magrittr)
library(gstat)


boston.tr <- st_read(
  system.file(
    "shapes/boston_tracts.gpkg",
    package = "spData"
  ),
  quiet = TRUE
)

boston_utm <- boston.tr %>% st_transform(., 32619) #utm19N
boston <- st_transform(boston_utm, 4326)
plot(st_geometry(boston))

plot(boston_utm[,"MEDV"])

boston_utm_pts <- st_centroid(boston_utm)

vg <- variogram(MEDV ~ 1, 
                data = boston_utm_pts, 
                cutoff = 6000,
                width = 500)


plot(vg)



m <- lm(
  MEDV ~ CRIM + RM + NOX + LSTAT,
  data = boston_utm_pts
)

summary(m)


boston_utm$resid <- resid(m)

vg_res <- variogram(
  resid ~ 1,
  data = boston_utm,
  cutoff = 6000,
  width = 500
)

plot(vg_res)


#conley

coords <- st_coordinates(st_centroid(boston))

boston$lon <- coords[,1]
boston$lat <- coords[,2]


library(fixest)

mf <- feols(
  MEDV ~ CRIM + RM + NOX + LSTAT,
  data = boston
)

summary(m)
summary(mf)

summary(
  mf,
  vcov = vcov_conley(cutoff = 2.5,
                     lat = "lat",
                     lon = "lon")
)


#contiguity 
library(spdep)

#this creat the "graph", not yet the matrix
nb_q <- poly2nb(
  boston_utm,
  queen = TRUE
)

class(nb_q)

nb_q[[1]]
nb_q[[2]]
card(nb_q) #how many neighbors per each polygon


i <- 200

plot(
  st_geometry(boston_utm),
  border = "grey80",
  col = "white"
)

plot(
  st_geometry(boston_utm[499, ]),
  add = TRUE,
  col = "red"
)

plot(
  st_geometry(boston_utm[nb_q[[499]], ]),
  add = TRUE,
  col = "gold"
)

#distance based neighbors

nb_d <- dnearneigh(
  boston_utm_pts,
  0,
  10000
)

nb_d[[499]]
nb_d[[500]]

i <- 499

plot(
  st_geometry(boston_utm),
  border = "grey80",
  col = "white"
)

plot(
  st_geometry(boston_utm[i, ]),
  add = TRUE,
  col = "red"
)

plot(
  st_geometry(
    boston_utm[nb_d[[i]], ]
  ),
  add = TRUE,
  col = "gold"
)

#k-nearest neighbots

knn <- knearneigh(
  boston_utm_pts,
  k = 4
)

#with kneares, you need to explicitely convert the output to an nb object
nb_knn <- knn2nb(knn)

nb_knn[[499]]
nb_knn[[495]]


i <- 499

plot(
  st_geometry(boston_utm),
  border = "grey80",
  col = "white"
)

plot(
  st_geometry(boston_utm[i, ]),
  add = TRUE,
  col = "red"
)

plot(
  st_geometry(
    boston_utm[nb_knn[[i]], ]
  ),
  add = TRUE,
  col = "gold"
)

#create weighting matrixes


# Binary weights
lw_B <- nb2listw(
  nb_q,
  style = "B"
)

class(lw_B) #sparse representation of matrix (list)

nb2mat(nb_q, style = "B")[1:5,1:5] #matrix representation

# Row-standardized weights
lw_W <- nb2listw(
  nb_q,
  style = "W"
)

nb2mat(nb_q, style = "W")[1:5,1:5]

# Inverse-distance weights
d <- nbdists( #compute actual distances between neighbors (based on distance)
  nb_d,
  boston_utm_pts
)

nb_d[[499]]
d[[499]]

idw <- lapply( #compute inverse distance
  d,
  function(x) 1/x
)

lw_id <- nb2listw( #create the sparse matrix with explicit weights
  nb_d,
  glist = idw,
  style = "B"
)

lw_id$weights[[499]]
1/d[[499]]

lag_y <- lag.listw(
  lw_W,
  boston_utm$MEDV
)

head(lag_y)


#moran

moran.test(
  boston_utm$MEDV,
  lw_W
)

moran.test(
  resid(m),
  lw_W
)


z <- scale(boston_utm$MEDV)[,1]
moran.plot(
  z,
  lw_W,
  labels = FALSE,
  pch = 16,
  col = "grey40"
)

#With row-standardized weights, Moran's I coincide with the slope of the regression Wz ~ z.


# LISA

lisa <- localmoran(
  boston_utm$MEDV,
  lw_W
)

wz <- lag.listw(
  lw_W,
  z
)

quad <- rep(
  "Not significant",
  nrow(boston_utm)
)

sig <- lisa[,5] < 0.05

quad[
  z > 0 & wz > 0 & sig
] <- "HH"

quad[
  z < 0 & wz < 0 & sig
] <- "LL"

quad[
  z > 0 & wz < 0 & sig
] <- "HL"

quad[
  z < 0 & wz > 0 & sig
] <- "LH"

boston_utm$quad <- factor(
  quad,
  levels = c(
    "HH",
    "LL",
    "HL",
    "LH",
    "Not significant"
  )
)




ggplot(boston_utm) +
  geom_sf(
    aes(fill = quad),
    color = "white",
    linewidth = 0.1
  ) +
  scale_fill_manual(
    values = c(
      "HH" = "red",
      "LL" = "blue",
      "HL" = "orange",
      "LH" = "lightblue",
      "Not significant" = "grey90"
    )
  ) +
  theme_void() +
  labs(fill = "LISA")



#SLX

boston_utm$W_CRIM <-
  lag.listw(
    lw_W,
    boston_utm$CRIM
  )

boston_utm$W_LSTAT <-
  lag.listw(
    lw_W,
    boston_utm$LSTAT
  )

slx <- lm(
  MEDV ~
    CRIM + RM + NOX + LSTAT +
    W_CRIM + W_LSTAT,
  data = boston_utm
)

summary(slx)

summary(m)

moran.test(
  resid(slx),
  lw_W
)

#you need lat lon to use conley, reproject
boston_wgs <- st_transform(boston_utm, 4326)

coords <- st_coordinates(
  st_centroid(boston_wgs)
)

boston_wgs$lon <- coords[,1]
boston_wgs$lat <- coords[,2]


slx <- feols(
  MEDV ~ CRIM + RM + NOX + LSTAT +
    W_CRIM + W_LSTAT,
  data = boston_wgs
)

summary(
  slx,
  vcov = vcov_conley(
    lat = "lat",
    lon = "lon",
    cutoff = 2.5
  )
)


#Sar

library(spatialreg)

sar <- lagsarlm(
  MEDV ~ CRIM + RM + NOX + LSTAT,
  data = boston_utm,
  listw = lw_W
)

summary(sar)

imp <- impacts(
  sar,
  listw = lw_W,
  R = 1000
)

summary(imp, zstats = TRUE)

#Coefficients in SAR are not directly comparable to OLS because spatial feedback loops amplify shocks through the network.

#Rather than treating Wy as an ordinary regressor,
#the SAR model jointly estimates ρ and β via maximum likelihood, 
#accounting for the simultaneity induced by Wy.


#SEM

sem <- errorsarlm(
  MEDV ~ CRIM + RM +
    NOX + LSTAT,
  data = boston_utm,
  listw = lw_W
)

summary(sem)
