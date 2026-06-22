############################################################
# CLIMATE ECONOMETRICS LAB
# Lecture 4
#
# Goal:
# Starting from raw climate and socio-economic rasters,
# build harmonized country-year exposure measures
# for econometric analysis.
############################################################

#Era5 data https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means?tab=overview 
#pop data https://www.earthdata.nasa.gov/data/catalog/sedac-ciesin-sedac-gpwv4-apct-wpp-2015-r11-4.11 
#land use data https://archaeology.datastations.nl/dataset.xhtml?persistentId=doi:10.17026/DANS-25G-GEZ3

############################################################
# 0. Load libraries
############################################################
library(terra)
library(tidyverse)
library(lubridate) #time manipulation
library(sf)
library(exactextractr)
library(fixest)
library(WDI)
library(rnaturalearth)

############################################################
# 1. Load ERA5 climate data
############################################################

# ERA5 monthly climate data:
# https://cds.climate.copernicus.eu/

temp <- rast("lecture4/data/temp_rot.tif")
prec <- rast("lecture4/data/prec_rot.tif")


############################################################
# 2. Explore raster metadata
############################################################

temp
prec

# Number of layers
nlyr(temp)
nlyr(prec)
#552 layers = 46 years × 12 months=552

# Inspect layer names
names(temp)[1:5]
names(prec)[1:5]

#dates are often recored as seconds since 1-1-1970. You can check 
as.POSIXct(
  315532800,
  origin = "1970-01-01",
  tz = "UTC"
)

#let's fix all names 
names(temp) <- as.POSIXct(
  as.numeric(sub(".*=", "", names(temp))),
  origin = "1970-01-01",
  tz = "UTC"
)

names(prec) <- as.POSIXct(
  as.numeric(sub(".*=", "", names(prec))),
  origin = "1970-01-01",
  tz = "UTC"
)

# Coordinate Reference System (CRS)
crs(temp)

# Spatial resolution
res(temp)

# Spatial extent
ext(temp)


############################################################
# DIDACTIC QUESTION
#
# - What CRS are we using?
# - What is the spatial resolution?
# - Does it match the ERA5 documentation?
#
# Expected answer:
# WGS84 / CRS84
# 0.25° x 0.25°
############################################################



############################################################
# 3. Inspect climate units
############################################################

# Minimum and maximum values for first month

global(temp[[1]], c("min", "max"), na.rm = TRUE)

# Temperature values are around 250-310.
# Are these Celsius or Kelvin?

temp <- temp - 273.15
# ERA5 temperature is expressed in Kelvin


global(prec[[1]], c("min", "max"), na.rm = TRUE)
# just 57mm max in one month? 
# ERA5 monthly precipitation is stored in meters per day (m/day).
#
# To obtain annual precipitation totals:
# 1. multiply each month by its number of days;
# 2. sum monthly totals within each year;
# 3. convert meters to millimeters.


days <- days_in_month(
  as.Date(names(prec))
)

years <- format(
  as.Date(names(prec)),
  "%Y"
)

prec_year <- tapp(
  prec * days, #compute month totals
  years, #over years
  sum #sum over months
) * 1000 #transform into mm


# Temperature is an intensive variable.
# Annual temperature is computed
# as the average across months.

temp_year <- tapp(
  temp, #compute month totals
  years, #over years
  mean #average across months
)

#fix names
names(temp_year) <- unique(years)
names(prec_year) <- unique(years)


############################################################
# Sanity check after conversion
############################################################

global(temp_year, c("min", "mean", "max"), na.rm = TRUE)
global(prec_year, c("min", "mean", "max"), na.rm = TRUE)

############################################################
# Expected values:
#
# Temperature:
# roughly between -50°C and +40°C
#
# Precipitation:
# measured in millimeters
############################################################



############################################################
# 5. Visualize climate data
############################################################

plot(temp_year[[1]])
title("ERA5 Temperature, 1980")

plot(prec_year[[1]])
title("ERA5 Precipitation, 1980")

############################################################
# DIDACTIC QUESTION
#
# Can you recognize:
# - deserts?
# - mountain ranges?
# - tropical regions?
############################################################



############################################################
# 6. Load population data
############################################################

# GPW v4 population data:
# https://www.earthdata.nasa.gov/

pop2000 <- rast(
  "lecture4/data/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2000_2pt5_min.tif"
)

pop2005 <- rast(
  "lecture4/data/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2005_2pt5_min.tif"
)

pop2010 <- rast(
  "lecture4/data/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2010_2pt5_min.tif"
)

pop2015 <- rast(
  "lecture4/data/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2015_2pt5_min.tif"
)

pop2020 <- rast(
  "lecture4/data/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif"
)


pop <- rast(
  list(
    pop2000,
    pop2005,
    pop2010,
    pop2015,
    pop2020
  )
)


names(pop) <- c("2000", "2005", "2010", "2015", "2020")

names(pop)

############################################################
# 8. Sanity check:
# World population totals
############################################################

global(pop, "sum", na.rm = TRUE) / 1e9

############################################################
# Expected values:
#
# 2000 ~ 6.1 billion
# 2005 ~ 6.5 billion
# 2010 ~ 6.9 billion
# 2015 ~ 7.3 billion
# 2020 ~ 7.8 billion
############################################################



############################################################
# 9. Interpolation strategy
############################################################

# We only observe population every 5 years.
#
# We construct annual population layers using:
#
# 1980–1999 -> use 2000 values
# 2000–2020 -> linear interpolation
# 2021–2025 -> use 2020 values

#We assume linear growth in temperature (reasonable approximation)
############################################################



############################################################
# 10. Manual interpolation example
############################################################

# Construct population for 2003 manually

pop2003 <-
  0.4 * pop[["2000"]] +
  0.6 * pop[["2005"]]

plot(log(pop2003))

############################################################
# DIDACTIC QUESTION
#
# Why 0.4 and 0.6?
#
# Because:
#
# 2000 ---- 2003 ---- 2005
#
# 2003 is 60% of the way from 2000 to 2005.
############################################################



############################################################
# 11. Build a general interpolation function
############################################################

interp_rast <- function(year, rast){
  
  # before 2000: use 2000 population
  
  if(year <= 2000)
    return(rast[["2000"]])
  
  # after 2020: use 2020 population
  
  if(year >= 2020)
    return(rast[["2020"]])
  
  # lower and upper benchmark years
  
  y0 <- floor(year / 5) * 5
  y1 <- y0 + 5
  
  # interpolation weight
  
  weight_next <-
    (year - y0) /
    (y1 - y0)
  
  # linear interpolation
  
  (1 - weight_next) *
    rast[[as.character(y0)]] +
    
    weight_next *
    rast[[as.character(y1)]]
}



############################################################
# 12. Test the function
############################################################

test <- interp_rast(2003, pop)
identical(test, pop2003)

############################################################
# 13. Create annual population rasters
############################################################

years <- 1980:2025

############################################################
# lapply applies the function to every year
############################################################

pop_yearly <- lapply(
  years,
  interp_rast,
  rast = pop
)

class(pop_yearly)

length(pop_yearly)

class(pop_yearly[[1]])

############################################################
# DIDACTIC QUESTION
#
# Why is the result a LIST?
#
# Because each element is a raster.
############################################################



############################################################
# 14. Convert list of rasters into a SpatRaster
############################################################

pop_yearly <- rast(pop_yearly)

names(pop_yearly) <- years

class(pop_yearly)

nlyr(pop_yearly)

pop_yearly

############################################################
# Expected result:
#
# A SpatRaster with 46 layers
#
# 1980 ... 2025
############################################################



############################################################
# 15. Final sanity checks
############################################################

global(
  pop_yearly,
  "sum",
  na.rm = TRUE
) / 1e9

############################################################
# DIDACTIC QUESTION
#
# Is using 2000 population for 1980–1999
# potentially problematic?
#
# Under which circumstances could this
# introduce measurement error?
############################################################

############################################################
# SPATIAL HARMONIZATION
#
# Climate and socio-economic data
# rarely share the same spatial grid.
#
# We face two distinct problems:
#
# 1. Different spatial resolution
# 2. Different grid alignment
############################################################
# First, fix spatial resolution
res(prec_year)
res(pop_yearly)

res(prec_year)/res(pop_yearly)

# ERA5 resolution: 15 arc-minutes (0.25°)
# Population resolution: 2.5 arc-minutes
#
# One ERA5 cell contains:
#
# 6 × 6 = 36 population cells.

# Aggregate population to the ERA5 resolution.
#
# fact = 6 means:
# merge 6 cells in longitude and
# 6 cells in latitude.
# A block of 6x6 cells
#
# Population is an extensive variable,
# therefore we SUM values.

pop_15 <- aggregate(
  pop_yearly,
  fact = 6,
  fun = "sum",
  na.rm = TRUE
)

res(pop_15)

#sanity check (number should be equal)
global(pop_15, "sum", na.rm = TRUE) / 1e9
global(pop_yearly, "sum", na.rm = TRUE) / 1e9


ext(pop_15)
ext(prec_year)

origin(pop_15)
origin(prec_year)

############################################################
# GRID ALIGNMENT
#
# ERA5 and population grids have
# the same resolution (0.25°),
# but differ by half a cell (0.125°).
#
# Following Gortan et al. (2024),
# we align the population grid
# to ERA5 using bilinear interpolation.

# |----|----|----|----|
#   -180 -.75 -.50 -.25 0
#
#ERA5 grid
#
# |----|----|----|----|
#   -180.125 -.875 -.625 -.375 -.125
############################################################

pop_era5 <- resample(
  pop_15,
  prec_year,
  method = "bilinear"
)

############################################################
# Final checks
#
# 1. Geometries should now match.
# 2. World population totals should
#    remain approximately unchanged.
############################################################

compareGeom(
  pop_era5,
  prec_year,
  stopOnError = FALSE
)

global(pop_era5, "sum", na.rm = TRUE) / 1e9






############################################################
# VECTOR DATA
#
# Until now we worked with rasters.
#
# Econometric analysis, however, is often
# performed at the country level.
#
# Therefore we need country polygons
# to aggregate raster information.
############################################################

countries <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

#check compatibility
st_crs(countries)
crs(temp_year)

#visual overlay
plot(prec_year[[1]])
plot(st_geometry(countries), add = TRUE)


names(countries)

############################################################
# exact_extract computes zonal statistics
# over polygons.
#
# It automatically accounts for
# partial overlaps between raster cells
# and country borders.
#
# This is generally more accurate than
# raster::extract().
############################################################

############################################################
# POPULATION-WEIGHTED CLIMATE EXPOSURE
#
# We want to compute:
#
#              Σ(T_i × Pop_i)
# T_pw = ----------------------------
#              Σ(Pop_i)
#
# where i indexes grid cells.
#
# Population weighting approximates
# the climate experienced by people
# rather than by land area.
############################################################

temp_pop <- temp_year * pop_era5
prec_pop <- prec_year * pop_era5


temp_num <- exact_extract(
  temp_pop,
  countries,
  "sum"
)

pop_den <- exact_extract(
  pop_era5,
  countries,
  "sum"
)

temp_pw <- temp_num / pop_den



prec_num <- exact_extract(
  prec_pop,
  countries,
  "sum"
)

prec_pw <- prec_num / pop_den

dim(temp_pw)
dim(prec_pw)

summary(as.matrix(temp_pw))
summary(as.matrix(prec_pw))

#Is the planet warming? 
plot(
  1980:2025,
  colMeans(temp_pw, na.rm = TRUE),
  type = "l",
  xlab = "Year",
  ylab = "Population-weighted temperature",
  main = "Global warming signal"
)

abline(
  lm(colMeans(temp_pw, na.rm = TRUE) ~ I(1980:2025)),
  col = "red",
  lwd = 2
)

############################################################
# DIDACTIC QUESTION
#
# Why do we observe an upward trend?
#
# Is this evidence of climate change?
#
# Not necessarily:
# population weights also evolve over time.
############################################################


## regression

temp_pw

#filter NaNs

keep <- !apply(is.na(temp_pw), 1, all)

countries <- countries[keep, ]
temp_pw   <- temp_pw[keep, ]
prec_pw   <- prec_pw[keep, ]

temp_df <- as.data.frame(temp_pw)
prec_df <- as.data.frame(prec_pw)

years <- 1980:2025

names(temp_df) <- paste0("temp_", years)
names(prec_df) <- paste0("prec_", years)


panel_wide <- countries %>%
  st_drop_geometry() %>%
  transmute(
    iso3 = adm0_a3,
    country = name
  ) %>%
  bind_cols(
    temp_df,
    prec_df,
  )

#sanity checks
dim(panel_wide)

head(
  panel_wide %>%
    select(iso3, country, temp_1980, prec_1980)
)

sum(duplicated(panel_wide$iso3))

panel_wide %>%
  count(iso3) %>%
  filter(n > 1)

############################################################
# Most econometric software expects
# panel data in LONG format:
#
# country year temp prec
#
# rather than:
#
# country temp_1980 temp_1981 ...
############################################################

names(panel_wide)

panel <- panel_wide %>%
  pivot_longer(
    cols = -c(iso3, country),
    names_to = c(".value", "year"),
    names_sep = "_"
  ) %>%
  mutate(
    year = as.integer(year)
  )

############################################################
# WORLD BANK GDP PER CAPITA
#
# Indicator:
#
# NY.GDP.PCAP.PP.KD
#
# GDP per capita (PPP, constant dollars)
############################################################

gdp <- WDI::WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.PP.KD",
  start = 1980,
  end = 2025
)

############################################################
# Merge climate data with GDP.
#
# We keep all climate observations
# and later inspect missing GDP values.
############################################################


panel <- panel %>%
  left_join(
    gdp %>% select(iso3c, year, NY.GDP.PCAP.PP.KD),
    by = c("iso3" = "iso3c", "year")
  )

sum(is.na(panel$NY.GDP.PCAP.PP.KD))

panel %>%
  group_by(iso3, country) %>%
  summarise(
    n_missing = sum(is.na(NY.GDP.PCAP.PP.KD)),
    .groups = "drop"
  ) %>%
  arrange(desc(n_missing)) %>% print(n=5000)

#remove missing values

panel <- panel %>%
  filter(
    !is.na(NY.GDP.PCAP.PP.KD)
  )


############################################################
# Economic growth:
#
# g_it =
# 100 × [ log(GDP_it) − log(GDP_i,t−1) ]
#
# Log differences approximate
# percentage growth rates.
############################################################

panel <- panel %>%
  arrange(
    iso3,
    year
  ) %>%
  group_by(iso3) %>%
  mutate(
    growth = 100 * (
      log(NY.GDP.PCAP.PP.KD) -
        lag(log(NY.GDP.PCAP.PP.KD))
    )
  ) %>%
  ungroup()

#sanity checks
summary(panel$growth)

#inspect outliers
panel %>% 
  filter(growth >=80)

panel %>% 
  filter(iso3=="GNQ")

panel %>% 
  filter(growth < -100)

panel %>% 
  filter(iso3=="IRQ")

quantile(
  panel$growth,
  probs = c(.01, .05, .5, .95, .99),
  na.rm = TRUE
)


############################################################
# COUNTRY-SPECIFIC TRENDS
#
# Each country is allowed to have
# its own long-run trajectory:
#
# α_i + δ_i t + γ_i t²
#
# This removes slow-moving processes:
#
# - institutional change
# - demographic transition
# - economic convergence
############################################################

#create a time trend variable to use for country specific trends

panel <- panel %>%
  mutate(
    t = year - min(year)
  )


m1 <- feols(
  growth ~ temp + I(temp^2) +
    prec + I(prec^2) |
    iso3[ t + I(t^2) ] + year,
  data = panel,
  cluster = ~iso3
)

m1

############################################################
# TEMPERATURE OPTIMUM
#
# If:
#
# growth = β1*T + β2*T²
#
# the optimum temperature is:
#
# T* = -β1 / (2β2)
############################################################

-coef(m1)["temp"]/(2*coef(m1)["I(I(temp^2))"]) #a bit to high...


############################################################
# Extreme growth episodes:
#
# wars
# oil discoveries
# financial crises
#
# may dominate estimates.
#
# We winsorize growth
# as a robustness exercise.
############################################################


panel2 <- panel %>%
  mutate(
    growth_w = pmax(
      pmin(growth, 15),
      -15
    )
  )

m2 <- feols(
  growth_w ~ temp + I(temp^2) +
    prec + I(prec^2) |
    iso3[ t + I(t^2) ] + year,
  data = panel2,
  cluster = ~iso3
)

m2

-coef(m2)["temp"]/(2*coef(m2)["I(I(temp^2))"])



#Let's plot the parabola

b1 <- coef(m2)["temp"]
b2 <- coef(m2)["I(I(temp^2))"]

opt <- -b1/(2*b2)

curve_df <- tibble(
  temp = seq(
    min(panel2$temp, na.rm = TRUE),
    max(panel2$temp, na.rm = TRUE),
    length.out = 500
  )
) %>%
  mutate(
    growth = b1 * temp + b2 * temp^2
  )

curve_df <- curve_df %>%
  mutate(
    growth_centered =
      growth -
      (b1*opt + b2*opt^2)
  )

ggplot(curve_df, aes(temp, growth_centered)) +
  
  geom_hline(
    yintercept = 0,
    linewidth = .4,
    colour = "grey70"
  ) +
  
  geom_line(
    linewidth = 1.4,
    colour = "#2C7FB8"
  ) +
  
  geom_vline(
    xintercept = opt,
    linetype = 2,
    linewidth = .8,
    colour = "firebrick"
  ) +
  
  geom_point(
    aes(
      x = opt,
      y = 0
    ),
    colour = "firebrick",
    size = 3
  ) +
  
  annotate(
    "text",
    x = opt + 1,
    y = 2,
    label = paste0(
      "Optimum = ",
      round(opt, 1),
      "°C"
    ),
    hjust = 0,
    size = 5
  ) +
  
  labs(
    x = "Annual temperature (°C)",
    y = "Predicted growth effect",
    title = "Estimated temperature-growth relationship"
  ) +
  
  theme_minimal(base_size = 16) +
  
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )
