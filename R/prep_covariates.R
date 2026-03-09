# prep covariates for use in modelling

source("R/packages.R")

# use geodata to load some rasters

# bioclim climate data
# this means use package geodata to call function worldclim_country (in this case COD)

drc_bioclim <- geodata::worldclim_country("COD", var = "bioc")
names(drc_bioclim) <- gsub("wc2.1_30s_", "", names(drc_bioclim))

# landcover data
# global_trees <- global_landcover
global_trees <- geodata::landcover(var = "trees")
global_grassland <- geodata::landcover(var = "grassland")
global_shrubs <- geodata::landcover(var = "shrubs")
global_cropland <- geodata::landcover(var = "cropland")
global_built <- geodata::landcover(var = "built")
global_bare <- geodata::landcover(var = "bare")
global_snow <- geodata::landcover(var = "snow")
global_water <- geodata::landcover(var = "water")
global_wetland <- geodata::landcover(var = "wetland")
global_mangroves <- geodata::landcover(var = "mangroves")
global_moss <- geodata::landcover(var = "moss")

global_landcover <- c(global_trees,
                      global_grassland,
                      global_shrubs,
                      global_cropland,
                      global_built,
                      global_bare,
                      global_snow,
                      global_water,
                      global_wetland,
                      global_mangroves,
                      global_moss)

# crop to the extent around the study area : KC
# get administrative area for a country

drc_shp <-gadm(
  country = "COD",
  level= 0,
  path= "data/downloads"
)

# Select KC

kc<- drc_shp_prv |> 
  filter(
    NAME_1 == "Kasaï-Central"
  )

# extent around the study area
kc_ext <- ext(kc)

# crop rasters to KC extent
bioclim_crop <- crop(drc_bioclim, kc_ext)
landcover_crop <- crop(global_landcover, kc_ext)

# plot
plot (landcover_crop)

writeRaster(
  bioclim_crop,
  "data/clean/bioclim_crop.tif"
)

writeRaster(
  landcover_crop,
  "data/clean/landcover_crop.tif"
)


# load the data coordinates, to transform covariates to maximise exaplanation of
# variance across these locations
coords <- read_csv("data/clean/kc_households_coords.csv",
                   col_types = cols(
                     health_zone = col_character(),
                     health_area = col_character(),
                     village = col_character(),
                     lat_dd = col_double(),
                     long_dd = col_double()
                   )) 
