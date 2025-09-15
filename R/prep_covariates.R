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


