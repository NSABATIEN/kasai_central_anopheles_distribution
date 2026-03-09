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

# Select KC province
drc_shp_prv <- gadm(
  country = "COD",
  level = 1,
  path = "data/downloads"
)
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
  "data/clean/bioclim_crop.tif",
  overwrite = TRUE
)

writeRaster(
  landcover_crop,
  "data/clean/landcover_crop.tif",
  overwrite = TRUE
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
# do PCA to reduce the dimensions of these
pca_bioclim <- bioclim_crop %>%
  terra::extract(
    select(coords, long_dd, lat_dd),
    ID = FALSE
  ) %>%
  prcomp(center = TRUE,
         scale = TRUE)
summary(pca_bioclim)

# keep the Bioclimatic variables explaining 95% of the variance
sry <- summary(pca_bioclim)
n_pcs_keep <- min(which(sry$importance["Cumulative Proportion", ] > 0.9))

# make rasters of those variables explaining 95% of the variance
pcs_bioclim <- predict(bioclim_crop, pca_bioclim)
pcs_bioclim_keep <- pcs_bioclim[[seq_len(n_pcs_keep)]]
names(pcs_bioclim_keep) <- paste0("bioclim_", tolower(names(pcs_bioclim_keep)))
plot(pcs_bioclim_keep)
## BIO1: Annual Mean Temperature, BIO2: Mean Diurnal Range (Mean of monthly (max temp - min temp)), BIO3: Isothermality (BIO2/BIO7) (×100) and BIO4: Temperature Seasonality (standard deviation ×100) were the only variable explaining 95% of the variance

# Now let do the same for landcover
# drop the layers with no variance (all 0% cover for snow, mangroves, moss)
landcover_vals <- landcover_crop %>%
  terra::extract(
    select(coords, long_dd, lat_dd),
    ID = FALSE
  )
landcover_layers_varying <- which(apply(landcover_vals, 2, var) != 0)
landcover_crop_sub <- landcover_crop[[landcover_layers_varying]]

# and empirical-logit transform it for more normality in the resulting variables
# (nicer plots at least). Assume the number of trials from the number of decimal
# places recorded in the raster (4)
emplogit_fraction <- function(fraction, trials = 1e4) {
  successes <- trials * fraction
  failures <- trials - successes
  log((successes + 0.5) / (failures + 0.5))
}
landcover_crop_sub_emplogit <- emplogit_fraction(landcover_crop_sub)

8 <- landcover_crop_sub_emplogit %>%
  terra::extract(
    select(coords, long_dd, lat_dd),
    ID = FALSE
  ) %>%
  prcomp(center = TRUE,
         scale = TRUE)

summary(pca_landcover)

# keep those explaining 90% of the variance
sry_landcover <- summary(pca_landcover)
sry_landcover
n_pcs_keep_landcover <- min(which(sry_landcover$importance["Cumulative Proportion", ] > 0.9))

pcs_landcover <- predict(landcover_crop_sub_emplogit, pca_landcover)
pcs_landcover_keep <- pcs_landcover[[seq_len(n_pcs_keep_landcover)]]
names(pcs_landcover_keep) <- paste0("landcover_", tolower(names(pcs_landcover_keep)))
plot(pcs_landcover_keep)
