# prep covariates for use in modelling

# Install packages
# install.packages(c("geodata", "terra", "sf", "tidyterra", "dplyr","readr"))

# Load packages
library(geodata)
library(terra)
library(sf)
library(dplyr)
library(tidyterra)
library(readr)
library(dismo)
library(mgcv)
# use geodata to load some rasters

# bioclim climate data
# this means use package geodata to call function worldclim_country (in this case COD)

# Get GRID3 administrative health-zone boundaries for the country
drc_shp_hz <- sf::st_read(
  "data/downloads/grid3/grid3_cod_health_zones_v8_0.gpkg"
) |>
  janitor::clean_names()
# Load Kasaï-Central health-zone boundaries
kc_hz <- drc_shp_hz |>
  filter(
    province == "Kasaï-Central"
  )

# Convert sf boundary to terra vector
kc_hz_vect <- terra::vect(kc_hz)



# Load household coordinates
coords <- readr::read_csv(
  "data/clean/kc_household_coords.csv",
  show_col_types = FALSE
) |>
  dplyr::select(long_dd, lat_dd) |>
  dplyr::filter(!is.na(long_dd), !is.na(lat_dd))

#Load climate variable
drc_bioclim <- rast(
  "data/raw/climate/wc2.1_country/COD_wc2.1_30s_bio.tif"
)

names(drc_bioclim) <- gsub("wc2.1_30s_", "", names(drc_bioclim))

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

# Extent around the study area
kc_ext <- ext(kc_hz_vect)

# crop to the extent around the study area : KC
bioclim_crop <- crop(drc_bioclim, kc_ext)
landcover_crop <- crop(global_landcover, kc_ext)

bioclim_crop <- mask(bioclim_crop, kc_hz_vect)
landcover_crop <- mask(landcover_crop, kc_hz_vect)

# Plot
plot(landcover_crop)

## explanation : 
#legend from 0.0 to 1.0 for treess : Yellow = lots of trees "1", "0" no trees
# Yellow = more grassland "1", "0" no grassland
# Yellow = more shrubs "1", "0" no shrubs 
# Yellow = more agriculture "1","0" no cropland


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
coords <- read_csv("data/clean/kc_household_coords.csv",
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
    dplyr::select(coords, long_dd, lat_dd),
    ID = FALSE
  ) %>%
  prcomp(
    center = TRUE,
    scale. = TRUE
  )

summary(pca_bioclim)

# keep the Bioclimatic variables explaining 95% of the variance
sry <- summary(pca_bioclim)
n_pcs_keep <- min(which(sry$importance["Cumulative Proportion", ] > 0.9))

# make rasters of those variables explaining 95% of the variance
pcs_bioclim <- predict(bioclim_crop, pca_bioclim)
pcs_bioclim_keep <- pcs_bioclim[[seq_len(n_pcs_keep)]]
names(pcs_bioclim_keep) <- paste0("bioclim_", tolower(names(pcs_bioclim_keep)))
plot(pcs_bioclim_keep)

#> pca_bioclim$rotation # I can check what pc is doing what
## BIO1: Annual Mean Temperature, BIO2: Mean Diurnal Range (Mean of monthly (max temp - min temp)), BIO3: Isothermality (BIO2/BIO7) (×100) and BIO4: Temperature Seasonality (standard deviation ×100) were the only variable explaining 95% of the variance

# Now let do the same for landcover
# drop the layers with no variance (all 0% cover for snow, mangroves, moss) 
# in my study area, cannot help the model. So I need to remove them
landcover_vals <- landcover_crop %>%
  terra::extract(
    dplyr::select(coords, long_dd, lat_dd),
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

pca_landcover <- landcover_crop_sub_emplogit %>%
  terra::extract(
    dplyr::select(coords, long_dd, lat_dd),
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

#need to know the interpretation of each pc for landcover
pca_landcover$rotation

#landcover_pc1 : "1" more tree-dominated landscapes
# "negative value" more grassland/cropland-dominated landscapes


# # quickly visualise these to check extent
drc <- geodata::gadm("COD", level = 0)
drc_l2 <- geodata::gadm("COD", level = 2)
plot(pcs_bioclim_keep$bioclim_pc1)
plot(drc_l2, add = TRUE, bg = grey(0.5))
plot(drc, lwd = 2, add = TRUE)
points(coords$lat_dd ~ coords$long_dd,
       pch = 21,
       bg = "blue",
       cex = 1)

covariates_crop <- c(pcs_bioclim_keep, pcs_landcover_keep)
writeRaster(covariates_crop,
            "data/clean/covariates.tif",
            overwrite = TRUE)

# # extract covariates values at household coordinates
# coord_covs <- terra::extract(
#   covariates_crop,
#   select(coords, long_dd, lat_dd),
#   ID = FALSE
# )

# # combine coordinates and covariates
# coord_covs <- bind_cols(coords, coord_covs)
# 
# # save coordinates with extracted covariates
# write_csv(
#   coord_covs,
#   "data/clean/coords_covariates.csv"
# )

## prepare covariates for whole country
drc <- geodata::gadm("COD", level = 0)


# get bioclim layers in country boundary
cod_bioclim <- drc_bioclim |>
  crop(drc) |>
  mask(drc)

# plot(cod_bioclim)

# predict using PCA from above
# then keep only layers predicting 90% of variance
cod_pcs_bioclim <- predict(cod_bioclim, pca_bioclim)
cod_pcs_bioclim_keep <- cod_pcs_bioclim[[seq_len(n_pcs_keep)]]
names(cod_pcs_bioclim_keep) <- paste0("bioclim_", tolower(names(cod_pcs_bioclim_keep)))

# plot smaller area together with larger to get understanding of extent of difference
# over whole country
par(mfrow = c(2, 3))
plot(pcs_bioclim_keep[[1]], range = c(-101, 17), main = names(pcs_bioclim_keep[[1]]))
plot(pcs_bioclim_keep[[2]], range = c(-123, 41), main = names(pcs_bioclim_keep[[2]]))
plot(pcs_bioclim_keep[[3]], range = c(-49, 16), main = names(pcs_bioclim_keep[[3]]))
plot(cod_pcs_bioclim_keep[[1]], range = c(-101, 17), main = "DRC bioclim_pc1")
plot(cod_pcs_bioclim_keep[[2]], range = c(-123, 41), main = "DRC bioclim_pc2")
plot(cod_pcs_bioclim_keep[[3]], range = c(-49, 16), main = "DRC bioclim_pc3")

# landcover
# get layers with variation per above, and crop to country boundary
cod_landcover <- global_landcover[[landcover_layers_varying]] |>
  crop(drc) |>
  mask(drc)

# convert with empirical logit function above
cod_landcover_sub_emp_logit <- cod_landcover |>
  emplogit_fraction()

# predict using PCA from above
# then keep only layers predicting 90% of variance
cod_pcs_landcover <- predict(cod_landcover_sub_emp_logit, pca_landcover)
cod_pcs_landcover_keep <- cod_pcs_landcover[[seq_len(n_pcs_keep_landcover)]]
names(cod_pcs_landcover_keep) <- paste0("landcover_", tolower(names(cod_pcs_landcover_keep)))


# plot smaller area together with larger to get understanding of extent of difference
# over whole country
par(mfrow = c(2, 2))
par(mar = c(1, 1, 2, 2))
plot(pcs_landcover_keep[[1]], range = c(-4.1, 4.3), main = names(pcs_landcover_keep[[1]]))
plot(cod_pcs_landcover_keep[[1]], range = c(-4.1, 4.3), main = "DRC landcover_pc1")
plot(pcs_landcover_keep[[2]], range = c(-3.8, 5), main = names(pcs_landcover_keep[[2]]))
plot(cod_pcs_landcover_keep[[2]], range = c(-3.8, 5), main = "DRC landcover_pc2")
plot(pcs_landcover_keep[[3]], range = c(-11.7, 6.8), main = names(pcs_landcover_keep[[3]]))
plot(cod_pcs_landcover_keep[[3]], range = c(-11.7, 6.8), main = "DRC landcover_pc3")
plot(pcs_landcover_keep[[4]], range = c(-2.9, 14.7), main = names(pcs_landcover_keep[[4]]))
plot(cod_pcs_landcover_keep[[4]], range = c(-2.9, 14.7), main = "DRC landcover_pc4")
plot(pcs_landcover_keep[[5]], range = c(-5.8, 4.5), main = names(pcs_landcover_keep[[5]]))
plot(cod_pcs_landcover_keep[[5]], range = c(-5.8, 4.5), main = "DRC landcover_pc5")


cod_covs <- c(cod_pcs_bioclim_keep, cod_pcs_landcover_keep) |>
  writeRaster(filename = "data/clean/cod_covariates.tif",
overwrite = TRUE
)
