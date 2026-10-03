
# PREPARE MONTHLY ENVIRONMENTAL DATA


# 1. Load packages

source("R/packages.R")


# 2. Download monthly WorldClim data

monthly_tavg <- worldclim_global(
  var = "tavg",
  res = 0.5,
  path = "data/downloads/worldclim_monthly"
)

monthly_prec <- worldclim_global(
  var = "prec",
  res = 0.5,
  path = "data/downloads/worldclim_monthly"
)

monthly_vapr <- worldclim_global(
  var = "vapr",
  res = 0.5,
  path = "data/downloads/worldclim_monthly"
)


# 3. Load Kasaï-Central boundary

kc_boundary <- vect(
  "data/clean/kc_health_zones.gpkg"
) |>
  aggregate()


# 4. Crop and mask monthly climate rasters

monthly_tavg_kc <- monthly_tavg |>
  crop(kc_boundary) |>
  mask(kc_boundary)

monthly_prec_kc <- monthly_prec |>
  crop(kc_boundary) |>
  mask(kc_boundary)

monthly_vapr_kc <- monthly_vapr |>
  crop(kc_boundary) |>
  mask(kc_boundary)


# Use WorldClim as the reference grid

climate_grid <- monthly_tavg_kc[[1]]


# 5. Load existing land-cover variables

landcover_wc <- rast(
  list.files(
    "data/downloads/landuse",
    pattern = "\\.tif$",
    full.names = TRUE
  )
)


# Keep land-cover variables used for PCA

landcover_wc <- landcover_wc[[
  c(
    "trees",
    "grassland",
    "shrubs",
    "cropland",
    "built",
    "bare",
    "water",
    "wetland"
  )
]]


# 6. Use the WorldClim grid for land cover

landcover_wc <- landcover_wc |>
  crop(climate_grid) |>
  mask(climate_grid)


# Check that climate and land cover use the same grid

compareGeom(
  climate_grid,
  landcover_wc[[1]],
  stopOnError = FALSE
)


# 7. Load household coordinates

coords <- read_csv(
  "data/clean/kc_household_coords.csv",
  show_col_types = FALSE
)


# 8. Apply empirical-logit transformation

emplogit_fraction <- function(
    fraction,
    trials = 1e4
) {
  
  successes <- trials * fraction
  failures <- trials - successes
  
  log(
    (successes + 0.5) /
      (failures + 0.5)
  )
}


landcover_wc_emplogit <- emplogit_fraction(
  landcover_wc
)


# 9. Extract transformed land-cover values
# at sampled households

landcover_household_wc <- terra::extract(
  landcover_wc_emplogit,
  select(
    coords,
    long_dd,
    lat_dd
  ),
  ID = FALSE
)


# 10. Remove constant land-cover variables

landcover_variance_wc <- apply(
  landcover_household_wc,
  2,
  var,
  na.rm = TRUE
)

landcover_keep_wc <- names(
  landcover_variance_wc[
    landcover_variance_wc > 0
  ]
)

landcover_household_wc <- landcover_household_wc[
  ,
  landcover_keep_wc,
  drop = FALSE
]

landcover_wc_emplogit <- landcover_wc_emplogit[[
  landcover_keep_wc
]]


# 11. Perform land-cover PCA

pca_landcover_wc <- prcomp(
  landcover_household_wc,
  center = TRUE,
  scale. = TRUE
)

summary(
  pca_landcover_wc
)


# 12. Apply PCA across the WorldClim grid

landcover_pcs_wc <- predict(
  landcover_wc_emplogit,
  pca_landcover_wc
)


# Keep first five PCs

landcover_pcs_wc <- landcover_pcs_wc[[
  1:5
]]

names(
  landcover_pcs_wc
) <- c(
  "landcover_wc_pc1",
  "landcover_wc_pc2",
  "landcover_wc_pc3",
  "landcover_wc_pc4",
  "landcover_wc_pc5"
)


# 13. Plot environmental covariates

plot(
  monthly_tavg_kc,
  axes = FALSE,
  col = idem(
    50,
    rev = TRUE
  )
)

plot(
  monthly_prec_kc,
  axes = FALSE,
  col = idem(
    50,
    rev = TRUE
  )
)

plot(
  monthly_vapr_kc,
  axes = FALSE,
  col = idem(
    50,
    rev = TRUE
  )
)

plot(
  landcover_pcs_wc,
  axes = FALSE,
  col = idem(
    50,
    rev = TRUE
  )
)


# 14. Save prepared environmental covariates

writeRaster(
  monthly_tavg_kc,
  "data/clean/monthly_tavg_kc.tif",
  overwrite = TRUE
)

writeRaster(
  monthly_prec_kc,
  "data/clean/monthly_prec_kc.tif",
  overwrite = TRUE
)

writeRaster(
  monthly_vapr_kc,
  "data/clean/monthly_vapr_kc.tif",
  overwrite = TRUE
)

writeRaster(
  landcover_pcs_wc,
  "data/clean/landcover_pcs_worldclim_grid.tif",
  overwrite = TRUE
)

saveRDS(
  pca_landcover_wc,
  "data/clean/pca_landcover_worldclim_grid.rds"
)