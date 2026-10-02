
# PREPARE MONTHLY CLIMATE DATA


# 1. Load packages

source("R/packages.R")


# 2. Download monthly WorldClim data

# Mean temperature

monthly_tavg <- worldclim_global(
  var = "tavg",
  res = 0.5,
  path = "data/downloads/worldclim_monthly"
)


# Precipitation

monthly_prec <- worldclim_global(
  var = "prec",
  res = 0.5,
  path = "data/downloads/worldclim_monthly"
)


# Vapour pressure

monthly_vapr <- worldclim_global(
  var = "vapr",
  res = 0.5,
  path = "data/downloads/worldclim_monthly"
)


# 3. Check that each variable has 12 monthly layers

nlyr(monthly_tavg)

nlyr(monthly_prec)

nlyr(monthly_vapr)


# 4. Load Kasaï-Central health-zone boundaries

kasai_central_health_zones <- vect(
  "data/clean/kc_health_zones.gpkg"
)


# 5. Create Kasaï-Central boundary

kasai_central_boundary <- aggregate(
  kasai_central_health_zones
)

# 6. Crop and mask monthly climate rasters to Kasaï-Central

monthly_tavg_kc <- monthly_tavg |>
  crop(kasai_central_boundary) |>
  mask(kasai_central_boundary)

monthly_prec_kc <- monthly_prec |>
  crop(kasai_central_boundary) |>
  mask(kasai_central_boundary)

monthly_vapr_kc <- monthly_vapr |>
  crop(kasai_central_boundary) |>
  mask(kasai_central_boundary)

# 7. Plot monthly climate rasters

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


# 8. Save monthly climate rasters

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


# 9. Check that files were saved

file.exists(
  "data/clean/monthly_tavg_kc.tif"
)

file.exists(
  "data/clean/monthly_prec_kc.tif"
)

file.exists(
  "data/clean/monthly_vapr_kc.tif"
)
