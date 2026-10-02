
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


# 4. Load DRC health-zone boundaries

drc_health_zones <- vect(
  "data/downloads/grid3/grid3_cod_health_zones_v8_0.gpkg"
)


# 5. Select Kasaï-Central health zones

kasai_central_health_zones <- drc_health_zones |>
  filter(
    province == "Kasaï-Central"
  )


# 6. Create Kasaï-Central boundary

kasai_central_boundary <- aggregate(
  kasai_central_health_zones
)


# 7. Crop monthly climate rasters to Kasaï-Central

monthly_tavg_kc <- crop(
  monthly_tavg,
  kasai_central_boundary
)

monthly_prec_kc <- crop(
  monthly_prec,
  kasai_central_boundary
)

monthly_vapr_kc <- crop(
  monthly_vapr,
  kasai_central_boundary
)


# 8. Mask rasters to Kasaï-Central boundary

monthly_tavg_kc <- mask(
  monthly_tavg_kc,
  kasai_central_boundary
)

monthly_prec_kc <- mask(
  monthly_prec_kc,
  kasai_central_boundary
)

monthly_vapr_kc <- mask(
  monthly_vapr_kc,
  kasai_central_boundary
)


# 9. Plot monthly climate rasters

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


# 10. Save monthly climate rasters

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


# 11. Check that files were saved

file.exists(
  "data/clean/monthly_tavg_kc.tif"
)

file.exists(
  "data/clean/monthly_prec_kc.tif"
)

file.exists(
  "data/clean/monthly_vapr_kc.tif"
)
