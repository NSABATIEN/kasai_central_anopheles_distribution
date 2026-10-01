
# MONTHLY CLIMATE COVARIATES FOR SPECIES DISTRIBUTION MODELLING

# This test script prepares monthly climate covariates for the
# Kasaï-Central Anopheles species distribution models.
#
# Monthly mean temperature, precipitation, and water vapour pressure
# are matched to the the monthly entomo survey of mosquito in the fiedl.

# 1. Load packages

source(
  "R/packages.R"
)


# 2. Download monthly WorldClim variables 

# WorldClim 2.1 climate rasters at 0.5 arc-minute resolution.

# Download 12 monthly climate layers for:
# - mean temperature;
# - precipitation; and
# - water vapour pressure.

# Download monthly mean temperature.

monthly_tavg <- geodata::worldclim_global(
  var = "tavg",
  res = 0.5,
  path = "data/downloads/worldclim_monthly"
)

# Download monthly precipitation.

monthly_prec <- geodata::worldclim_global(
  var = "prec",
  res = 0.5,
  path = "data/downloads/worldclim_monthly"
)


# Download monthly water vapour pressure.

monthly_vapr <- geodata::worldclim_global(
  var = "vapr",
  res = 0.5,
  path = "data/downloads/worldclim_monthly"
)


# 3. Check downloaded monthly climate layers 

# Inspect the raster-layer names for each climate variable.

names(
  monthly_tavg
)

names(
  monthly_prec
)

names(
  monthly_vapr
)


# Confirm that each climate variable contains 12 monthly layers.

nlyr(
  monthly_tavg
)

nlyr(
  monthly_prec
)

nlyr(
  monthly_vapr
)


# 4. Load GRID3 DRC health-zone boundaries 

# Load the GRID3 health-zone boundaries for the
# Democratic Republic of the Congo.

drc_health_zones <- terra::vect(
  "data/downloads/grid3/grid3_cod_health_zones_v8_0.gpkg"
)


# Inspect the health-zone boundary dataset.

drc_health_zones


# Check the available attribute names.

names(
  drc_health_zones
)

head(
  drc_health_zones
)

# Create one national DRC boundary by dissolving
# all GRID3 health-zone polygons.

drc_boundary <- terra::aggregate(
  drc_health_zones
)

# 5. Crop monthly climate rasters to the DRC 

# Crop the global WorldClim monthly rasters to the
# spatial extent of the Democratic Republic of the Congo.
#
# Cropping reduces the amount of raster data that must be processed.
# Cells outside the national boundary are still present at this stage
# and will be removed in the next step.

monthly_tavg_drc <- terra::crop(
  monthly_tavg,
  drc_boundary
)

monthly_prec_drc <- terra::crop(
  monthly_prec,
  drc_boundary
)

monthly_vapr_drc <- terra::crop(
  monthly_vapr,
  drc_boundary
)

# 6. Mask monthly climate rasters to the DRC 

# Mask the cropped rasters using the GRID3-derived
# national boundary.
#
# Raster cells outside the DRC are set to NA.

monthly_tavg_drc <- terra::mask(
  monthly_tavg_drc,
  drc_boundary
)

monthly_prec_drc <- terra::mask(
  monthly_prec_drc,
  drc_boundary
)

monthly_vapr_drc <- terra::mask(
  monthly_vapr_drc,
  drc_boundary
)


# 7. Check DRC monthly climate rasters 

# Confirm that each DRC climate raster still contains
# 12 monthly layers.

nlyr(
  monthly_tavg_drc
)

nlyr(
  monthly_prec_drc
)

nlyr(
  monthly_vapr_drc
)


# Check the monthly raster-layer names.

names(
  monthly_tavg_drc
)

names(
  monthly_prec_drc
)

names(
  monthly_vapr_drc
)


# 8. Select Kasaï-Central health zones

# Select all GRID3 health zones belonging to
# Kasaï-Central Province.

kasai_central_health_zones <- drc_health_zones |>
  filter(
    province == "Kasaï-Central"
  )

# Confirm the number of selected health zones.

nrow(
  kasai_central_health_zones
)


# Check the selected health-zone names.

kasai_central_health_zones$zonesante


# 9. Create Kasaï-Central boundary

# Dissolve all selected health-zone polygons into
# one Kasaï-Central provincial boundary.

kasai_central_boundary <- terra::aggregate(
  kasai_central_health_zones
)


# Inspect the Kasaï-Central boundary.

kasai_central_boundary


# 10. Crop monthly climate rasters to Kasaï-Central

# Crop the DRC monthly climate rasters to the spatial extent
# of Kasaï-Central Province.

monthly_tavg_kc <- terra::crop(
  monthly_tavg_drc,
  kasai_central_boundary
)

monthly_prec_kc <- terra::crop(
  monthly_prec_drc,
  kasai_central_boundary
)

monthly_vapr_kc <- terra::crop(
  monthly_vapr_drc,
  kasai_central_boundary
)


# 11. Mask monthly climate rasters to Kasaï-Central

# Mask the cropped monthly climate rasters using the
# Kasaï-Central provincial boundary.
#
# Raster cells outside Kasaï-Central are set to NA.

monthly_tavg_kc <- terra::mask(
  monthly_tavg_kc,
  kasai_central_boundary
)

monthly_prec_kc <- terra::mask(
  monthly_prec_kc,
  kasai_central_boundary
)

monthly_vapr_kc <- terra::mask(
  monthly_vapr_kc,
  kasai_central_boundary
)


# 12. Check Kasaï-Central monthly climate rasters

# Confirm that each climate raster contains
# 12 monthly layers.

nlyr(
  monthly_tavg_kc
)

nlyr(
  monthly_prec_kc
)

nlyr(
  monthly_vapr_kc
)


# 13. Visually inspect Kasaï-Central monthly climate rasters

# Plot selected months to inspect seasonal spatial patterns
# across Kasaï-Central.

# 13. Visually inspect Kasaï-Central monthly climate rasters

# Plot the 12 monthly mean-temperature layers to inspect
# seasonal spatial patterns across Kasaï-Central.

plot(
  monthly_tavg_kc[[1:12]],
  axes = FALSE,
  col = idem(
    50,
    rev = TRUE
  )
)


# Plot the 12 monthly precipitation layers.

plot(
  monthly_prec_kc[[1:12]],
  axes = FALSE,
  col = idem(
    50,
    rev = TRUE
  )
)


# Plot the 12 monthly water-vapour-pressure layers.

plot(
  monthly_vapr_kc[[1:12]],
  axes = FALSE,
  col = idem(
    50,
    rev = TRUE
  )
)
