
# MONTHLY CLIMATE COVARIATES FOR SPECIES DISTRIBUTION MODELLING

# This test script prepares monthly climate covariates for the
# Kasaï-Central Anopheles species distribution models.
#
# Monthly mean temperature, precipitation, and water vapour pressure
# are matched to the the monthly entomo survey of mosquito in the fiedl.

# 1. Load packages -------------------------------------------------------------

source(
  "R/packages.R"
)


# 2. Download monthly WorldClim variables --------------------------------------

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


# 3. Check downloaded monthly climate layers ----------------------------------

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