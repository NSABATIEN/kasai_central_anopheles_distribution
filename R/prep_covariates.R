
# PREPARE ENVIRONMENTAL COVARIATES FOR SPECIES DISTRIBUTION MODELLING

## This script prepares environmental covariates for modelling
# Anopheles mosquito distributions across Kasaï-Central Province.
#
# Bioclimatic and land-cover variables are first processed for the
# study area. Principal component analysis (PCA) is then used to
# reduce collinearity among environmental variables and retain the
# principal components explaining at least 90% of the environmental
# variation represented at the sampled household locations.


# 1. Load packages

# Load all packages required for data manipulation, spatial processing,
# environmental covariate preparation, modelling, and visualisation.

source(
  "R/packages.R"
)


## 2. Download and load bioclimatic variables

# Download the 19 WorldClim bioclimatic variables for the
# Democratic Republic of the Congo.
#
# These variables describe long-term temperature and precipitation
# conditions that may influence Anopheles mosquito distributions.
#
# The file is stored in data/downloads/climate.
#
# This command can be commented out after the data have been
# downloaded successfully.

# drc_bioclim <- worldclim_country(
#   country = "COD",
#   var = "bioc",
#   path = "data/downloads"
# )


# Load the downloaded WorldClim bioclimatic raster.

drc_bioclim <- rast(
  "data/downloads/climate/wc2.1_country/COD_wc2.1_30s_bio.tif"
)


# Check the original WorldClim layer names.

names(
  drc_bioclim
)


# Remove the WorldClim prefix to create shorter
# and more readable variable names.
#
# For example:
# wc2.1_30s_bio_1 -> bio_1
# wc2.1_30s_bio_2 -> bio_2

names(drc_bioclim) <- gsub(
  "wc2.1_30s_",
  "",
  names(drc_bioclim)
)


# Confirm that the layer names were successfully standardised.

names(
  drc_bioclim
)


# 3. Check bioclimatic raster resolution

# Check the spatial resolution of the WorldClim bioclimatic rasters.
#
# The raster uses geographic coordinates, so the resolution
# is expressed in degrees. A resolution of approximately 0.00833°
# corresponds to 30 arc-seconds, or roughly 1 km near the equator.

res(
  drc_bioclim
)


# Extract the raster resolution in degrees.

resolution_deg <- res(
  drc_bioclim
)[1]


# Convert the approximate resolution from degrees to kilometres.
#
# One degree of latitude is approximately 111 km.

resolution_km <- resolution_deg * 111


# Display the approximate raster-cell size in kilometres.

round(
  resolution_km,
  2
)

# 4. Check bioclimatic raster spatial properties

# Check the geographic extent of the bioclimatic raster.
#
# The extent gives the minimum and maximum longitude and latitude
# covered by the raster across the Democratic Republic of the Congo.

ext(
  drc_bioclim
)


# Check the coordinate reference system (CRS).
#
# The WorldClim raster uses the WGS 84 geographic coordinate
# reference system (EPSG:4326).

crs(
  drc_bioclim
)


# Confirm that the raster contains the expected
# 19 WorldClim bioclimatic variables.

nlyr(
  drc_bioclim
)


# 5. Download and load land-cover variables

# Download the global land-cover covariates using the landcover()
# function from the geodata package.
#
# Each raster represents the proportion of a cell covered by a
# particular land-cover class.


# global_trees <- landcover(
#   var = "trees",
#   path = "data/downloads/landuse"
# )

# global_grassland <- landcover(
#   var = "grassland",
#   path = "data/downloads/landuse"
# )

# global_shrubs <- landcover(
#   var = "shrubs",
#   path = "data/downloads/landuse"
# )

# global_cropland <- landcover(
#   var = "cropland",
#   path = "data/downloads/landuse"
# )

# global_built <- landcover(
#   var = "built",
#   path = "data/downloads/landuse"
# )

# global_bare <- landcover(
#   var = "bare",
#   path = "data/downloads/landuse"
# )

# global_snow <- landcover(
#   var = "snow",
#   path = "data/downloads/landuse"
# )

# global_water <- landcover(
#   var = "water",
#   path = "data/downloads/landuse"
# )

# global_wetland <- landcover(
#   var = "wetland",
#   path = "data/downloads/landuse"
# )

# global_mangroves <- landcover(
#   var = "mangroves",
#   path = "data/downloads/landuse"
# )

# global_moss <- landcover(
#   var = "moss",
#   path = "data/downloads/landuse"
# )


# Load the downloaded land-cover raster files.

landcover_files <- list.files(
  "data/downloads/landuse",
  pattern = "\\.tif$",
  full.names = TRUE
)


# Combine the individual land-cover rasters into
# a single multi-layer SpatRaster.

global_landcover <- rast(
  landcover_files
)


# Confirm that the expected 11 land-cover layers were loaded.

nlyr(
  global_landcover
)


# Check the names of the land-cover variables.

names(
  global_landcover
)



# 6. Load the DRC administrative boundary

# Load the national boundary of the Democratic Republic of the Congo
# using the gadm() function from the geodata package.
#
# level = 0 returns the national boundary as a single spatial polygon.
#
# The downloaded boundary data are stored in data/downloads
# so they can be reused without downloading them again.

drc_shp <- gadm(
  country = "COD",
  level = 0,
  path = "data/downloads"
)


# Inspect the DRC national boundary.

drc_shp


# 7. Load DRC provincial boundaries

# Load the first-level administrative boundaries of the DRC.
#
# level = 1 returns the provincial boundaries, which are used
# to identify and select Kasaï-Central Province.

drc_shp_prv <- gadm(
  country = "COD",
  level = 1,
  path = "data/downloads"
)


# Inspect the provincial boundary dataset.

drc_shp_prv


# Confirm the number of provincial polygons.

nrow(
  drc_shp_prv
)


# Check the province names available in the dataset.

drc_shp_prv$NAME_1


# 8. Select Kasaï-Central Province

# Select Kasaï-Central from the DRC provincial boundaries.

kc <- drc_shp_prv |>
  filter(
    NAME_1 == "Kasaï-Central"
  )


# Confirm that only one provincial polygon was selected.

nrow(
  kc
)


# Inspect the Kasaï-Central boundary.

kc


# 9. Extract the Kasaï-Central spatial extent

# Extract the rectangular bounding box surrounding Kasaï-Central
# using the ext() function from the terra package.
#
# This extent will be used to crop the environmental rasters
# before masking them to the exact provincial boundary.

kc_ext <- ext(
  kc
)


# Inspect the Kasaï-Central extent.

kc_ext

# 10. Crop environmental rasters to Kasaï-Central extent

# Crop the bioclimatic and land-cover rasters to the rectangular
# extent surrounding Kasaï-Central using crop() from the terra package.
#
# Cropping reduces the amount of raster data that must be processed.
# Cells outside the actual provincial boundary are still present
# at this stage and will be removed in the next step.

bioclim_crop <- crop(
  drc_bioclim,
  kc_ext
)

landcover_crop <- crop(
  global_landcover,
  kc_ext
)


# 11. Mask environmental rasters to the Kasaï-Central boundary

# Mask the cropped rasters using the Kasaï-Central polygon.
#
# mask() sets raster cells outside the provincial boundary to NA,
# retaining environmental information only within Kasaï-Central.

bioclim_crop <- mask(
  bioclim_crop,
  kc
)

landcover_crop <- mask(
  landcover_crop,
  kc
)


# 12. Check the processed environmental rasters

# Confirm that all 19 bioclimatic variables and
# 11 land-cover variables are retained.

nlyr(
  bioclim_crop
)

nlyr(
  landcover_crop
)


# Check the names of the environmental variables.

names(
  bioclim_crop
)

names(
  landcover_crop
)


# Check the spatial resolution of both raster datasets.

res(
  bioclim_crop
)

res(
  landcover_crop
)


# Check the coordinate reference system (CRS).

crs(
  bioclim_crop,
  describe = TRUE
)$name

crs(
  landcover_crop,
  describe = TRUE
)$name


# Check that the bioclimatic and land-cover rasters have
# compatible spatial geometry.
#
# compareGeom() compares properties including extent,
# resolution, dimensions, and coordinate reference system.

compareGeom(
  bioclim_crop,
  landcover_crop,
  stopOnError = FALSE
)


# 13. Visually inspect the environmental rasters

# Plot the first 10 bioclimatic variables to inspect their
# spatial patterns across Kasaï-Central.

plot(
  bioclim_crop[[1:10]],
  axes = FALSE,
  col = idem(50, rev = TRUE)
)


# Plot the remaining 9 bioclimatic variables.

plot(
  bioclim_crop[[11:19]],
  axes = FALSE,
  col = idem(50, rev = TRUE)
)


# Plot the 11 land-cover variables to inspect their
# spatial patterns across Kasaï-Central.

plot(
  landcover_crop,
  axes = FALSE,
  col = idem(50, rev = TRUE)
)


# 14. Save the processed environmental rasters

# Save the cropped and masked bioclimatic and land-cover rasters.
#
# These intermediate rasters are retained so they can be reused
# in subsequent environmental and modelling analyses without
# repeating the cropping and masking steps.

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


# Confirm that both raster files were successfully saved.

file.exists(
  "data/clean/bioclim_crop.tif"
)

file.exists(
  "data/clean/landcover_crop.tif"
)


# 15. Load household coordinates

# Load the 650 household sampling locations across Kasaï-Central.
#
# Environmental covariate values will be extracted at these
# locations and used to define the environmental conditions
# represented by the mosquito sampling design.

coords <- read_csv(
  "data/clean/kc_household_coords.csv",
  col_types = cols(
    health_zone = col_character(),
    health_area = col_character(),
    village = col_character(),
    lat_dd = col_double(),
    long_dd = col_double()
  )
)


# Confirm the dimensions of the household coordinate dataset.

dim(
  coords
)


# Check the available household and coordinate variables.

names(
  coords
)


# Inspect the first few household records.

head(
  coords
)


# 16. Perform PCA on the bioclimatic variables

# Extract the 19 bioclimatic variables at each sampled
# household location.
#
# Each row represents a household and each column represents
# one bioclimatic variable.

bioclim_household <- terra::extract(
  bioclim_crop,
  select(
    coords,
    long_dd,
    lat_dd
  ),
  ID = FALSE
)


# Confirm that environmental values were extracted for
# the 650 households and 19 bioclimatic variables.

dim(
  bioclim_household
)


# Check for missing bioclimatic values before PCA.
#
# We expect no missing values at the sampled household locations.

sum(
  is.na(bioclim_household)
)


# Perform principal component analysis using prcomp().
#
# PCA reduces the 19 correlated bioclimatic variables into
# a smaller number of uncorrelated principal components that
# summarise the main climatic gradients represented across
# the sampled household locations.

pca_bioclim <- prcomp(
  bioclim_household,
  center = TRUE,
  scale. = TRUE
)


# Inspect the proportion of climatic variation explained
# by each principal component.

summary(
  pca_bioclim
)


# Identify the minimum number of principal components required
# to explain at least 90% of the climatic variation represented
# at the sampled household locations.

pca_bioclim_summary <- summary(
  pca_bioclim
)

n_bioclim_pcs_keep <- min(
  which(
    pca_bioclim_summary$importance["Cumulative Proportion", ] >= 0.90
  )
)


# Check the number of retained bioclimatic principal components.

n_bioclim_pcs_keep


# Apply the PCA transformation fitted at the household locations
# to every raster cell across Kasaï-Central.
#
# This converts the original 19 bioclimatic variables into
# spatial layers representing the same climatic gradients
# identified by the PCA.

bioclim_pcs <- predict(
  bioclim_crop,
  pca_bioclim
)


# Retain only the principal components required to explain
# at least 90% of the climatic variation.

bioclim_pcs_keep <- bioclim_pcs[[
  seq_len(n_bioclim_pcs_keep)
]]


# Confirm the number of retained bioclimatic PC layers.

nlyr(
  bioclim_pcs_keep
)


# Rename the retained principal-component layers to clearly
# identify them as bioclimatic predictors.

names(bioclim_pcs_keep) <- paste0(
  "bioclim_",
  tolower(
    names(bioclim_pcs_keep)
  )
)


# Check the final bioclimatic predictor names.

names(
  bioclim_pcs_keep
)


# Plot the retained bioclimatic principal components to inspect
# their spatial patterns across Kasaï-Central.

plot(
  bioclim_pcs_keep,
  axes = FALSE,
  col = idem(50, rev = TRUE)
)



# 17. Prepare land-cover variables for PCA

# Extract the 11 land-cover variables at each sampled
# household location.
#
# Each row represents a household and each column represents
# one land-cover class.

landcover_household <- terra::extract(
  landcover_crop,
  select(
    coords,
    long_dd,
    lat_dd
  ),
  ID = FALSE
)


# Confirm the dimensions of the extracted land-cover data.

dim(
  landcover_household
)


# Check the land-cover variable names.

names(
  landcover_household
)


# Check for missing land-cover values at household locations.

sum(
  is.na(landcover_household)
)


# Calculate the variance of each land-cover class across
# the sampled household locations.
#
# Variables with no variation cannot contribute information
# to PCA and should therefore be excluded.

landcover_variance <- apply(
  landcover_household,
  2,
  var,
  na.rm = TRUE
)


# Identify the land-cover classes that show variation
# across household locations.

landcover_layers_varying <- which(
  landcover_variance != 0
)


# Check the land-cover classes retained for PCA.

names(
  landcover_household
)[landcover_layers_varying]


# Check the land-cover classes excluded because
# they have no variation.

names(
  landcover_household
)[-landcover_layers_varying]


# Retain only the land-cover raster layers that show
# variation across sampled household locations.

landcover_crop_sub <- landcover_crop[[
  landcover_layers_varying
]]


# Apply an empirical-logit transformation to the retained
# land-cover fractions before PCA.
#
# Land-cover values are bounded between 0 and 1.
# The empirical-logit transformation converts these fractions
# to an approximately unbounded scale while allowing values
# equal to 0 or 1 to be transformed.
#
# trials = 1e4 reflects the four-decimal precision
# of the land-cover fractions.

emplogit_fraction <- function(fraction, trials = 1e4) {
  
  successes <- trials * fraction
  failures <- trials - successes
  
  log(
    (successes + 0.5) /
      (failures + 0.5)
  )
}


# Transform the retained land-cover raster layers.

landcover_crop_emplogit <- emplogit_fraction(
  landcover_crop_sub
)


# 18. Perform PCA on the land-cover variables

# Extract the transformed land-cover values at the
# sampled household locations.
#
# These transformed values will be used to fit the
# land-cover PCA.

landcover_household_emplogit <- terra::extract(
  landcover_crop_emplogit,
  select(
    coords,
    long_dd,
    lat_dd
  ),
  ID = FALSE
)


# Check for missing values before PCA.

sum(
  is.na(landcover_household_emplogit)
)


# Perform principal component analysis.
#
# PCA reduces the correlated land-cover variables into
# a smaller number of uncorrelated principal components
# representing the main land-cover gradients observed
# across sampled household locations.

pca_landcover <- prcomp(
  landcover_household_emplogit,
  center = TRUE,
  scale. = TRUE
)


# Inspect the proportion of land-cover variation explained
# by each principal component.

summary(
  pca_landcover
)


# Identify the minimum number of principal components required
# to explain at least 90% of the land-cover variation represented
# at the sampled household locations.

pca_landcover_summary <- summary(
  pca_landcover
)

n_landcover_pcs_keep <- min(
  which(
    pca_landcover_summary$importance["Cumulative Proportion", ] >= 0.90
  )
)


# Check the number of retained land-cover principal components.

n_landcover_pcs_keep


# Apply the PCA transformation fitted at the household locations
# to every raster cell across Kasaï-Central.

landcover_pcs <- predict(
  landcover_crop_emplogit,
  pca_landcover
)


# Retain only the principal components required to explain
# at least 90% of the land-cover variation.

landcover_pcs_keep <- landcover_pcs[[
  seq_len(n_landcover_pcs_keep)
]]


# Confirm the number of retained land-cover PC layers.

nlyr(
  landcover_pcs_keep
)


# Rename the retained principal-component layers to clearly
# identify them as land-cover predictors.

names(landcover_pcs_keep) <- paste0(
  "landcover_pc",
  seq_len(n_landcover_pcs_keep)
)


# Check the final land-cover predictor names.

names(
  landcover_pcs_keep
)


# Plot the retained land-cover principal components to inspect
# their spatial patterns across Kasaï-Central.

plot(
  landcover_pcs_keep,
  axes = FALSE,
  col = idem(50, rev = TRUE)
)


# 19. Combine the retained environmental covariates

# Combine the retained bioclimatic and land-cover principal
# components into a single multi-layer raster.
#
# These layers represent the final environmental predictors
# that will be used in the species distribution models.

covariates_crop <- c(
  bioclim_pcs_keep,
  landcover_pcs_keep
)


# Confirm the total number of environmental predictor layers.
#
# We expect:
# - 4 bioclimatic principal components; and
# - 5 land-cover principal components.
#
# This should give a total of 9 environmental predictors.

nlyr(
  covariates_crop
)


# Check the names of the final environmental predictors.

names(
  covariates_crop
)


# Check that the combined predictor stack retains
# the expected spatial geometry.

ext(
  covariates_crop
)

res(
  covariates_crop
)

crs(
  covariates_crop,
  describe = TRUE
)$name


# Plot the final environmental predictors to inspect
# their spatial patterns across Kasaï-Central.

plot(
  covariates_crop,
  axes = FALSE,
  col = idem(50, rev = TRUE)
)

# 20. Inspect land-cover PCA loadings

# Inspect the PCA loadings to understand how strongly each
# original land-cover variable contributes to each retained
# principal component.
#
# Larger positive or negative loadings indicate variables
# that contribute more strongly to a given principal component.

pca_landcover$rotation


# Retain and round the loadings for the principal components
# selected for modelling.

landcover_loadings <- round(
  pca_landcover$rotation[, 1:n_landcover_pcs_keep],
  3
)


# Inspect the retained land-cover PCA loadings.

landcover_loadings


# 21. Save the Kasaï-Central environmental covariates

# Save the final bioclimatic and land-cover principal components
# as a single multi-layer raster.
#
# This raster contains the environmental predictors that will
# later be used to fit the species distribution models.

writeRaster(
  covariates_crop,
  "data/clean/covariates.tif",
  overwrite = TRUE
)


# Confirm that the environmental covariate raster was saved.

file.exists(
  "data/clean/covariates.tif"
)


# Reload the saved raster to verify that the file can be read
# correctly and contains the expected predictor layers.

covariates_check <- rast(
  "data/clean/covariates.tif"
)


# Confirm the number of predictor layers.

nlyr(
  covariates_check
)


# Check the predictor names.

names(
  covariates_check
)

# 22. Prepare bioclimatic PCA covariates for the whole DRC

# Crop and mask the original DRC bioclimatic rasters
# to the national boundary.
#
# This removes raster cells outside the Democratic Republic
# of the Congo while retaining the 19 original bioclimatic variables.

drc_bioclim_masked <- drc_bioclim |>
  crop(
    drc_shp
  ) |>
  mask(
    drc_shp
  )


# Confirm that the expected 19 bioclimatic variables are retained.

nlyr(
  drc_bioclim_masked
)


# Check the bioclimatic variable names.

names(
  drc_bioclim_masked
)


# Apply the bioclimatic PCA fitted from the Kasaï-Central
# household environmental conditions to every raster cell
# across the DRC.
#
# Using the same PCA transformation ensures that environmental
# conditions across the country are represented on the same
# climatic gradients used for model development in Kasaï-Central.

drc_bioclim_pcs <- predict(
  drc_bioclim_masked,
  pca_bioclim
)


# Retain the same bioclimatic principal components
# selected for the Kasaï-Central analysis.

drc_bioclim_pcs_keep <- drc_bioclim_pcs[[
  seq_len(n_bioclim_pcs_keep)
]]


# Give the DRC bioclimatic PC layers the same names
# as the corresponding Kasaï-Central predictors.

names(drc_bioclim_pcs_keep) <- names(
  bioclim_pcs_keep
)


# Confirm the number of retained DRC bioclimatic PC layers.

nlyr(
  drc_bioclim_pcs_keep
)


# Check that the predictor names match the
# Kasaï-Central bioclimatic predictors.

names(
  drc_bioclim_pcs_keep
)

identical(
  names(drc_bioclim_pcs_keep),
  names(bioclim_pcs_keep)
)


# 23. Prepare land-cover PCA covariates for the whole DRC

# Identify the land-cover classes that showed variation across
# the Kasaï-Central household sampling locations.
#
# Only these variables were used to fit the land-cover PCA,
# so the same variables must be retained for the DRC projection.

landcover_classes_varying <- names(
  landcover_household
)[landcover_layers_varying]


# Check the land-cover classes retained for PCA.

landcover_classes_varying


# Select the same land-cover classes from the national raster stack,
# then crop and mask them to the DRC national boundary.

drc_landcover_varying <- global_landcover[[
  landcover_classes_varying
]] |>
  crop(
    drc_shp
  ) |>
  mask(
    drc_shp
  )


# Confirm the number of retained land-cover variables.

nlyr(
  drc_landcover_varying
)


# Check that the retained variable names match those used
# to fit the Kasaï-Central land-cover PCA.

names(
  drc_landcover_varying
)

identical(
  names(drc_landcover_varying),
  landcover_classes_varying
)


# Apply the same empirical-logit transformation used
# for the Kasaï-Central land-cover variables.

drc_landcover_emplogit <- emplogit_fraction(
  drc_landcover_varying
)


# Apply the land-cover PCA fitted from the Kasaï-Central
# household environmental conditions to every raster cell
# across the DRC.
#
# This ensures that land-cover conditions across the country
# are represented using the same environmental gradients
# used during model development.

drc_landcover_pcs <- predict(
  drc_landcover_emplogit,
  pca_landcover
)


# Retain the same land-cover principal components selected
# for the Kasaï-Central analysis.

drc_landcover_pcs_keep <- drc_landcover_pcs[[
  seq_len(n_landcover_pcs_keep)
]]


# Give the DRC land-cover PC layers the same names
# as the corresponding Kasaï-Central predictors.

names(drc_landcover_pcs_keep) <- names(
  landcover_pcs_keep
)


# Confirm the number of retained DRC land-cover PC layers.

nlyr(
  drc_landcover_pcs_keep
)


# Check that the predictor names match the
# Kasaï-Central land-cover predictors.

names(
  drc_landcover_pcs_keep
)

identical(
  names(drc_landcover_pcs_keep),
  names(landcover_pcs_keep)
)


# Check that the DRC bioclimatic and land-cover PC rasters
# have compatible spatial geometry before combining them.

compareGeom(
  drc_bioclim_pcs_keep,
  drc_landcover_pcs_keep,
  stopOnError = FALSE
)


# 24. Combine the DRC environmental covariates

# Combine the retained bioclimatic and land-cover principal
# components into a single multi-layer raster for the whole DRC.
#
# This raster contains the environmental predictors that will
# later be used for national-scale spatial prediction.

drc_covariates <- c(
  drc_bioclim_pcs_keep,
  drc_landcover_pcs_keep
)


# Confirm the total number of environmental predictor layers.
#
# We expect:
# - 4 bioclimatic principal components; and
# - 5 land-cover principal components.
#
# This should give a total of 9 environmental predictors.

nlyr(
  drc_covariates
)


# Check the names of the final DRC environmental predictors.

names(
  drc_covariates
)


# Confirm that the DRC predictor names match the
# Kasaï-Central predictor names exactly.

identical(
  names(drc_covariates),
  names(covariates_crop)
)


# Save the final DRC environmental predictor stack
# for national prediction and environmental similarity analyses.

writeRaster(
  drc_covariates,
  "data/clean/drc_covariates.tif",
  overwrite = TRUE
)


# Confirm that the DRC covariate raster was successfully saved.

file.exists(
  "data/clean/drc_covariates.tif"
)


# Reload the saved raster to verify that it can be read
# correctly and contains the expected predictor layers.

drc_covariates_check <- rast(
  "data/clean/drc_covariates.tif"
)


# Confirm the number of saved predictor layers.

nlyr(
  drc_covariates_check
)


# Check the saved predictor names.

names(
  drc_covariates_check
)

# 25. Compare bioclimatic PCs between Kasaï-Central and the whole DRC

# Compare each retained bioclimatic principal component between
# Kasaï-Central and the whole DRC.
#
# For each principal component, the KC and DRC maps use the same
# value range so that their climatic gradients can be compared directly.
#
# The shared range is calculated automatically rather than
# being entered manually.

par(
  mfrow = c(2, n_bioclim_pcs_keep)
)


for (i in seq_len(n_bioclim_pcs_keep)) {
  
  # Calculate the common minimum and maximum values
  # for the corresponding KC and DRC principal component.
  
  shared_range <- range(
    c(
      minmax(bioclim_pcs_keep[[i]]),
      minmax(drc_bioclim_pcs_keep[[i]])
    ),
    na.rm = TRUE
  )
  
  
  # Plot the Kasaï-Central principal component.
  
  plot(
    bioclim_pcs_keep[[i]],
    range = shared_range,
    main = paste0(
      "KC ",
      names(bioclim_pcs_keep)[i]
    ),
    col = idem(50, rev = TRUE)
  )
  
  
  # Plot the corresponding DRC principal component
  # using exactly the same value range.
  
  plot(
    drc_bioclim_pcs_keep[[i]],
    range = shared_range,
    main = paste0(
      "DRC ",
      names(drc_bioclim_pcs_keep)[i]
    ),
    col = idem(50, rev = TRUE)
  )
}


# Return the plotting window to a single panel.

par(
  mfrow = c(1, 1)
)


# 26. Compare land-cover PCs between Kasaï-Central and the whole DRC

# Compare each retained land-cover principal component between
# Kasaï-Central and the whole DRC.
#
# For each principal component, the KC and DRC maps use the same
# value range so that their land-cover gradients can be compared directly.
#
# The shared range is calculated automatically rather than
# being entered manually.

par(
  mfrow = c(n_landcover_pcs_keep, 2)
)


for (i in seq_len(n_landcover_pcs_keep)) {
  
  # Calculate the common minimum and maximum values
  # for the corresponding KC and DRC principal component.
  
  shared_range <- range(
    c(
      minmax(landcover_pcs_keep[[i]]),
      minmax(drc_landcover_pcs_keep[[i]])
    ),
    na.rm = TRUE
  )
  
  
  # Plot the Kasaï-Central principal component.
  
  plot(
    landcover_pcs_keep[[i]],
    range = shared_range,
    main = paste0(
      "KC ",
      names(landcover_pcs_keep)[i]
    ),
    col = idem(50, rev = TRUE)
  )
  
  
  # Plot the corresponding DRC principal component
  # using exactly the same value range.
  
  plot(
    drc_landcover_pcs_keep[[i]],
    range = shared_range,
    main = paste0(
      "DRC ",
      names(drc_landcover_pcs_keep)[i]
    ),
    col = idem(50, rev = TRUE)
  )
}


# Return the plotting window to a single panel.

par(
  mfrow = c(1, 1)
)


# 27. Extract environmental covariates at household locations

# Extract the final environmental predictor values from the
# Kasaï-Central raster stack at each sampled household location.
#
# Each row represents one household location and each column
# represents one environmental predictor.

coord_covs <- terra::extract(
  covariates_crop,
  select(
    coords,
    long_dd,
    lat_dd
  ),
  ID = FALSE
)


# Confirm that environmental predictors were extracted for
# the expected 650 household locations.

dim(
  coord_covs
)


# Check the environmental predictor names.
#
# We expect:
# - 4 bioclimatic principal components; and
# - 5 land-cover principal components.

names(
  coord_covs
)


# Check for missing environmental predictor values.

sum(
  is.na(coord_covs)
)


# Inspect the first few extracted records.

head(
  coord_covs
)


# 28. Combine household information and environmental covariates

# Combine the household identifiers and geographic coordinates
# with their corresponding environmental predictor values.
#
# The row order is preserved because the environmental values
# were extracted directly using the coordinates in coords.

coords_covariates <- bind_cols(
  coords,
  coord_covs
)


# Confirm the dimensions of the final household
# environmental covariate dataset.

dim(
  coords_covariates
)


# Check the variable names.

names(
  coords_covariates
)


# Inspect the first few records.

head(
  coords_covariates
)


# 29. Save household environmental covariates

# Save the final household-level environmental covariate dataset.
#
# This file will be used later to join the environmental predictors
# to the repeated mosquito count observations during model preparation.

write_csv(
  coords_covariates,
  "data/clean/coords_covariates.csv"
)


# Confirm that the file was successfully saved.

file.exists(
  "data/clean/coords_covariates.csv"
)
