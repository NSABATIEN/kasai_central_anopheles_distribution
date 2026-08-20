
# 1. Load packages

# Load all packages required for data management,
# spatial processing and statistical modelling.

source(
  "R/packages.R"
)

# 2. Load mosquito count data and household environmental covariates

# Use the read_csv() function from the readr package
# to load the cleaned Anopheles count dataset.

# This dataset contains species-specific mosquito counts
# recorded across the sampled households and collection rounds.

count_data <- read_csv(
  "data/clean/kc_anopheles_count_data.csv",
  show_col_types = FALSE
)

# 3. Create household-level Anopheles counts for the full study period

# The species-distribution model describes the overall spatial
# distribution of Anopheles taxa across the complete study period.

# Combine mosquito counts from all collection rounds for each
# household and taxon.

# After this step, collection month and collection round are no longer
# part of the modelling dataset.

counts <- count_data |>
  group_by(
    health_zone,
    health_area,
    village,
    house_number,
    identification_taxon
  ) |>
  summarise(
    count = sum(
      species_count,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  rename(
    species = identification_taxon
  ) |>
  mutate(
    species = factor(
      species
    )
  )

# Inspect the structure of the household-level count dataset.

counts |>
  glimpse()

# Check the number of rows and columns.

dim(
  counts
)

# Confirm that pooling the collection rounds has preserved
# the total number of mosquitoes in the study.

sum(
  counts$count
)

# 4. Load household coordinates and environmental covariates

# Use the read_csv() function from the readr package
# to load the geographic coordinates of the 650 sampled households.

# The household coordinates will be used to extract the environmental
# covariate values at each mosquito sampling location.

coords <- read_csv(
  "data/clean/kc_household_coords.csv",
  show_col_types = FALSE
)

coords |>
  glimpse()


dim(
  coords
)

# Load the spatial environmental covariates prepared previously.

# These raster layers contain the environmental PCA variables
# that will be used as predictors in the species-distribution model.

covs <- rast(
  "data/clean/covariates.tif"
)

covs

names(
  covs
)

# 5. Extract environmental covariates at sampled household locations

# Use the extract() function from the terra package
# to obtain the values of the nine environmental PCA layers
# at each of the 650 sampled household locations.

# bind_cols() combines the household identifiers with the
# environmental values extracted from the raster.

# Latitude, longitude and GPS precision are then removed because
# they are not predictors in the species-distribution model.

coord_covs <- coords |>
  bind_cols(
    terra::extract(
      covs,
      select(
        coords,
        long_dd,
        lat_dd
      ),
      ID = FALSE
    )
  ) |>
  select(
    -lat_dd,
    -long_dd,
    -precision
  )

coord_covs |>
  glimpse()

dim(
  coord_covs
)

# Confirm that all sampled households received values
# for each of the nine environmental PCA covariates.

coord_covs |>
  summarise(
    across(
      c(
        starts_with("bioclim_pc"),
        starts_with("landcover_pc")
      ),
      ~ sum(is.na(.x))
    )
  )

# 6. JCombine household counts with environmental covariates

# Join the household-level mosquito count data with the
# environmental PCA values extracted at each sampled household.

# The resulting df object is the dataset that will be used
# to fit the hierarchical species-distribution model.

model_data <- counts |>
  left_join(
    coord_covs,
    by = c(
      "health_zone",
      "health_area",
      "village",
      "house_number"
    )
  )

model_data |>
  glimpse()

dim(
  model_data
)

# 7. Check the final modelling dataset

# Inspect the structure of the final modelling dataset.

# Each row should represent one household × one Anopheles taxon,
# with the corresponding mosquito count and environmental covariates.

model_data |>
  glimpse()

# Confirm the dimensions of the modelling dataset.
# We expect:
# 650 households × 7 taxa = 4,550 observations.

dim(
  model_data
)


# Confirm that all household observations received values
# for each of the nine environmental PCA predictors.

# We expect zero missing values for every predictor.

model_data |>
  summarise(
    across(
      c(
        starts_with("bioclim_pc"),
        starts_with("landcover_pc")
      ),
      ~ sum(is.na(.x))
    )
  )

# Confirm that the join did not duplicate or lose mosquito counts.

sum(
  model_data$count
)

# 8. Examine mosquito count distribution by taxon

# Summarise the household-level mosquito counts
# separately for each Anopheles taxon.

# These summaries help assess:
#   - differences in sampling support among taxa;
#   - the proportion of households with zero mosquitoes;
#   - variation in mosquito counts among households;
#   - whether the count data are overdispersed.

taxon_count_summary <- model_data |>
  group_by(
    species
  ) |>
  summarise(
    n_households = n(),
    total_count = sum(count),
    mean_count = mean(count),
    variance_count = var(count),
    maximum_count = max(count),
    zero_households = sum(count == 0),
    percent_zero =
      100 * zero_households / n_households,
    .groups = "drop"
  )

taxon_count_summary


# 9. Quantify modelling support and overdispersion by taxon

# Calculate the number of households with at least one mosquito
# and the variance-to-mean ratio for each Anopheles taxon.

# A variance-to-mean ratio greater than 1 indicates
# overdispersion relative to a Poisson distribution.

taxon_model_support <- taxon_count_summary |>
  mutate(
    nonzero_households =
      n_households - zero_households,
    
    variance_to_mean =
      variance_count / mean_count
  ) |>
  select(
    species,
    total_count,
    nonzero_households,
    percent_zero,
    mean_count,
    variance_count,
    variance_to_mean,
    maximum_count
  )

print(
  taxon_model_support,
  n = 7,
  width = Inf
)


# 10. Fit the hierarchical negative-binomial GAM

# Use the gam() function from the mgcv package
# to model household-level Anopheles mosquito counts
# as a function of environmental covariates.

# SIMPLIFIED MODEL STRUCTURE

# count ~ species
#
#         + shared smooth effects of bioclimatic PCs
#
#         + shared smooth effects of land-cover PCs
#
#         + taxon-specific responses to bioclimatic PCs
#
#         + taxon-specific responses to land-cover PCs
#

# The response variable is the total number of mosquitoes
# of a given Anopheles taxon collected at a household
# across the complete study period.

# Counts from the 12 collection rounds were combined before modelling.
# Therefore, collection round is not included as a temporal term
# in this model.

# The model uses a negative-binomial distribution because
# household mosquito counts are overdispersed, particularly
# for the dominant Anopheles taxa.

# The model is hierarchical because the environmental responses contain:

#   1. shared environmental effects across all Anopheles taxa; and

#   2. taxon-specific deviations from those shared responses.

# The shared smooth terms describe broad relationships between
# Anopheles mosquito counts and climatic or land-cover gradients.

# The taxon-specific terms allow individual taxa to deviate
# from these shared environmental responses.

# This structure allows information about broad environmental
# responses to be shared across taxa while still allowing
# individual taxa to respond differently to environmental conditions.

# The model includes four bioclimatic principal components
# and five land-cover principal components.

# A negative-binomial observation model is used to account
# for overdispersion in the household-level mosquito counts.

# The model is fitted using Restricted Maximum Likelihood (REML).

anopheles_hierarchical_gam <- gam(
  count ~
    
    # Separate baseline count for each Anopheles taxon
    1 + species +
    
    # Shared responses to bioclimatic conditions
    s(bioclim_pc1) +
    s(bioclim_pc2) +
    s(bioclim_pc3) +
    s(bioclim_pc4) +
    
    # Shared responses to land-cover conditions
    s(landcover_pc1) +
    s(landcover_pc2) +
    s(landcover_pc3) +
    s(landcover_pc4) +
    s(landcover_pc5) +
    
    # Taxon-specific responses to bioclimatic conditions
    s(bioclim_pc1, species, bs = "re") +
    s(bioclim_pc2, species, bs = "re") +
    s(bioclim_pc3, species, bs = "re") +
    s(bioclim_pc4, species, bs = "re") +
    
    # Taxon-specific responses to land-cover conditions
    s(landcover_pc1, species, bs = "re") +
    s(landcover_pc2, species, bs = "re") +
    s(landcover_pc3, species, bs = "re") +
    s(landcover_pc4, species, bs = "re") +
    s(landcover_pc5, species, bs = "re"),
  
  # Negative-binomial observation model
  family = nb(),
  
  # Fit the model using Restricted Maximum Likelihood
  method = "REML",
  
  data = model_data
)

summary(
  anopheles_hierarchical_gam
)

# 11. Prepare species-specific layers for spatial prediction

# The fitted hierarchical GAM will be used to predict
# Anopheles mosquito counts across Kasaï-Central.

# Predictions will be generated separately for each
# of the seven Anopheles taxa included in the model.

# First, confirm the exact taxon names used in the fitted model.

# These names must be reproduced exactly when creating
# the taxon-specific prediction layers.

levels(
  model_data$species
)

# Create a template raster for the species variable.

# Use the first environmental covariate layer only to inherit
# the spatial extent, resolution and coordinate reference system
# of the prediction grid.

# Multiplying the layer by zero creates a raster with the
# correct spatial structure but no environmental information.

# Rename this raster "species" so that it matches the
# species variable used in the fitted hierarchical GAM.

cov_species_dummy <- covs[[1]] * 0

names(
  cov_species_dummy
) <- "species"

# Create one copy of the species template raster
# for each Anopheles taxon included in the model.

# Each raster has exactly the same spatial structure
# as the environmental covariates.

species_layer_funestus <-
  species_layer_gambiae <-
  species_layer_hancocki <-
  species_layer_moucheti <-
  species_layer_paludis <-
  species_layer_an_sp <-
  species_layer_ziemanni <-
  cov_species_dummy

# Assign the corresponding taxon name to each species layer.

# These names must exactly match the factor levels
# used when fitting the hierarchical GAM.

species_layer_funestus[] <- "An. funestus gp"

species_layer_gambiae[] <- "An. gambiae s.l."

species_layer_hancocki[] <- "An. hancocki"

species_layer_moucheti[] <- "An. moucheti"

species_layer_paludis[] <- "An. paludis"

species_layer_an_sp[] <- "An. sp."

species_layer_ziemanni[] <- "An. ziemanni"


# 12. Generate taxon-specific spatial predictions

# Use the predict() function from the terra package
# to apply the fitted hierarchical GAM across the
# environmental covariate surfaces of Kasaï-Central.

# Predictions are generated separately for each Anopheles taxon
# by combining the nine environmental covariates with the
# corresponding taxon-specific species layer.

# type = "response" returns predictions on the original count scale.

# Because the model was fitted to mosquito counts pooled across
# the complete study period, each raster represents the predicted
# Anopheles count for that taxon under the fitted model.

predicted_count_funestus <- predict(
  c(
    covs,
    species_layer_funestus
  ),
  anopheles_hierarchical_gam,
  type = "response"
)

predicted_count_gambiae <- predict(
  c(
    covs,
    species_layer_gambiae
  ),
  anopheles_hierarchical_gam,
  type = "response"
)

predicted_count_hancocki <- predict(
  c(
    covs,
    species_layer_hancocki
  ),
  anopheles_hierarchical_gam,
  type = "response"
)

predicted_count_moucheti <- predict(
  c(
    covs,
    species_layer_moucheti
  ),
  anopheles_hierarchical_gam,
  type = "response"
)

predicted_count_paludis <- predict(
  c(
    covs,
    species_layer_paludis
  ),
  anopheles_hierarchical_gam,
  type = "response"
)

predicted_count_an_sp <- predict(
  c(
    covs,
    species_layer_an_sp
  ),
  anopheles_hierarchical_gam,
  type = "response"
)

predicted_count_ziemanni <- predict(
  c(
    covs,
    species_layer_ziemanni
  ),
  anopheles_hierarchical_gam,
  type = "response"
)


# Assign informative layer names corresponding to
# the Anopheles taxa represented by each prediction.

names(predicted_count_funestus) <- "An. funestus gp"

names(predicted_count_gambiae) <- "An. gambiae s.l."

names(predicted_count_hancocki) <- "An. hancocki"

names(predicted_count_moucheti) <- "An. moucheti"

names(predicted_count_paludis) <- "An. paludis"

names(predicted_count_an_sp) <- "An. sp."

names(predicted_count_ziemanni) <- "An. ziemanni"

# 13. Combine taxon-specific predictions and calculate detection probability

# Combine the seven taxon-specific predicted count rasters
# into a single multi-layer spatial object.

# Each layer represents the predicted mosquito count
# for one Anopheles taxon across Kasaï-Central.

predicted_counts <- c(
  predicted_count_funestus,
  predicted_count_gambiae,
  predicted_count_hancocki,
  predicted_count_moucheti,
  predicted_count_paludis,
  predicted_count_an_sp,
  predicted_count_ziemanni
)

predicted_counts

names(
  predicted_counts
)


# 13. Convert predicted counts to household detection probability

# Convert the predicted mosquito counts to the expected count per household.

# In the Kasaï-Central sampling design, 25 households were sampled
# within each health-zone sampling site.

# Therefore, divide the predicted count by 25.

expected_count_per_household <- predicted_counts / 25


# Convert the expected mosquito count per household
# to the probability of detecting at least one mosquito.

# The probability is calculated as:

#   P(detection) = 1 - exp(-expected count per household)

# The resulting values are bounded between 0 and 1.

household_detection_probability <-
  1 - exp(
    -expected_count_per_household
  )

household_detection_probability


# 14. Restrict predictions to Kasaï-Central and plot the distribution maps

# Use the vect() function from the terra package
# to load the boundaries of the 26 health zones included
# in the Kasaï-Central study.

kasai_central_health_zones <- vect(
  "data/clean/kc_health_zones.gpkg"
)


# Dissolve the 26 health-zone polygons to create
# a single boundary for Kasaï-Central.

kasai_central_boundary <- aggregate(
  kasai_central_health_zones
)


# Mask the household detection probability rasters
# to the Kasaï-Central study boundary.

# This removes raster cells located outside the province
# while preserving the predicted probabilities within it.

household_detection_probability_kc <- mask(
  household_detection_probability,
  kasai_central_boundary
)

# 15. Define the colour scale for Anopheles probability maps

# Create a common sequential Green–blue colour scale
# for all seven Anopheles taxa.

# Low household detection probabilities are represented
# by very light colours.

# As probability increases, colours progress through
# green and turquoise to dark blue.

# The same colour scale is used for every taxon so that
# probability values can be compared directly across maps.

probability_colours <- colorRampPalette(
  c(
    "#F7FCF0",
    "#C7E9C0",
    "#7FCDBB",
    "#41B6C4",
    "#2C7FB8",
    "#253494"
  )
)(
  100
)

# 16. Plot taxon-specific model predictions

# Plot the predicted household probability of detection
# for each Anopheles taxon across Kasaï-Central.

# The prediction surfaces are generated from the fitted
# hierarchical negative-binomial GAM.

# The Green-blue colour scale represents the predicted
# household probability of detection from 0 to 1.

# Observed mosquito counts at surveyed sites are overlaid
# as proportional black circles.

# A common proportional-circle scale is used across all taxa
# so that the same observed count produces the same circle size
# on every taxon-specific prediction map.

# Only the external Kasaï-Central boundary is displayed
# so that the prediction surfaces remain visually clear.


# Combine household-level mosquito counts within each surveyed site.
#
# total_count represents the total number of mosquitoes of each
# Anopheles taxon collected across the 25 sampled households
# during the complete study period.

# Mean household coordinates represent the geographic position
# of each surveyed site.

observed_site_counts <- counts |>
  left_join(
    coords |>
      select(
        health_zone,
        health_area,
        village,
        house_number,
        long_dd,
        lat_dd
      ),
    by = c(
      "health_zone",
      "health_area",
      "village",
      "house_number"
    )
  ) |>
  group_by(
    species,
    health_zone,
    health_area,
    village
  ) |>
  summarise(
    total_count = sum(
      count,
      na.rm = TRUE
    ),
    long_dd = mean(
      long_dd,
      na.rm = TRUE
    ),
    lat_dd = mean(
      lat_dd,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Identify the maximum observed site count across all taxa.

maximum_site_count_all_taxa <- max(
  observed_site_counts$total_count,
  na.rm = TRUE
)


# Create a common proportional-circle size for all taxa.

# Every site with a positive mosquito count receives a minimum
# visible point size.

# Point size then increases with the square root of the observed
# count relative to the maximum count across all taxa.

# Using the same formula for every taxon ensures that identical
# mosquito counts are represented by identical circle sizes.

observed_site_counts <- observed_site_counts |>
  mutate(
    point_size =
      1.8 +
      4.2 * sqrt(
        total_count / maximum_site_count_all_taxa
      )
  )

# 16A. Plot An. gambiae s.l. model prediction

# Extract the predicted household detection probability
# for An. gambiae s.l.

gambiae_detection_probability <-
  household_detection_probability_kc[["An. gambiae s.l."]]


# Select surveyed sites where An. gambiae s.l. was observed.

# Sites with zero observed mosquitoes are not displayed
# as proportional count circles.

gambiae_site_counts <- observed_site_counts |>
  filter(
    species == "An. gambiae s.l.",
    total_count > 0
  )

# Create the An. gambiae s.l. model-prediction map.

# The raster represents the predicted household probability
# of detection from the hierarchical GAM.

# Black proportional circles represent the observed total
# An. gambiae s.l. count at each surveyed site.

# Larger circles indicate larger observed mosquito counts.

gambiae_prediction_map <- ggplot() +
  
  # Add the predicted household probability surface.
  tidyterra::geom_spatraster(
    data = gambiae_detection_probability
  ) +
  
  # Apply the common Green-blue probability scale.
  scale_fill_gradientn(
    colours = probability_colours,
    limits = c(
      0,
      1
    ),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    labels = c(
      "0",
      "0.25",
      "0.50",
      "0.75",
      "1.00"
    ),
    name = "Household\nprobability\nof detection",
    na.value = "white",
    guide = guide_colourbar(
      barheight = grid::unit(
        4,
        "cm"
      ),
      barwidth = grid::unit(
        0.35,
        "cm"
      ),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  # Add only the external Kasaï-Central boundary.
  tidyterra::geom_spatvector(
    data = kasai_central_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.4
  ) +
  
  # Overlay observed mosquito counts at surveyed sites.
  # A thin white outline keeps the black circles visible
  # over both light and dark areas of the prediction surface.
  geom_point(
    mapping = aes(
      x = long_dd,
      y = lat_dd,
      size = point_size
    ),
    data = gambiae_site_counts,
    shape = 21,
    fill = "black",
    colour = "white",
    stroke = 0.4
  ) +
  
  # Use the common proportional point sizes calculated above.
  scale_size_identity(
    guide = "none"
  ) +
  
  # Add the taxon name in italics.
  labs(
    title = expression(
      italic("An. gambiae s.l.")
    )
  ) +
  
  # Remove axes and background.
  theme_void() +
  
  # Format the title and probability legend.
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 14
    ),
    
    legend.position = "right",
    
    legend.box.spacing = grid::unit(
      0.15,
      "cm"
    ),
    
    legend.margin = margin(
      l = 0,
      r = 0,
      t = 0,
      b = 0
    )
  )

gambiae_prediction_map

# 16B. Plot An. funestus gp model prediction

# Extract the predicted household detection probability
# for An. funestus gp.

funestus_detection_probability <-
  household_detection_probability_kc[["An. funestus gp"]]


# Select surveyed sites where An. funestus gp was observed.
# Sites with zero observed mosquitoes are not displayed
# as proportional count circles.
# point_size already uses the common count scale defined
# across all seven Anopheles taxa.

funestus_site_counts <- observed_site_counts |>
  filter(
    species == "An. funestus gp",
    total_count > 0
  )

# Create the An. funestus gp model-prediction map.

# The raster represents the predicted household probability
# of detection from the hierarchical GAM.

# Black proportional circles represent the observed total
# An. funestus gp count at each surveyed site.
#
# Circle sizes use the same count scale as the
# An. gambiae s.l. prediction map.

funestus_prediction_map <- ggplot() +
  
  # Add the predicted household probability surface.
  tidyterra::geom_spatraster(
    data = funestus_detection_probability
  ) +
  
  # Apply the common Green-blue probability scale.
  scale_fill_gradientn(
    colours = probability_colours,
    limits = c(
      0,
      1
    ),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    labels = c(
      "0",
      "0.25",
      "0.50",
      "0.75",
      "1.00"
    ),
    name = "Household\nprobability\nof detection",
    na.value = "white",
    guide = guide_colourbar(
      barheight = grid::unit(
        4,
        "cm"
      ),
      barwidth = grid::unit(
        0.35,
        "cm"
      ),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  # Add only the external Kasaï-Central boundary.
  tidyterra::geom_spatvector(
    data = kasai_central_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.4
  ) +
  
  # Overlay observed mosquito counts at surveyed sites.
  geom_point(
    mapping = aes(
      x = long_dd,
      y = lat_dd,
      size = point_size
    ),
    data = funestus_site_counts,
    shape = 21,
    fill = "black",
    colour = "white",
    stroke = 0.4
  ) +
  
  # Use the common proportional point sizes calculated
  # across all seven taxa.
  scale_size_identity(
    guide = "none"
  ) +
  
  # Add the taxon name in italics.
  labs(
    title = expression(
      italic("An. funestus gp")
    )
  ) +
  
  # Remove axes and background.
  theme_void() +
  
  # Format the title and probability legend.
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 14
    ),
    
    legend.position = "right",
    
    legend.box.spacing = grid::unit(
      0.15,
      "cm"
    ),
    
    legend.margin = margin(
      l = 0,
      r = 0,
      t = 0,
      b = 0
    )
  )

funestus_prediction_map

# 16C. Plot An. hancocki model prediction

# Extract the predicted household detection probability
# for An. hancocki.

hancocki_detection_probability <-
  household_detection_probability_kc[["An. hancocki"]]


# Select surveyed sites where An. hancocki was observed.

hancocki_site_counts <- observed_site_counts |>
  filter(
    species == "An. hancocki",
    total_count > 0
  )


# Confirm that the observed total count is preserved.

sum(
  hancocki_site_counts$total_count
)

# Create the An. hancocki model-prediction map.

hancocki_prediction_map <- ggplot() +
  
  tidyterra::geom_spatraster(
    data = hancocki_detection_probability
  ) +
  
  scale_fill_gradientn(
    colours = probability_colours,
    limits = c(
      0,
      1
    ),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    labels = c(
      "0",
      "0.25",
      "0.50",
      "0.75",
      "1.00"
    ),
    name = "Household\nprobability\nof detection",
    na.value = "white",
    guide = guide_colourbar(
      barheight = grid::unit(
        4,
        "cm"
      ),
      barwidth = grid::unit(
        0.35,
        "cm"
      ),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  tidyterra::geom_spatvector(
    data = kasai_central_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.4
  ) +
  
  geom_point(
    mapping = aes(
      x = long_dd,
      y = lat_dd,
      size = point_size
    ),
    data = hancocki_site_counts,
    shape = 21,
    fill = "black",
    colour = "white",
    stroke = 0.4
  ) +
  
  scale_size_identity(
    guide = "none"
  ) +
  
  labs(
    title = expression(
      italic("An. hancocki")
    )
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 14
    ),
    
    legend.position = "right",
    
    legend.box.spacing = grid::unit(
      0.15,
      "cm"
    ),
    
    legend.margin = margin(
      l = 0,
      r = 0,
      t = 0,
      b = 0
    )
  )


hancocki_prediction_map

# 16D. Plot An. moucheti model prediction

# Extract the predicted household detection probability
# for An. moucheti.

moucheti_detection_probability <-
  household_detection_probability_kc[["An. moucheti"]]


# Select surveyed sites where An. moucheti was observed.

moucheti_site_counts <- observed_site_counts |>
  filter(
    species == "An. moucheti",
    total_count > 0
  )


# Confirm that the observed total count is preserved.

sum(
  moucheti_site_counts$total_count
)

# Create the An. moucheti model-prediction map.

moucheti_prediction_map <- ggplot() +
  
  tidyterra::geom_spatraster(
    data = moucheti_detection_probability
  ) +
  
  scale_fill_gradientn(
    colours = probability_colours,
    limits = c(
      0,
      1
    ),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    labels = c(
      "0",
      "0.25",
      "0.50",
      "0.75",
      "1.00"
    ),
    name = "Household\nprobability\nof detection",
    na.value = "white",
    guide = guide_colourbar(
      barheight = grid::unit(
        4,
        "cm"
      ),
      barwidth = grid::unit(
        0.35,
        "cm"
      ),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  tidyterra::geom_spatvector(
    data = kasai_central_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.4
  ) +
  
  geom_point(
    mapping = aes(
      x = long_dd,
      y = lat_dd,
      size = point_size
    ),
    data = moucheti_site_counts,
    shape = 21,
    fill = "black",
    colour = "white",
    stroke = 0.4
  ) +
  
  scale_size_identity(
    guide = "none"
  ) +
  
  labs(
    title = expression(
      italic("An. moucheti")
    )
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 14
    ),
    
    legend.position = "right",
    
    legend.box.spacing = grid::unit(
      0.15,
      "cm"
    ),
    
    legend.margin = margin(
      l = 0,
      r = 0,
      t = 0,
      b = 0
    )
  )


moucheti_prediction_map


# 16E. Plot An. paludis model prediction

# Extract the predicted household detection probability
# for An. paludis from the multi-layer prediction raster.

paludis_detection_probability <-
  household_detection_probability_kc[["An. paludis"]]


# Select surveyed sites where An. paludis was observed.

# Sites with zero observed mosquitoes are not displayed
# as proportional count circles.

# point_size uses the common count scale defined
# across all seven Anopheles taxa.

paludis_site_counts <- observed_site_counts |>
  filter(
    species == "An. paludis",
    total_count > 0
  )

# Confirm that the observed An. paludis total
# has been preserved after aggregation by surveyed site.

sum(
  paludis_site_counts$total_count
)

# Create the An. paludis model-prediction map.

# The raster represents the predicted household probability
# of detection from the hierarchical GAM.

# Black proportional circles represent the observed total
# An. paludis count at each surveyed site.

# Point sizes use the same common scale applied
# across all seven Anopheles taxa.

paludis_prediction_map <- ggplot() +
  
  tidyterra::geom_spatraster(
    data = paludis_detection_probability
  ) +
  
  scale_fill_gradientn(
    colours = probability_colours,
    limits = c(
      0,
      1
    ),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    labels = c(
      "0",
      "0.25",
      "0.50",
      "0.75",
      "1.00"
    ),
    name = "Household\nprobability\nof detection",
    na.value = "white",
    guide = guide_colourbar(
      barheight = grid::unit(
        4,
        "cm"
      ),
      barwidth = grid::unit(
        0.35,
        "cm"
      ),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  tidyterra::geom_spatvector(
    data = kasai_central_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.4
  ) +
  
  geom_point(
    mapping = aes(
      x = long_dd,
      y = lat_dd,
      size = point_size
    ),
    data = paludis_site_counts,
    shape = 21,
    fill = "black",
    colour = "white",
    stroke = 0.4
  ) +
  
  scale_size_identity(
    guide = "none"
  ) +
  
  labs(
    title = expression(
      italic("An. paludis")
    )
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 14
    ),
    
    legend.position = "right",
    
    legend.box.spacing = grid::unit(
      0.15,
      "cm"
    ),
    
    legend.margin = margin(
      l = 0,
      r = 0,
      t = 0,
      b = 0
    )
  )

paludis_prediction_map


# 16F. Plot An. sp. model prediction

# Extract the predicted household detection probability
# for unidentified Anopheles recorded as An. sp.

an_sp_detection_probability <-
  household_detection_probability_kc[["An. sp."]]


# Select surveyed sites where An. sp. was observed.

# Sites with zero observed mosquitoes are not displayed
# as proportional count circles.

an_sp_site_counts <- observed_site_counts |>
  filter(
    species == "An. sp.",
    total_count > 0
  )


# Confirm that the observed total count is preserved.

sum(
  an_sp_site_counts$total_count
)

# Create the An. sp. model-prediction map.

an_sp_prediction_map <- ggplot() +
  
  tidyterra::geom_spatraster(
    data = an_sp_detection_probability
  ) +
  
  scale_fill_gradientn(
    colours = probability_colours,
    limits = c(
      0,
      1
    ),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    labels = c(
      "0",
      "0.25",
      "0.50",
      "0.75",
      "1.00"
    ),
    name = "Household\nprobability\nof detection",
    na.value = "white",
    guide = guide_colourbar(
      barheight = grid::unit(
        4,
        "cm"
      ),
      barwidth = grid::unit(
        0.35,
        "cm"
      ),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  tidyterra::geom_spatvector(
    data = kasai_central_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.4
  ) +
  
  geom_point(
    mapping = aes(
      x = long_dd,
      y = lat_dd,
      size = point_size
    ),
    data = an_sp_site_counts,
    shape = 21,
    fill = "black",
    colour = "white",
    stroke = 0.4
  ) +
  
  scale_size_identity(
    guide = "none"
  ) +
  
  labs(
    title = expression(
      italic("An. sp.")
    )
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 14
    ),
    
    legend.position = "right",
    
    legend.box.spacing = grid::unit(
      0.15,
      "cm"
    ),
    
    legend.margin = margin(
      l = 0,
      r = 0,
      t = 0,
      b = 0
    )
  )


an_sp_prediction_map

# 16G. Plot An. ziemanni model prediction

# Extract the predicted household detection probability
# for An. ziemanni.

ziemanni_detection_probability <-
  household_detection_probability_kc[["An. ziemanni"]]


# Select surveyed sites where An. ziemanni was observed.

ziemanni_site_counts <- observed_site_counts |>
  filter(
    species == "An. ziemanni",
    total_count > 0
  )


# Confirm that the observed total count is preserved.

sum(
  ziemanni_site_counts$total_count
)

# Create the An. ziemanni model-prediction map.

# Because An. ziemanni was rarely observed,
# proportional circles are expected to remain small under
# the common count-size scale used across all taxa.

ziemanni_prediction_map <- ggplot() +
  
  tidyterra::geom_spatraster(
    data = ziemanni_detection_probability
  ) +
  
  scale_fill_gradientn(
    colours = probability_colours,
    limits = c(
      0,
      1
    ),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    labels = c(
      "0",
      "0.25",
      "0.50",
      "0.75",
      "1.00"
    ),
    name = "Household\nprobability\nof detection",
    na.value = "white",
    guide = guide_colourbar(
      barheight = grid::unit(
        4,
        "cm"
      ),
      barwidth = grid::unit(
        0.35,
        "cm"
      ),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  tidyterra::geom_spatvector(
    data = kasai_central_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.4
  ) +
  
  geom_point(
    mapping = aes(
      x = long_dd,
      y = lat_dd,
      size = point_size
    ),
    data = ziemanni_site_counts,
    shape = 21,
    fill = "black",
    colour = "white",
    stroke = 0.4
  ) +
  
  scale_size_identity(
    guide = "none"
  ) +
  
  labs(
    title = expression(
      italic("An. ziemanni")
    )
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 14
    ),
    
    legend.position = "right",
    
    legend.box.spacing = grid::unit(
      0.15,
      "cm"
    ),
    
    legend.margin = margin(
      l = 0,
      r = 0,
      t = 0,
      b = 0
    )
  )


ziemanni_prediction_map

# 16H. Combine all taxon-specific model-prediction maps

# Combine the seven separate taxon-specific prediction maps
# into a single multi-panel figure.

# Each panel retains:
#
#   - the predicted household probability of detection;
#
#   - the external Kasaï-Central boundary; and
#
#   - proportional black circles representing observed mosquito
#     counts at surveyed sites.

# Because all taxa use the same 0–1 probability scale,
# the probability legend is collected and displayed only once.
#
# The common observed-count point-size scale is also retained
# across all seven maps.

combined_prediction_maps <- patchwork::wrap_plots(
  list(
    gambiae_prediction_map,
    funestus_prediction_map,
    hancocki_prediction_map,
    moucheti_prediction_map,
    paludis_prediction_map,
    an_sp_prediction_map,
    ziemanni_prediction_map
  ),
  ncol = 4,
  guides = "collect"
) &
  theme(
    legend.position = "right"
  )

combined_prediction_maps

# 17. Restrict model predictions to environmentally similar areas

# Calculate the Multivariate Environmental Similarity Surface (MESS)
# using the same environmental covariates used to fit the
# hierarchical species-distribution model.

# MESS compares environmental conditions across Kasaï-Central
# with the environmental conditions represented by the
# 650 sampled households.
#
# Positive MESS values indicate environmental conditions
# represented within the sampled environmental range.
#
# Negative MESS values indicate environmental conditions
# outside the sampled range and therefore areas of
# environmental extrapolation.


# Convert the environmental covariate raster to a RasterBrick.

# The mess() function from the dismo package requires
# environmental raster predictors in Raster* format.

covraster <- brick(
  covs
)


# Select the same nine environmental PCA predictors
# used in the hierarchical GAM.

# Household identifiers are excluded because MESS should
# compare only environmental conditions.

sampled_environmental_covariates <- coord_covs |>
  select(
    starts_with("bioclim_pc"),
    starts_with("landcover_pc")
  ) |>
  as.data.frame()


# Calculate the Multivariate Environmental Similarity Surface
# using the environmental conditions observed at sampled households
# as the reference environmental space.

kc_mess <- dismo::mess(
  x = covraster,
  v = sampled_environmental_covariates
) |>
  rast()


# Restrict the MESS surface to the Kasaï-Central study boundary.

kc_mess <- terra::mask(
  kc_mess,
  kasai_central_boundary
)


# Inspect the distribution of MESS values.

# Negative values indicate environmental extrapolation.
# Values greater than or equal to zero indicate
# environmentally similar conditions.

terra::global(
  kc_mess,
  c(
    "min",
    "max",
    "mean"
  ),
  na.rm = TRUE
)


# Create the environmental-similarity mask.

# MESS >= 0:
# environmental conditions are similar to those represented
# by the sampled households and predictions are retained.

# MESS < 0:
# environmental conditions fall outside the sampled
# environmental range and predictions are excluded.

kc_mess_mask <- terra::ifel(
  kc_mess >= 0,
  1,
  NA
)


# 17A. Quantify the environmentally similar prediction area

# Identify raster cells available for prediction
# within the Kasaï-Central study boundary.

kc_prediction_area <- terra::mask(
  covs[[1]],
  kasai_central_boundary
)


# Count all raster cells available for prediction
# within Kasaï-Central.

total_kc_cells <- terra::global(
  terra::ifel(
    !is.na(kc_prediction_area),
    1,
    NA
  ),
  "sum",
  na.rm = TRUE
)[1, 1]


# Count raster cells classified as environmentally similar
# according to the MESS threshold.

environmentally_similar_cells <- terra::global(
  kc_mess_mask,
  "sum",
  na.rm = TRUE
)[1, 1]


# Calculate the percentage of the Kasaï-Central prediction area
# represented by environmental conditions observed
# at the sampled households.

percent_environmentally_similar <-
  100 *
  environmentally_similar_cells /
  total_kc_cells


percent_environmentally_similar


# 17B. Save the MESS surfaces

# Save the recalculated MESS surface.

terra::writeRaster(
  kc_mess,
  "outputs/spatial/kc_mess.tif",
  overwrite = TRUE
)


# Save the corresponding environmental-similarity mask.

terra::writeRaster(
  kc_mess_mask,
  "outputs/spatial/kc_mess_mask.tif",
  overwrite = TRUE
)


# 17C. Apply the MESS mask to model predictions

# Apply the same environmental-similarity mask to the
# predicted household probability of detection for all seven taxa.

# Predictions are retained where MESS >= 0.

# Predictions are excluded where MESS < 0 because these areas
# require environmental extrapolation beyond the sampled conditions.

household_detection_probability_mess <- terra::mask(
  household_detection_probability_kc,
  kc_mess_mask
)


household_detection_probability_mess

# 17D. Create a common plotting function for MESS-supported predictions

# Create a common plotting function so that all seven taxa
# use exactly the same map design.

# Light grey:
# areas inside Kasaï-Central where MESS < 0.

# Light-blue to dark-blue surface:
# predicted household probability of detection where MESS >= 0.

# Black proportional circles:
# observed mosquito counts at surveyed sites using the
# common count-size scale defined in Step 16.

create_mess_prediction_map <- function(
    prediction_raster,
    site_counts,
    taxon_title
) {
  
  ggplot() +
    
    # Fill the complete Kasaï-Central study area in light grey.
    #
    # This provides the background representing areas where
    # environmental conditions are outside the sampled range.
    tidyterra::geom_spatvector(
      data = kasai_central_boundary,
      fill = "grey90",
      colour = NA
    ) +
    
    # Overlay the model predictions retained where MESS >= 0.
    tidyterra::geom_spatraster(
      data = prediction_raster
    ) +
    
    # Apply the light-blue to dark-blue probability scale.
    #
    # Low probability = light blue.
    # High probability = dark blue.
    scale_fill_gradient(
      low = "lightblue",
      high = "darkblue",
      limits = c(
        0,
        1
      ),
      breaks = c(
        0,
        0.25,
        0.50,
        0.75,
        1
      ),
      labels = c(
        "0",
        "0.25",
        "0.50",
        "0.75",
        "1.00"
      ),
      name = "Household\nprobability\nof detection",
      
      # NA raster cells remain transparent so that the
      # light-grey Kasaï-Central background remains visible.
      na.value = "transparent",
      
      guide = guide_colourbar(
        barheight = grid::unit(
          4,
          "cm"
        ),
        barwidth = grid::unit(
          0.35,
          "cm"
        ),
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    
    # Add only the external Kasaï-Central boundary.
    tidyterra::geom_spatvector(
      data = kasai_central_boundary,
      fill = NA,
      colour = "black",
      linewidth = 0.4
    ) +
    
    # Overlay observed mosquito counts at surveyed sites.
    #
    # The same proportional-circle scale is used for all taxa.
    geom_point(
      mapping = aes(
        x = long_dd,
        y = lat_dd,
        size = point_size
      ),
      data = site_counts,
      shape = 21,
      fill = "black",
      colour = "white",
      stroke = 0.4
    ) +
    
    # point_size was calculated in Step 16 and therefore
    # does not need to be rescaled by ggplot.
    scale_size_identity(
      guide = "none"
    ) +
    
    # Add the taxon name.
    labs(
      title = taxon_title
    ) +
    
    # Remove axes and unnecessary map background elements.
    theme_void() +
    
    # Apply common formatting to all taxon-specific maps.
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 14
      ),
      
      legend.position = "right",
      
      legend.box.spacing = grid::unit(
        0.15,
        "cm"
      ),
      
      legend.margin = margin(
        l = 0,
        r = 0,
        t = 0,
        b = 0
      )
    )
}


# 17E. Plot MESS-supported An. gambiae s.l. prediction

# Extract the predicted household probability of detection
# for An. gambiae s.l. within environmentally similar areas.

gambiae_detection_probability_mess <-
  household_detection_probability_mess[["An. gambiae s.l."]]


# Create the MESS-supported prediction map.

gambiae_prediction_map_mess <- create_mess_prediction_map(
  prediction_raster = gambiae_detection_probability_mess,
  site_counts = gambiae_site_counts,
  taxon_title = expression(
    italic("An. gambiae s.l.")
  )
)


# Display the map.

gambiae_prediction_map_mess


# 17F. Plot MESS-supported An. funestus gp prediction

# Extract the predicted household probability of detection
# for An. funestus gp within environmentally similar areas.

funestus_detection_probability_mess <-
  household_detection_probability_mess[["An. funestus gp"]]


# Create the MESS-supported prediction map.

funestus_prediction_map_mess <- create_mess_prediction_map(
  prediction_raster = funestus_detection_probability_mess,
  site_counts = funestus_site_counts,
  taxon_title = expression(
    italic("An. funestus gp")
  )
)


# Display the map.

funestus_prediction_map_mess


# 17G. Plot MESS-supported An. hancocki prediction

# Extract the predicted household probability of detection
# for An. hancocki within environmentally similar areas.

hancocki_detection_probability_mess <-
  household_detection_probability_mess[["An. hancocki"]]


# Create the MESS-supported prediction map.

hancocki_prediction_map_mess <- create_mess_prediction_map(
  prediction_raster = hancocki_detection_probability_mess,
  site_counts = hancocki_site_counts,
  taxon_title = expression(
    italic("An. hancocki")
  )
)


# Display the map.

hancocki_prediction_map_mess

# 17H. Plot MESS-supported An. moucheti prediction

# Extract the predicted household probability of detection
# for An. moucheti within environmentally similar areas.

moucheti_detection_probability_mess <-
  household_detection_probability_mess[["An. moucheti"]]


# Create the MESS-supported prediction map.

moucheti_prediction_map_mess <- create_mess_prediction_map(
  prediction_raster = moucheti_detection_probability_mess,
  site_counts = moucheti_site_counts,
  taxon_title = expression(
    italic("An. moucheti")
  )
)


# Display the map.

moucheti_prediction_map_mess


# 17I. Plot MESS-supported An. paludis prediction

# Extract the predicted household probability of detection
# for An. paludis within environmentally similar areas.

paludis_detection_probability_mess <-
  household_detection_probability_mess[["An. paludis"]]


# Create the MESS-supported prediction map.

paludis_prediction_map_mess <- create_mess_prediction_map(
  prediction_raster = paludis_detection_probability_mess,
  site_counts = paludis_site_counts,
  taxon_title = expression(
    italic("An. paludis")
  )
)


# Display the map.

paludis_prediction_map_mess


# 17J. Plot MESS-supported An. sp. prediction

# Extract the predicted household probability of detection
# for unidentified Anopheles recorded as An. sp.

an_sp_detection_probability_mess <-
  household_detection_probability_mess[["An. sp."]]


# Create the MESS-supported prediction map.

an_sp_prediction_map_mess <- create_mess_prediction_map(
  prediction_raster = an_sp_detection_probability_mess,
  site_counts = an_sp_site_counts,
  taxon_title = expression(
    italic("An. sp.")
  )
)


# Display the map.

an_sp_prediction_map_mess


# 17K. Plot MESS-supported An. ziemanni prediction

# Extract the predicted household probability of detection
# for An. ziemanni within environmentally similar areas.

ziemanni_detection_probability_mess <-
  household_detection_probability_mess[["An. ziemanni"]]


# Create the MESS-supported prediction map.

ziemanni_prediction_map_mess <- create_mess_prediction_map(
  prediction_raster = ziemanni_detection_probability_mess,
  site_counts = ziemanni_site_counts,
  taxon_title = expression(
    italic("An. ziemanni")
  )
)


# Display the map.

ziemanni_prediction_map_mess


# 17L. Combine all MESS-supported taxon-specific prediction maps

# Combine the seven environmentally supported prediction maps
# into a single multi-panel figure.

# All taxa use:

#   - the same 0-1 household probability scale;

#   - the same environmental-similarity mask;

#   - the same proportional-circle scale for observed counts.

# The probability legend is collected and displayed only once.

combined_prediction_maps_mess <- patchwork::wrap_plots(
  list(
    gambiae_prediction_map_mess,
    funestus_prediction_map_mess,
    hancocki_prediction_map_mess,
    moucheti_prediction_map_mess,
    paludis_prediction_map_mess,
    an_sp_prediction_map_mess,
    ziemanni_prediction_map_mess
  ),
  ncol = 4,
  guides = "collect"
) +
  
  # Add the overall figure title.
  patchwork::plot_annotation(
    title = "Predicted household probability of detection within environmentally similar areas"
  ) &
  
  # Apply common formatting to the combined figure.
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 11
    ),
    
    legend.position = "right"
  )


# Display the combined MESS-supported prediction figure.

combined_prediction_maps_mess


# 18. Save the MESS-supported prediction figure for the poster

# Create a poster-ready version of the combined figure.

# The background outside the Kasaï-Central maps is transparent.

# Light-grey areas inside Kasaï-Central are preserved because
# they represent environmental conditions outside the sampled range.

combined_prediction_maps_mess_poster <-
  combined_prediction_maps_mess &
  
  theme(
    plot.background = element_rect(
      fill = "transparent",
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = "transparent",
      colour = NA
    ),
    
    legend.background = element_rect(
      fill = "transparent",
      colour = NA
    ),
    
    legend.key = element_rect(
      fill = "transparent",
      colour = NA
    )
  )


# Display the poster-ready figure.

combined_prediction_maps_mess_poster


# Save the combined MESS-supported prediction figure
# as a high-resolution transparent PNG for the poster.

# ggsave(
#   filename = "outputs/figures/kc_anopheles_mess_predictions_blue_poster.png",
#   plot = combined_prediction_maps_mess_poster,
#   width = 14,
#   height = 8,
#   units = "in",
#   dpi = 600,
#   bg = "transparent"
# )

# 19. Save final model and spatial prediction outputs

# Save the predicted household probability of detection
# for all seven Anopheles taxa across Kasaï-Central.

terra::writeRaster(
  household_detection_probability_kc,
  "outputs/spatial/kc_household_detection_probability.tif",
  overwrite = TRUE
)


# Save the predicted household probability of detection
# restricted to environmentally similar areas where MESS >= 0.

terra::writeRaster(
  household_detection_probability_mess,
  "outputs/spatial/kc_household_detection_probability_mess.tif",
  overwrite = TRUE
)


# Save the fitted hierarchical negative-binomial GAM.

saveRDS(
  anopheles_hierarchical_gam,
  "outputs/anopheles_hierarchical_gam.rds"
)




# CREATE AN EXTRA POSTER FIGURE WITH FIVE ANOPHELES TAXA -----------------------


# 1. Calculate the number of collection months
# in which each Anopheles taxon was detected at each site.

site_month_detection <- count_data |>
  filter(
    species_count > 0
  ) |>
  group_by(
    health_zone,
    health_area,
    village,
    identification_taxon
  ) |>
  summarise(
    months_detected = n_distinct(
      collection_month
    ),
    .groups = "drop"
  ) |>
  rename(
    species = identification_taxon
  ) |>
  mutate(
    proportion_months_detected =
      months_detected / 12
  )


# 2. Add monthly detection information
# to the observed site-count dataset.

# Remove previous monthly-detection columns if this code
# has already been run in the current R session.

observed_site_counts <- observed_site_counts |>
  select(
    -any_of(
      c(
        "months_detected",
        "proportion_months_detected"
      )
    )
  ) |>
  left_join(
    site_month_detection,
    by = c(
      "species",
      "health_zone",
      "health_area",
      "village"
    )
  )


# Check the monthly detection information.

observed_site_counts |>
  select(
    species,
    health_zone,
    total_count,
    months_detected,
    proportion_months_detected
  )


# 3. Recreate the site-count datasets for the five taxa
# after adding the monthly detection information.

gambiae_site_counts_poster <- observed_site_counts |>
  filter(
    species == "An. gambiae s.l.",
    total_count > 0
  )


funestus_site_counts_poster <- observed_site_counts |>
  filter(
    species == "An. funestus gp",
    total_count > 0
  )


hancocki_site_counts_poster <- observed_site_counts |>
  filter(
    species == "An. hancocki",
    total_count > 0
  )


moucheti_site_counts_poster <- observed_site_counts |>
  filter(
    species == "An. moucheti",
    total_count > 0
  )


paludis_site_counts_poster <- observed_site_counts |>
  filter(
    species == "An. paludis",
    total_count > 0
  )


# 4. Create a plotting function for the poster.

# Point size represents the total mosquito count.
# Point colour represents the proportion of the 12 months
# in which each taxon was detected.
#
# Faint pink indicates detection in only a few months.
# Bright pink indicates detection in most or all months.

create_mess_prediction_map_poster <- function(
    prediction_raster,
    site_counts,
    taxon_title
) {
  
  ggplot() +
    
    # Show environmentally unsupported areas in grey.
    tidyterra::geom_spatvector(
      data = kasai_central_boundary,
      fill = "grey90",
      colour = NA
    ) +
    
    # Add the predicted household probability surface.
    tidyterra::geom_spatraster(
      data = prediction_raster
    ) +
    
    # Use the common probability scale.
    scale_fill_gradient(
      low = "lightblue",
      high = "darkblue",
      limits = c(
        0,
        1
      ),
      breaks = c(
        0,
        0.25,
        0.50,
        0.75,
        1
      ),
      labels = c(
        "0",
        "0.25",
        "0.50",
        "0.75",
        "1.00"
      ),
      name = "Household\nprobability\nof detection",
      na.value = "transparent",
      guide = guide_colourbar(
        barheight = grid::unit(
          4,
          "cm"
        ),
        barwidth = grid::unit(
          0.35,
          "cm"
        ),
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    
    # Add the Kasaï-Central boundary.
    tidyterra::geom_spatvector(
      data = kasai_central_boundary,
      fill = NA,
      colour = "black",
      linewidth = 0.4
    ) +
    
    # Overlay observed mosquito data.
    #
    # Point size = total mosquito count.
    # Point colour = proportion of months detected.
    geom_point(
      data = site_counts,
      mapping = aes(
        x = long_dd,
        y = lat_dd,
        size = point_size,
        colour = proportion_months_detected
      ),
      shape = 16
    ) +
    
    # Keep the proportional point sizes already calculated.
    scale_size_identity(
      guide = "none"
    ) +
    
    # Scale point colour according to temporal persistence.
    scale_colour_gradient(
      low = "mistyrose",
      high = "deeppink",
      limits = c(
        1 / 12,
        1
      ),
      breaks = c(
        1 / 12,
        3 / 12,
        6 / 12,
        9 / 12,
        12 / 12
      ),
      labels = c(
        "1/12",
        "3/12",
        "6/12",
        "9/12",
        "12/12"
      ),
      name = "Months\ndetected"
    ) +
    
    labs(
      title = taxon_title
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 14
      ),
      legend.position = "right"
    )
}


# 5. Create the five poster maps.


# An. gambiae s.l.

gambiae_prediction_map_mess_poster <- create_mess_prediction_map_poster(
  prediction_raster = gambiae_detection_probability_mess,
  site_counts = gambiae_site_counts_poster,
  taxon_title = expression(
    italic("An. gambiae s.l.")
  )
)


# An. funestus gp

funestus_prediction_map_mess_poster <- create_mess_prediction_map_poster(
  prediction_raster = funestus_detection_probability_mess,
  site_counts = funestus_site_counts_poster,
  taxon_title = expression(
    italic("An. funestus gp")
  )
)


# An. hancocki

hancocki_prediction_map_mess_poster <- create_mess_prediction_map_poster(
  prediction_raster = hancocki_detection_probability_mess,
  site_counts = hancocki_site_counts_poster,
  taxon_title = expression(
    italic("An. hancocki")
  )
)


# An. moucheti

moucheti_prediction_map_mess_poster <- create_mess_prediction_map_poster(
  prediction_raster = moucheti_detection_probability_mess,
  site_counts = moucheti_site_counts_poster,
  taxon_title = expression(
    italic("An. moucheti")
  )
)


# An. paludis

paludis_prediction_map_mess_poster <- create_mess_prediction_map_poster(
  prediction_raster = paludis_detection_probability_mess,
  site_counts = paludis_site_counts_poster,
  taxon_title = expression(
    italic("An. paludis")
  )
)


# 6. Combine the five taxon-specific maps.

# An. sp. and An. ziemanni are excluded.

combined_prediction_maps_mess_5_taxa <- patchwork::wrap_plots(
  list(
    gambiae_prediction_map_mess_poster,
    funestus_prediction_map_mess_poster,
    hancocki_prediction_map_mess_poster,
    moucheti_prediction_map_mess_poster,
    paludis_prediction_map_mess_poster
  ),
  ncol = 3,
  guides = "collect"
) +
  patchwork::plot_annotation(
    title = "Predicted household probability of detection within environmentally similar areas"
  ) &
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 11
    ),
    legend.position = "right"
  )


# Display the five-taxa poster figure.

combined_prediction_maps_mess_5_taxa


# 7. Create a high-resolution transparent version for the poster

combined_prediction_maps_mess_5_taxa_poster <-
  combined_prediction_maps_mess_5_taxa &
  theme(
    plot.background = element_rect(
      fill = "transparent",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "transparent",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "transparent",
      colour = NA
    ),
    legend.box.background = element_rect(
      fill = "transparent",
      colour = NA
    ),
    legend.key = element_rect(
      fill = "transparent",
      colour = NA
    )
  )


# Display the transparent poster version.

combined_prediction_maps_mess_5_taxa_poster

# 8. Save the five-taxa figure as a high-resolution transparent PNG

ggsave(
  filename = "outputs/figures/kc_anopheles_5_taxa_detection_months_poster.png",
  plot = combined_prediction_maps_mess_5_taxa_poster,
  width = 18,
  height = 12,
  units = "in",
  dpi = 600,
  bg = "transparent"
)
