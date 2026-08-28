
# MULTIVARIATE ENVIRONMENTAL SIMILARITY SURFACE ANALYSIS ---------------------

# 1. Load packages

# Load the packages required for spatial analysis,
# environmental similarity assessment,
# and visualisation.

source(
  "R/packages.R"
)


# 2. Load Kasaï-Central environmental covariates

# Load the environmental PCA covariates prepared
# for Kasaï-Central.
#
# These raster layers summarise the main climatic and land-cover
# gradients across the province and will be used as environmental
# covariates in the species distribution models.

kc_covariates <- rast(
  "data/clean/covariates.tif"
)


# Confirm the number of environmental covariate layers.

nlyr(
  kc_covariates
)


# Check the names of the environmental covariates.

names(
  kc_covariates
)


# The raster stack contains nine environmental covariates:
#
# - four principal components summarising climatic variation
#   (bioclim_pc1 to bioclim_pc4); and
# - five principal components summarising land-cover variation
#   (landcover_pc1 to landcover_pc5).
#
# PCA was used to reduce collinearity and dimensionality among
# the original environmental variables while retaining at least
# 90% of the environmental variation represented at the
# sampled household locations.

# 3. Load DRC environmental covariates

# Load the environmental PCA covariates prepared
# for the whole Democratic Republic of the Congo.
#
# These raster layers contain the same environmental covariates
# prepared for Kasaï-Central but extend across the whole country.

drc_covariates <- rast(
  "data/clean/drc_covariates.tif"
)


# Confirm the number of environmental covariate layers.

nlyr(
  drc_covariates
)


# Check the names of the environmental covariates.

names(
  drc_covariates
)


# The DRC raster stack contains the same nine environmental covariates:
#
# - four principal components summarising climatic variation
#   (bioclim_pc1 to bioclim_pc4); and
# - five principal components summarising land-cover variation
#   (landcover_pc1 to landcover_pc5).


# 4. Confirm Kasaï-Central and DRC covariates match

# Confirm that the Kasaï-Central and DRC raster stacks
# contain exactly the same environmental covariates
# in the same order.
#
# This is required because the environmental similarity
# analysis must compare equivalent environmental variables.

identical(
  names(kc_covariates),
  names(drc_covariates)
)


# 5. Load household environmental covariates

# Load the environmental covariate values extracted
# at the 650 sampled household locations.
#
# These household-level environmental conditions will define
# the reference environmental space used in the MESS analysis.

household_covariates <- read_csv(
  "data/clean/coords_covariates.csv",
  show_col_types = FALSE
)


# Inspect the structure of the household covariate dataset.

glimpse(
  household_covariates
)



# 6. Confirm household and raster covariates match

# Retain only the environmental covariates extracted
# at the sampled household locations.

household_environment <- household_covariates |>
  select(
    starts_with("bioclim_pc"),
    starts_with("landcover_pc")
  )


# Confirm that the household environmental covariates
# have exactly the same names and order as the
# Kasaï-Central raster covariates.

identical(
  names(household_environment),
  names(kc_covariates)
)


# 7. Check for missing household environmental covariates

# Check for missing values in the environmental covariates
# extracted at the sampled household locations.
#
# Missing values could affect the MESS calculation and should
# therefore be identified before continuing.

missing_household_covariates <- colSums(
  is.na(
    household_environment
  )
)


# Display the number of missing values for each
# environmental covariate.

missing_household_covariates


# 8. Prepare Kasaï-Central covariates for MESS analysis

# Multivariate Environmental Similarity Surface (MESS)
#
# MESS evaluates whether environmental conditions across
# Kasaï-Central are similar to the environmental conditions
# represented at the 650 sampled household locations.
#
# kc_covariates represents environmental conditions across
# Kasaï-Central using the nine environmental PCA covariates.
#
# household_environment represents the environmental conditions
# observed at the sampled household locations.
#
# Positive MESS values indicate environmental conditions represented
# within the sampled environmental space.
#
# Negative MESS values indicate novel environmental conditions
# outside the sampled environmental space, where model predictions
# would require environmental extrapolation.
#
# Method: Elith et al. (2010)
# https://doi.org/10.1111/j.2041-210X.2010.00036.x


# Convert the Kasaï-Central environmental covariates
# from a terra SpatRaster to a RasterBrick.
#
# This conversion is required because dismo::mess()
# accepts Raster* objects from the raster package.

kc_covariates_raster <- raster::brick(
  kc_covariates
)


# Inspect the converted raster.

kc_covariates_raster


# 9. Calculate environmental similarity across Kasaï-Central

# Calculate the Multivariate Environmental Similarity Surface (MESS)
# across Kasaï-Central.
#
# x contains the nine environmental covariate layers
# across Kasaï-Central.
#
# v contains the environmental covariate values observed
# at the 650 sampled household locations.
#
# The resulting raster gives one MESS value for each raster cell.

kc_mess <- dismo::mess(
  x = kc_covariates_raster,
  v = as.data.frame(
    household_environment
  )
) |>
  rast()


# Inspect the resulting MESS raster.

kc_mess

# The MESS raster contains one layer describing environmental
# similarity across Kasaï-Central.

# Positive MESS values indicate environmental conditions that
# fall within the range represented by the sampled households.

# Negative MESS values indicate environmental conditions outside
# the range represented by the sampled households.

# These negative values correspond to environmental extrapolation,
# where model predictions are less strongly supported by the
# observed survey data.

# 10. Summarise the Kasaï-Central MESS surface

# Summarise the distribution of MESS values across Kasaï-Central.
#
# The minimum value identifies the strongest environmental
# dissimilarity relative to the sampled household environments.
#
# The maximum value identifies the highest environmental similarity.
#
# The mean provides an overall summary of environmental similarity
# across the province.

global(
  kc_mess,
  fun = c(
    "min",
    "max",
    "mean"
  ),
  na.rm = TRUE
)


# 11. Mask the MESS surface to the Kasaï-Central boundary

# Load the health-zone boundaries for Kasaï-Central.

kc_health_zones <- st_read(
  "data/clean/kc_health_zones.gpkg",
  quiet = TRUE
)


# Combine the 26 health-zone polygons into a single
# Kasaï-Central provincial boundary.

kc_boundary <- st_union(
  kc_health_zones
)


# Restrict the MESS surface to the Kasaï-Central boundary.
#
# MESS values inside the province remain unchanged,
# while raster cells outside the province are removed.

kc_mess <- mask(
  kc_mess,
  vect(kc_boundary)
)


# 12. Plot environmental similarity across Kasaï-Central

# Create a copy of the MESS raster for visualisation.

kc_mess_plot <- kc_mess


# Replace infinite MESS values with NA so that they
# do not interfere with the colour scale.

kc_mess_plot[
  is.infinite(
    values(kc_mess_plot)
  )
] <- NA


# Calculate a symmetrical colour-scale limit around zero.
#
# This ensures that negative and positive MESS values
# are displayed using the same absolute range.

mess_limit <- max(
  abs(
    values(kc_mess_plot)
  ),
  na.rm = TRUE
)


# Create a MESS map showing the 26 health-zone boundaries.
#
# Red indicates novel environmental conditions outside
# the sampled environmental space.
#
# White represents values around zero.
#
# Blue indicates environmental conditions represented
# within the sampled environmental space.

plot_kc_mess_hz <- ggplot() +
  geom_spatraster(
    data = kc_mess_plot
  ) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    direction = 1,
    limits = c(
      -mess_limit,
      mess_limit
    ),
    na.value = "transparent"
  ) +
  geom_sf(
    data = kc_health_zones,
    fill = NA,
    colour = "grey75",
    linewidth = 0.12
  ) +
  geom_sf(
    data = kc_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.3
  ) +
  theme_void() +
  labs(
    fill = "Multivariate\nEnvironmental\nSimilarity"
  )


plot_kc_mess_hz


# Create a second version showing only the
# Kasaï-Central provincial boundary.

plot_kc_mess_province <- ggplot() +
  geom_spatraster(
    data = kc_mess_plot
  ) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    direction = 1,
    limits = c(
      -mess_limit,
      mess_limit
    ),
    na.value = "transparent"
  ) +
  geom_sf(
    data = kc_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.3
  ) +
  theme_void() +
  labs(
    fill = "Multivariate\nEnvironmental\nSimilarity"
  )


plot_kc_mess_province

# The MESS analysis suggests that most sampled households captured a
# large proportion of the environmental conditions present across
# central and southern Kasaï-Central.

# However, several areas in the northern part of the province exhibit
# strongly negative MESS values.

# These locations contain environmental conditions that were not
# represented among the 650 sampled households.

# Predictions produced in these areas therefore require environmental
# extrapolation and should be interpreted with greater caution.

# In contrast, areas with positive MESS values fall within the
# environmental space represented by the survey data and provide
# greater confidence for model-based prediction.

# The spatial pattern suggests that environmental coverage was not
# uniform across Kasaï-Central, with northern health zones exhibiting
# the greatest environmental dissimilarity relative to sampled
# household locations.


# 13. Add sampled household locations to the MESS maps

# Add the 650 sampled household locations to the MESS maps.
#
# These points show the locations that define the
# reference environmental space used in the MESS analysis.


# Add household locations to the map showing
# health-zone boundaries.

plot_kc_mess_hz_households <- plot_kc_mess_hz +
  geom_point(
    data = household_covariates,
    aes(
      x = long_dd,
      y = lat_dd
    ),
    colour = "black",
    size = 1.2
  )


plot_kc_mess_hz_households


# Add household locations to the map showing
# only the Kasaï-Central provincial boundary.

plot_kc_mess_province_households <- plot_kc_mess_province +
  geom_point(
    data = household_covariates,
    aes(
      x = long_dd,
      y = lat_dd
    ),
    colour = "black",
    size = 1.2
  )


plot_kc_mess_province_households


# 14. Save the Kasaï-Central MESS maps


# Save the MESS map showing health-zone boundaries and
# sampled household locations as a high-resolution
# PNG with a transparent background.

ggsave(
  filename = "outputs/figures/kc_mess_health_zones_households.png",
  plot = plot_kc_mess_hz_households,
  width = 8,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "transparent"
)


# Save the MESS map showing the provincial boundary and
# sampled household locations as a high-resolution
# PNG with a transparent background.

ggsave(
  filename = "outputs/figures/kc_mess_province_households.png",
  plot = plot_kc_mess_province_households,
  width = 8,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "transparent"
)


# Confirm that both figures were saved successfully.

file.exists(
  "outputs/figures/kc_mess_health_zones_households.png"
)

file.exists(
  "outputs/figures/kc_mess_province_households.png"
)

# 15. Create the Kasaï-Central environmental similarity mask

# Create a binary mask from the Kasaï-Central MESS surface.
#
# MESS >= 0 is assigned 1:
# environmental conditions are represented within the
# sampled environmental space.
#
# MESS < 0 is assigned NA:
# environmental conditions are novel relative to the
# sampled environmental space and would require extrapolation.

kc_mess_mask <- ifel(
  kc_mess >= 0,
  1,
  NA
)


# Check the values present in the mask.
#
# The expected values are:
# - 1 for environmentally represented areas; and
# - NA for novel environmental conditions.

unique(
  values(kc_mess_mask)
)


# 16. Save Kasaï-Central environmental similarity outputs

# Save the continuous Kasaï-Central MESS surface.
#
# This raster retains the full environmental similarity values
# and can be used for interpretation and visualisation.

writeRaster(
  kc_mess,
  "outputs/spatial/kc_mess.tif",
  overwrite = TRUE
)


# Save the binary environmental similarity mask.
#
# Cells with a value of 1 represent environmentally
# represented conditions.
#
# Cells with NA represent novel environmental conditions
# outside the sampled environmental space.

writeRaster(
  kc_mess_mask,
  "outputs/spatial/kc_mess_mask.tif",
  overwrite = TRUE
)


# 17. Prepare DRC covariates for MESS analysis

# Convert the DRC environmental covariates
# from a terra SpatRaster to a RasterBrick.
#
# This conversion is required because dismo::mess()
# accepts Raster* objects from the raster package.

drc_covariates_raster <- raster::brick(
  drc_covariates
)


# Inspect the converted raster.

drc_covariates_raster


# 18. Calculate environmental similarity across the DRC

# Calculate the Multivariate Environmental Similarity Surface (MESS)
# across the whole Democratic Republic of the Congo.
#
# x contains the nine environmental covariate layers
# across the DRC.
#
# v contains the environmental covariate values observed
# at the 650 sampled household locations in Kasaï-Central.
#
# The resulting raster gives one MESS value for each raster cell
# across the country relative to the sampled environmental space.

drc_mess <- dismo::mess(
  x = drc_covariates_raster,
  v = as.data.frame(
    household_environment
  )
) |>
  rast() |>
  mask(
    drc_covariates[[1]]
  )


# Inspect the resulting DRC MESS raster.

drc_mess


# 20. Prepare the DRC MESS surface for visualisation

drc_mess_plot <- drc_mess


# Replace infinite values with NA.

drc_mess_plot[
  is.infinite(
    values(drc_mess_plot)
  )
] <- NA


# 21. Plot environmental similarity across the DRC

drc_boundary <- geodata::gadm(
  country = "COD",
  level = 0,
  path = "data/downloads"
)


plot_drc_mess <- ggplot() +
  geom_spatraster(
    data = drc_mess_plot
  ) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    direction = 1,
    limits = c(
      -100,
      100
    ),
    breaks = c(
      -100,
      0,
      100
    ),
    oob = scales::squish,
    na.value = "transparent"
  ) +
  geom_spatvector(
    data = drc_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.3
  ) +
  theme_void() +
  labs(
    fill = "Multivariate\nEnvironmental\nSimilarity"
  )


plot_drc_mess


# 23. Load DRC provincial boundaries

drc_provinces <- geodata::gadm(
  country = "COD",
  level = 1,
  path = "data/downloads"
)


# 24. Add provincial boundaries and highlight Kasaï-Central

plot_drc_mess_provinces <- plot_drc_mess +
  geom_spatvector(
    data = drc_provinces,
    fill = NA,
    colour = "white",
    linewidth = 0.45
  ) +
  geom_spatvector(
    data = drc_provinces,
    fill = NA,
    colour = "grey35",
    linewidth = 0.18
  ) +
  geom_sf(
    data = kc_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.9
  ) +
  geom_spatvector(
    data = drc_boundary,
    fill = NA,
    colour = "black",
    linewidth = 0.45
  )


plot_drc_mess_provinces


# Add the sampled household locations to the final DRC MESS map.
#
# These points show where the environmental reference conditions
# used in the MESS analysis were observed.

plot_drc_mess_provinces_households <- plot_drc_mess_provinces +
  geom_point(
    data = household_covariates,
    aes(
      x = long_dd,
      y = lat_dd
    ),
    colour = "black",
    size = 1.2
  )


# Display the final DRC MESS map.

plot_drc_mess_provinces_households


# # 25. Prepare DRC provinces for environmental representation analysis
# 
# # Assign a unique numeric ID to each of the 26 DRC provinces.
# #
# # These IDs will allow each raster cell to be linked
# # to the province in which it occurs.
# 
# drc_provinces$province_id <- seq_len(
#   nrow(drc_provinces)
# )
# 
# 
# # Check the province names and their corresponding IDs.
# 
# drc_provinces[
#   ,
#   c(
#     "province_id",
#     "NAME_1"
#   )
# ]
# 
# 
# # Convert the provincial boundaries to a raster using
# # the DRC MESS raster as the spatial template.
# #
# # Each raster cell receives the numeric ID of the
# # province in which it is located.
# 
# drc_province_raster <- rasterize(
#   drc_provinces,
#   drc_mess,
#   field = "province_id"
# )
# 
# 
# # Inspect the resulting province raster.
# 
# drc_province_raster


# # 26. Quantify environmental representation by DRC province
# 
# # Calculate the area of each DRC MESS raster cell
# # in square kilometres.
# #
# # Cells without MESS information remain NA.
# 
# drc_cell_area <- cellSize(
#   drc_mess,
#   unit = "km",
#   mask = TRUE
# )
# 
# names(
#   drc_cell_area
# ) <- "area_km2"
# 
# 
# # Classify each raster cell according to its
# # environmental representation.
# #
# # 1 = MESS >= 0:
# #     environmentally represented.
# #
# # 0 = MESS < 0:
# #     environmentally dissimilar.
# 
# drc_represented <- ifel(
#   drc_mess >= 0,
#   1,
#   0
# )
# 
# names(
#   drc_represented
# ) <- "represented"
# 
# 
# # Combine the province ID and environmental representation
# # into a single zone code.
# #
# # For example:
# #
# # province 8 + dissimilar area -> 80
# # province 8 + represented area -> 81
# #
# # This allows represented and dissimilar areas for all
# # provinces to be summarised in one operation.
# 
# zone_code <- (
#   drc_province_raster * 10
# ) + drc_represented
# 
# names(
#   zone_code
# ) <- "zone_code"
# 
# 
# # Sum the raster-cell area for each combination
# # of province and environmental representation.
# 
# zonal_result <- zonal(
#   drc_cell_area,
#   zone_code,
#   fun = "sum",
#   na.rm = TRUE
# ) |>
#   as_tibble()
# 
# 
# # Inspect the resulting area summary.
# 
# zonal_result
# 
# 
# # 27. Create the province-level environmental representation summary
# 
# # Decode the zone code into:
# #
# # - province_id: identifies the DRC province; and
# # - represented: identifies whether the environmental
# #   conditions are represented or dissimilar.
# #
# # For example:
# #
# # 81 -> province 8, represented
# # 80 -> province 8, dissimilar
# 
# province_mess_long <- zonal_result |>
#   mutate(
#     province_id = zone_code %/% 10,
#     represented = zone_code %% 10
#   ) |>
#   select(
#     province_id,
#     represented,
#     area_km2
#   )
# 
# 
# # Add any missing province × representation combinations.
# #
# # If a province has no raster cells in one category,
# # its area for that category is assigned 0.
# 
# province_mess_long <- province_mess_long |>
#   tidyr::complete(
#     province_id = seq_len(
#       nrow(drc_provinces)
#     ),
#     represented = c(
#       0,
#       1
#     ),
#     fill = list(
#       area_km2 = 0
#     )
#   )
# 
# 
# # Create a lookup table linking province IDs
# # to the names of the 26 DRC provinces.
# 
# province_lookup <- as.data.frame(
#   drc_provinces
# ) |>
#   select(
#     province_id,
#     province = NAME_1
#   )
# 
# 
# # Create one row per province and calculate
# # the percentage of environmentally represented
# # and environmentally dissimilar area.
# 
# province_mess_summary <- province_mess_long |>
#   left_join(
#     province_lookup,
#     by = "province_id"
#   ) |>
#   select(
#     province,
#     represented,
#     area_km2
#   ) |>
#   pivot_wider(
#     names_from = represented,
#     values_from = area_km2,
#     names_prefix = "area_"
#   ) |>
#   rename(
#     dissimilar_km2 = area_0,
#     represented_km2 = area_1
#   ) |>
#   mutate(
#     total_area_km2 =
#       represented_km2 + dissimilar_km2,
#     
#     percent_represented =
#       100 * represented_km2 / total_area_km2,
#     
#     percent_dissimilar =
#       100 * dissimilar_km2 / total_area_km2
#   ) |>
#   arrange(
#     desc(percent_represented)
#   )
# 
# 
# # Display the results for all 26 provinces.
# 
# print(
#   province_mess_summary,
#   n = 26
# )
# 
# 
# # 28. Summarise environmental representation across DRC provinces
# 
# # Count the number of provinces containing at least some
# # environmentally represented area and the number with none.
# #
# # A province with percent_represented > 0 contains at least
# # some environmental conditions represented by the
# # Kasaï-Central sampled household environments.
# #
# # A province with percent_represented == 0 is entirely outside
# # the sampled environmental space.
# 
# province_representation_count <- province_mess_summary |>
#   summarise(
#     total_provinces = n(),
#     
#     provinces_with_representation =
#       sum(percent_represented > 0),
#     
#     provinces_without_representation =
#       sum(percent_represented == 0)
#   )
# 
# 
# # Display the summary.
# 
# province_representation_count
# 
# 
# # 29. Plot environmental representation by DRC province
# 
# # Create a horizontal bar plot showing the percentage
# # of environmentally represented area in each DRC province.
# #
# # Provinces are ordered from the lowest to the highest
# # percentage of environmentally represented area.
# #
# # Kasaï-Central is highlighted because the sampled households
# # defining the reference environmental space are located there.
# 
# plot_province_mess <- province_mess_summary |>
#   mutate(
#     province = forcats::fct_reorder(
#       province,
#       percent_represented
#     ),
#     study_province = if_else(
#       province == "Kasaï-Central",
#       "Kasaï-Central",
#       "Other provinces"
#     )
#   ) |>
#   ggplot(
#     aes(
#       x = percent_represented,
#       y = province,
#       fill = study_province
#     )
#   ) +
#   geom_col(
#     width = 0.75
#   ) +
#   scale_fill_manual(
#     values = c(
#       "Kasaï-Central" = "black",
#       "Other provinces" = "grey70"
#     )
#   ) +
#   labs(
#     x = "Environmentally represented area (%)",
#     y = NULL,
#     fill = NULL
#   ) +
#   theme_classic() +
#   theme(
#     legend.position = "top"
#   )
# 
# 
# # Display the plot.
# 
# plot_province_mess
# 
# 
# # 30. Create the DRC environmental similarity mask
# 
# # Create a binary environmental similarity mask for the DRC.
# #
# # MESS >= 0 is assigned 1:
# # environmental conditions are represented within the
# # Kasaï-Central sampled environmental space.
# #
# # MESS < 0 is assigned NA:
# # environmental conditions are novel relative to the
# # sampled environmental space and would require extrapolation.
# 
# drc_mess_mask <- ifel(
#   drc_mess >= 0,
#   1,
#   NA
# )
# 
# 
# # Check the values present in the mask.
# #
# # Expected values:
# # - 1 for environmentally represented areas; and
# # - NA for novel environmental conditions.
# 
# unique(
#   values(drc_mess_mask)
# )
# 
# 
# # 31. Save DRC environmental similarity outputs
# 
# # Save the continuous DRC MESS surface.
# #
# # This raster retains the full environmental similarity values
# # across the country for interpretation and visualisation.
# 
# writeRaster(
#   drc_mess,
#   "outputs/spatial/drc_mess.tif",
#   overwrite = TRUE
# )
# 
# 
# # Save the DRC environmental similarity mask.
# #
# # Cells with a value of 1 represent environmentally
# # represented conditions.
# #
# # Cells with NA represent novel environmental conditions
# # outside the sampled environmental space.
# 
# writeRaster(
#   drc_mess_mask,
#   "outputs/spatial/drc_mess_mask.tif",
#   overwrite = TRUE
# )
# 
# 
# # Confirm that both raster outputs were saved successfully.
# 
# file.exists(
#   "outputs/spatial/drc_mess.tif"
# )
# 
# file.exists(
#   "outputs/spatial/drc_mess_mask.tif"
# )
# 
# 
# # 32. Save DRC province-level results and figures
# 
# # Save the province-level environmental representation summary.
# #
# # For each of the 26 DRC provinces, the table contains:
# #
# # - environmentally represented area in km2;
# # - environmentally dissimilar area in km2;
# # - total evaluated area in km2;
# # - percentage represented; and
# # - percentage dissimilar.
# 
# write_csv(
#   province_mess_summary,
#   "outputs/drc_province_mess_summary.csv"
# )
# 
# 
# # Confirm that the table was saved successfully.
# 
# file.exists(
#   "outputs/drc_province_mess_summary.csv"
# )
# 
# 
# # Save the DRC MESS map showing:
# #
# # - environmental similarity across the country;
# # - provincial boundaries;
# # - Kasaï-Central highlighted; and
# # - the sampled household locations.
# 
# ggsave(
#   filename = "outputs/figures/drc_mess_provinces_households.png",
#   plot = plot_drc_mess_provinces_households,
#   width = 10,
#   height = 8,
#   units = "in",
#   dpi = 600,
#   bg = "transparent"
# )
# 
# 
# # Save the province-level environmental representation plot.
# 
# ggsave(
#   filename = "outputs/figures/drc_province_mess_representation.png",
#   plot = plot_province_mess,
#   width = 9,
#   height = 7,
#   units = "in",
#   dpi = 600,
#   bg = "transparent"
# )
# 

# 32. Environmental space for high-impact covariates ---------------------------


# Load the original environmental covariates for Kasaï-Central.

bioclim_crop <- terra::rast(
  "data/clean/bioclim_crop.tif"
)

landcover_crop <- terra::rast(
  "data/clean/landcover_crop.tif"
)


# Check layer names.

names(
  bioclim_crop
)

names(
  landcover_crop
)


# Select annual precipitation (BIO12).

annual_precipitation <- bioclim_crop[["bio_12"]]


# Identify the tree-cover layer.

tree_layer_name <- grep(
  "trees",
  names(landcover_crop),
  ignore.case = TRUE,
  value = TRUE
)

tree_layer_name


# Select tree cover.

tree_cover <- landcover_crop[[tree_layer_name[1]]]


# Combine annual precipitation and tree cover.

kc_env <- c(
  annual_precipitation,
  tree_cover
)

names(kc_env) <- c(
  "annual_precipitation",
  "tree_cover"
)


# Create a raster mask representing environmental conditions
# across Kasaï-Central.

kc_mask_raster <- terra::ifel(
  !is.na(kc_env[[1]]) &
    !is.na(kc_env[[2]]),
  1,
  NA
)


# Check the Kasaï-Central mask.

kc_mask_raster

plot(
  kc_mask_raster
)


# Generate 5,000 random background locations across Kasaï-Central.

set.seed(123)

kc_random <- terra::spatSample(
  kc_mask_raster,
  size = 5000,
  method = "random",
  na.rm = TRUE,
  as.points = TRUE
)


# Check the random locations.

kc_random

nrow(
  kc_random
)


# Extract environmental values at random background locations.

kc_random_env <- terra::extract(
  kc_env,
  kc_random,
  ID = FALSE
)


# Check the extracted environmental values.

head(
  kc_random_env
)

dim(
  kc_random_env
)


# Prepare surveyed household coordinates as a numeric matrix.

household_xy <- household_covariates |>
  dplyr::select(
    long_dd,
    lat_dd
  ) |>
  dplyr::filter(
    !is.na(long_dd),
    !is.na(lat_dd)
  ) |>
  as.matrix()


# Check the coordinate matrix.

head(
  household_xy
)

dim(
  household_xy
)


# Identify raster cells containing surveyed households.

survey_cells <- terra::cellFromXY(
  kc_env[[1]],
  household_xy
)


# Retain unique valid raster cells.

survey_cells <- unique(
  survey_cells[
    !is.na(survey_cells)
  ]
)


# Check the number of unique environmental cells represented
# by the household survey.

length(
  survey_cells
)


# Extract environmental values from surveyed raster cells.

kc_sample_cell <- as.data.frame(
  kc_env,
  cells = TRUE,
  na.rm = TRUE
) |>
  dplyr::filter(
    cell %in% survey_cells
  ) |>
  dplyr::select(
    annual_precipitation,
    tree_cover
  )


# Check surveyed environmental conditions.

head(
  kc_sample_cell
)

nrow(
  kc_sample_cell
)


# Create the environmental-space figure.

plot_environmental_space <- ggplot() +
  
  # Environmental background across Kasaï-Central.
  geom_point(
    data = kc_random_env,
    aes(
      x = annual_precipitation,
      y = tree_cover,
      colour = "Environmental conditions"
    ),
    size = 0.5,
    alpha = 0.25
  ) +
  
  # Environmental conditions at surveyed households.
  geom_point(
    data = kc_sample_cell,
    aes(
      x = annual_precipitation,
      y = tree_cover,
      colour = "Surveyed households"
    ),
    size = 2
  ) +
  
  # Define legend colours.
  scale_colour_manual(
    values = c(
      "Environmental conditions" = "firebrick",
      "Surveyed households" = "black"
    )
  ) +
  
  # Improve legend appearance.
  guides(
    colour = guide_legend(
      override.aes = list(
        size = c(4, 4),
        alpha = c(1, 1)
      )
    )
  ) +
  
  labs(
    x = "Annual precipitation (mm)",
    y = "Tree cover proportion",
    colour = NULL
  ) +
  
  theme_classic() +
  
  theme(
    legend.position = "top",
    legend.text = element_text(
      size = 10
    )
  )

plot_environmental_space


# Save Figure 1: MESS map.

ggsave(
  filename = "outputs/figures/kc_mess_map.png",
  plot = plot_kc_mess_province_households,
  width = 8,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)


# Save Figure 2: environmental-space plot.

ggsave(
  filename = "outputs/figures/kc_environmental_space.png",
  plot = plot_environmental_space,
  width = 8,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)
