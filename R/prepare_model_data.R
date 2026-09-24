
# PREPARE DATA TO FIT THE MODEL 

# 1. Load packages

source(
  "R/packages.R"
)


# 2. Load Anopheles count data

# This dataset contains species mosquito counts
# recorded for each household sampling event across
# the 12 collection rounds.

count_data <- read_csv(
  "data/clean/kc_anopheles_count_data.csv",
  show_col_types = FALSE
)


# Inspect the count dataset.

count_data |>
  glimpse()


# 3. Create household-level Anopheles counts for the full study period

# Combine mosquito counts from all collection rounds
# for each household and Anopheles taxon.

# After this step, each row represents:
# one household × one Anopheles taxon.

counts <- count_data |>
  group_by(
    health_zone,
    health_area,
    village,
    house_number,
    collection_month,
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


# Inspect the household × taxon count dataset.

counts |>
  glimpse()


# Check the number of rows and columns.

dim(
  counts
)


# Confirm that pooling the collection rounds
# preserved the total number of mosquitoes.

sum(
  counts$count
)


# 4. Load household environmental covariates

# Coordinates are joined to mosquito counts prior to extracting covariates.

coords <- read_csv(
  "data/clean/kc_household_coords.csv",
  show_col_types = FALSE
)


# Keep household identifiers and coordinates.

coords <- coords |>
  select(
    health_zone,
    health_area,
    village,
    house_number,
    long_dd,
    lat_dd
  )

coords |>
  glimpse()


# 5. Join household coordinates to mosquito counts

counts_coords <- counts |>
  left_join(
    coords,
    by = c(
      "health_zone",
      "health_area",
      "village",
      "house_number"
    )
  )


counts_coords |>
  glimpse()


# Check that all records received coordinates.

counts_coords |>
  summarise(
    missing_longitude = sum(
      is.na(long_dd)
    ),
    missing_latitude = sum(
      is.na(lat_dd)
    )
  )


# 6. Load environmental covariate raster

# The raster contains the environmental PCA covariates
# prepared previously in prep_covariates.R.

covs <- rast(
  "data/clean/covariates.tif"
)


covs


names(
  covs
)


# 7. Attach raster cell ID to each household observation

# cellFromXY() identifies the raster cell containing
# each sampled household.

counts_coords$cell_id <- terra::cellFromXY(
  covs,
  counts_coords |>
    select(
      long_dd,
      lat_dd
    ) |>
    as.matrix()
)


# Inspect raster-cell assignment.

counts_coords |>
  select(
    health_zone,
    health_area,
    village,
    house_number,
    collection_month,
    species,
    count,
    cell_id
  ) |>
  glimpse()


# Check for observations that were not assigned
# to a raster cell.

sum(
  is.na(
    counts_coords$cell_id
  )
)


# 8. Aggregate mosquito counts by raster cell, month and taxon

# Each row now represents:
#
# one raster cell × one month × one Anopheles taxon.
#
# n_households records the number of sampled households
# contributing to the mosquito count in that raster cell.

cell_month_counts <- counts_coords |>
  group_by(
    cell_id,
    collection_month,
    species
  ) |>
  summarise(
    n_households = n(),
    count = sum(
      count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


cell_month_counts |>
  glimpse()


# Check that total mosquito counts are still preserved.

sum(
  cell_month_counts$count
)

# Check the sampling effort per cell.

cell_month_counts |>
  summarise(
    minimum_households = min(
      n_households
    ),
    maximum_households = max(
      n_households
    ),
    median_households = median(
      n_households
    )
  )

names(counts)

# 9. Extract environmental covariates for sampled raster cells

# Identify the unique raster cells represented
# in the mosquito observations.

sampled_cells <- sort(
  unique(
    cell_month_counts$cell_id
  )
)


# Extract environmental predictor values
# for each sampled raster cell.

cell_covariates <- terra::extract(
  covs,
  sampled_cells
) |>
  as_tibble() |>
  mutate(
    cell_id = sampled_cells,
    .before = 1
  )


# Inspect extracted covariates.

cell_covariates |>
  glimpse()


nrow(
  cell_covariates
)

length(
  sampled_cells
)

names(
  cell_covariates
)

# 10. Join environmental covariates to aggregated mosquito counts

model_data <- cell_month_counts |>
  left_join(
    cell_covariates,
    by = "cell_id"
  )


# 11. Check final modelling dataframe

model_data |>
  glimpse()


dim(
  model_data
)


# Check for missing environmental covariates.

model_data |>
  summarise(
    across(
      c(
        starts_with("bioclim_pc"),
        starts_with("landcover_pc")
      ),
      ~ sum(
        is.na(.x)
      )
    )
  )


# Confirm total mosquito counts are still preserved.

sum(
  model_data$count
)


# Check number of raster cells represented.

n_distinct(
  model_data$cell_id
)


# Check months represented.

sort(
  unique(
    model_data$collection_month
  )
)


# Check taxon levels.

levels(
  model_data$species
)


# 12. Save final modelling dataframe

saveRDS(
  model_data,
  "data/clean/kc_anopheles_model_data.rds"
)


# Confirm that the file was saved.

file.exists(
  "data/clean/kc_anopheles_model_data.rds"
)