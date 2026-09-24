
# PREPARE DATA TO FIT THE MODEL 

# 1. Load packages

source(
  "R/packages.R"
)


# 2. Load Anopheles count data

# This dataset contains species-specific mosquito counts
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

# These environmental covariates were prepared previously
# in prep_covariates.R.

# The dataset contains household identifiers,
# coordinates and the environmental PCA predictors.

coords_covariates <- read_csv(
  "data/clean/coords_covariates.csv",
  show_col_types = FALSE
)


# Inspect the household covariate dataset.

coords_covariates |>
  glimpse()


dim(
  coords_covariates
)


# 5. Retain variables required for modelling and mapping

household_covariates <- coords_covariates |>
  select(
    health_zone,
    health_area,
    village,
    house_number,
    long_dd,
    lat_dd,
    starts_with("bioclim_pc"),
    starts_with("landcover_pc")
  )


# Inspect the retained variables.

household_covariates |>
  glimpse()


# 6. Combine household counts with environmental covariates

# Join the household-level mosquito counts with
# environmental predictors at the sampled households.

model_data <- counts |>
  left_join(
    household_covariates,
    by = c(
      "health_zone",
      "health_area",
      "village",
      "house_number"
    )
  )


# 7. Check the final modelling dataframe

# Each row should represent:
# one household × one Anopheles taxon.

model_data |>
  glimpse()


# Expected:
# 650 households × 7 taxa = 4,550 observations.

dim(
  model_data
)


# Confirm that all observations received values
# for the nine environmental PCA predictors.

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


# Confirm that joining the environmental covariates
# did not duplicate or remove mosquito counts.

sum(
  model_data$count
)


# 8. Save final modelling dataframe

# This dataframe will be loaded directly by fit_model.R.

saveRDS(
  model_data,
  "data/clean/kc_anopheles_model_data.rds"
)


# Confirm that the file was saved successfully.

file.exists(
  "data/clean/kc_anopheles_model_data.rds"
)
