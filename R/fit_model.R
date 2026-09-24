
# FIT HIERARCHICAL ANOPHELES MODEL 


# 1. Load packages

source(
  "R/packages.R"
)


# 2. Load prepared modelling dataframe

# The modelling dataframe was created previously
# in prepare_model_data.R.

# Each row represents one household × one Anopheles taxon
# and contains:
#
# - the pooled mosquito count across the study period;
# - household coordinates; and
# - environmental PCA predictors.

model_data <- readRDS(
  "data/clean/kc_anopheles_model_data.rds"
)


# Inspect the modelling dataframe.

model_data |>
  glimpse()


dim(
  model_data
)


# 3. Examine mosquito count distribution by taxon

# Summarise household-level mosquito counts
# separately for each Anopheles taxon.

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


# 4. Quantify modelling support and overdispersion by taxon

# Calculate the number of households with at least one mosquito
# and the variance-to-mean ratio for each Anopheles taxon.

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


# 5. Fit the hierarchical negative-binomial GAM

# The response variable is the total number of mosquitoes
# of a given Anopheles taxon collected at a household
# across the complete study period.

# Counts from the 12 collection rounds were combined
# during model-data preparation.

# The model uses a negative-binomial distribution
# to account for overdispersion in mosquito counts.

# The model contains:
#
# 1. separate baseline counts for the Anopheles taxa;
#
# 2. shared environmental responses across taxa; and
#
# 3. taxon-specific deviations from those shared responses.

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
  
  # Fit using Restricted Maximum Likelihood
  method = "REML",
  
  data = model_data
)


# 6. Inspect fitted model

summary(
  anopheles_hierarchical_gam
)

# 7. Save fitted model

saveRDS(
  anopheles_hierarchical_gam,
  "outputs/anopheles_hierarchical_gam.rds"
)