
# FIT MODEL WITH MONTHLY CLIMATE AND LAND-COVER COVARIATES


# 1. Load packages

source("R/packages.R")


# 2. Load monthly model data

monthly_data <- readRDS(
  "data/clean/kc_anopheles_monthly_climate_model_data.rds"
)


# Keep variables needed for the model

monthly_data <- monthly_data |>
  select(
    climate_cell_id,
    collection_month,
    month,
    species,
    n_households,
    count,
    tavg,
    prec,
    vapr,
    landcover_wc_pc1,
    landcover_wc_pc2,
    landcover_wc_pc3,
    landcover_wc_pc4,
    landcover_wc_pc5
  )


# 3. Check mosquito counts by species

monthly_data |>
  group_by(
    species
  ) |>
  summarise(
    total_count = sum(count),
    mean_count = mean(count),
    zero_observations = sum(count == 0),
    .groups = "drop"
  )


# 4. Fit original negative-binomial GAM

monthly_climate_landcover_gam <- gam(
  count ~
    
    species +
    
    s(tavg) +
    s(prec) +
    s(vapr) +
    
    s(landcover_wc_pc1) +
    s(landcover_wc_pc2) +
    s(landcover_wc_pc3) +
    s(landcover_wc_pc4) +
    s(landcover_wc_pc5) +
    
    s(tavg, species, bs = "re") +
    s(prec, species, bs = "re") +
    s(vapr, species, bs = "re") +
    
    offset(
      log(n_households)
    ),
  
  family = nb(),
  method = "REML",
  data = monthly_data
)


# 5. Fit comparison model with shared climate effects only

monthly_climate_landcover_gam_shared <- gam(
  count ~
    
    species +
    
    s(tavg) +
    s(prec) +
    s(vapr) +
    
    s(landcover_wc_pc1) +
    s(landcover_wc_pc2) +
    s(landcover_wc_pc3) +
    s(landcover_wc_pc4) +
    s(landcover_wc_pc5) +
    
    offset(
      log(n_households)
    ),
  
  family = nb(),
  method = "REML",
  data = monthly_data
)



# 6. Save original model

saveRDS(
  monthly_climate_landcover_gam,
  "outputs/anopheles_monthly_climate_landcover_gam.rds"
)
