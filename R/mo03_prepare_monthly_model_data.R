
# PREPARE MONTHLY CLIMATE MODEL DATA


# 1. Load packages

source("R/packages.R")


# 2. Load monthly climate rasters

monthly_tavg_kc <- rast(
  "data/clean/monthly_tavg_kc.tif"
)

monthly_prec_kc <- rast(
  "data/clean/monthly_prec_kc.tif"
)

monthly_vapr_kc <- rast(
  "data/clean/monthly_vapr_kc.tif"
)


# Confirm that monthly climate rasters use the same grid

compareGeom(
  monthly_tavg_kc[[1]],
  monthly_prec_kc[[1]],
  stopOnError = FALSE
)

compareGeom(
  monthly_tavg_kc[[1]],
  monthly_vapr_kc[[1]],
  stopOnError = FALSE
)


# Use the common monthly climate grid as the spatial reference

climate_grid <- monthly_tavg_kc[[1]]


# 3. Load Anopheles count data

count_data <- read_csv(
  "data/clean/kc_anopheles_count_data.csv",
  show_col_types = FALSE
)


# 4. Create household-level counts by month and species

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


# 5. Load household coordinates

coords <- read_csv(
  "data/clean/kc_household_coords.csv",
  show_col_types = FALSE
) |>
  select(
    health_zone,
    health_area,
    village,
    house_number,
    long_dd,
    lat_dd
  )


# 6. Join household coordinates to mosquito counts

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


# 7. Assign households to the monthly climate grid

counts_coords$climate_cell_id <- cellFromXY(
  climate_grid,
  counts_coords |>
    select(
      long_dd,
      lat_dd
    ) |>
    as.matrix()
)


# Check that all observations received a climate cell

sum(
  is.na(
    counts_coords$climate_cell_id
  )
)


# 8. Match WorldClim calendar months to entomological survey rounds

month_lookup <- tibble(
  month = c(
    "01", "02", "03", "04", "05", "06",
    "07", "08", "09", "10", "11", "12"
  ),
  collection_month = c(
    "month_10", # January
    "month_11", # February
    "month_12", # March
    "month_1",  # April
    "month_2",  # May
    "month_3",  # June
    "month_4",  # July
    "month_5",  # August
    "month_6",  # September
    "month_7",  # October
    "month_8",  # November
    "month_9"   # December
  )
)


# Add calendar month to survey data

counts_coords <- counts_coords |>
  left_join(
    month_lookup,
    by = "collection_month"
  )


# Check the matching

counts_coords |>
  distinct(
    collection_month,
    month
  ) |>
  arrange(
    month
  )

# 9. Aggregate mosquito counts by climate cell, month and species

cell_month_counts <- counts_coords |>
  group_by(
    climate_cell_id,
    collection_month,
    month,
    species
  ) |>
  summarise(
    count = sum(
      count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Count sampled households in each climate cell and month

cell_month_effort <- counts_coords |>
  distinct(
    climate_cell_id,
    collection_month,
    month,
    health_zone,
    health_area,
    village,
    house_number
  ) |>
  count(
    climate_cell_id,
    collection_month,
    month,
    name = "n_households"
  )


# Add sampling effort to species counts

cell_month_counts <- cell_month_counts |>
  left_join(
    cell_month_effort,
    by = c(
      "climate_cell_id",
      "collection_month",
      "month"
    )
  )

# 10. Create unique climate cell × month combinations

cell_month <- cell_month_counts |>
  distinct(
    climate_cell_id,
    month
  )


# 11. Extract monthly climate values

extract_monthly_value <- function(
    data,
    climate_raster
) {
  
  values <- rep(
    NA_real_,
    nrow(data)
  )
  
  month_number <- as.integer(
    data$month
  )
  
  for (m in unique(month_number)) {
    
    rows <- which(
      month_number == m
    )
    
    values[rows] <- climate_raster[[m]][
      data$climate_cell_id[rows]
    ][[1]]
  }
  
  values
}


cell_month <- cell_month |>
  mutate(
    tavg = extract_monthly_value(
      cell_month,
      monthly_tavg_kc
    ),
    prec = extract_monthly_value(
      cell_month,
      monthly_prec_kc
    ),
    vapr = extract_monthly_value(
      cell_month,
      monthly_vapr_kc
    )
  )


# Check for missing climate values

cell_month |>
  summarise(
    missing_tavg = sum(is.na(tavg)),
    missing_prec = sum(is.na(prec)),
    missing_vapr = sum(is.na(vapr))
  )

# 12. Load static land-cover PCs

covariates <- rast(
  "data/clean/covariates.tif"
)

landcover <- covariates[[
  c(
    "landcover_pc1",
    "landcover_pc2",
    "landcover_pc3",
    "landcover_pc4",
    "landcover_pc5"
  )
]]


# 13. Align land-cover PCs to the monthly climate grid

landcover_climate_grid <- resample(
  landcover,
  climate_grid,
  method = "bilinear"
)


# Check that land cover now uses the same grid

compareGeom(
  climate_grid,
  landcover_climate_grid[[1]],
  stopOnError = FALSE
)


# 14. Extract static land-cover values

landcover_values <- landcover_climate_grid[
  cell_month$climate_cell_id
]


cell_month <- bind_cols(
  cell_month,
  landcover_values
)

# 15. Check environmental covariates

cell_month |>
  summarise(
    missing_tavg = sum(is.na(tavg)),
    missing_prec = sum(is.na(prec)),
    missing_vapr = sum(is.na(vapr)),
    missing_landcover_pc1 = sum(is.na(landcover_pc1)),
    missing_landcover_pc2 = sum(is.na(landcover_pc2)),
    missing_landcover_pc3 = sum(is.na(landcover_pc3)),
    missing_landcover_pc4 = sum(is.na(landcover_pc4)),
    missing_landcover_pc5 = sum(is.na(landcover_pc5))
  )

# 16. Join environmental covariates to mosquito counts

monthly_model_data <- cell_month_counts |>
  left_join(
    cell_month,
    by = c(
      "climate_cell_id",
      "month"
    )
  )


# 17. Check final modelling data

View(
  monthly_model_data
)


# 18. Save monthly climate model data

saveRDS(
  monthly_model_data,
  "data/clean/kc_anopheles_monthly_climate_model_data.rds"
)