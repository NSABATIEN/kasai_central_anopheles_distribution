
# PREPARE MONTHLY CLIMATE MODEL DATA


# 1. Load packages

source("R/packages.R")


# 2. Load prepared environmental covariates

monthly_tavg_kc <- rast(
  "data/clean/monthly_tavg_kc.tif"
)

monthly_prec_kc <- rast(
  "data/clean/monthly_prec_kc.tif"
)

monthly_vapr_kc <- rast(
  "data/clean/monthly_vapr_kc.tif"
)

landcover_pcs_wc <- rast(
  "data/clean/landcover_pcs_worldclim_grid.tif"
)


# Use WorldClim as the reference grid

climate_grid <- monthly_tavg_kc[[1]]


# 3. Load Anopheles count data

count_data <- read_csv(
  "data/clean/kc_anopheles_count_data.csv",
  show_col_types = FALSE
)


# 4. Create household counts by month and species

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


# 6. Join coordinates and assign WorldClim cells

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


counts_coords$climate_cell_id <- cellFromXY(
  climate_grid,
  counts_coords |>
    select(
      long_dd,
      lat_dd
    ) |>
    as.matrix()
)


# 7. Match calendar months to survey rounds

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


counts_coords <- counts_coords |>
  left_join(
    month_lookup,
    by = "collection_month"
  )


# 8. Aggregate mosquito counts by climate cell,
# month and species

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


# Count sampled households in each cell and month

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


# Add sampling effort

cell_month_counts <- cell_month_counts |>
  left_join(
    cell_month_effort,
    by = c(
      "climate_cell_id",
      "collection_month",
      "month"
    )
  )


# 9. Create unique climate cell × month combinations

cell_month <- cell_month_counts |>
  distinct(
    climate_cell_id,
    month
  )


# 10. Extract monthly climate values

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


# 11. Extract static land-cover PCs

landcover_values <- landcover_pcs_wc[
  cell_month$climate_cell_id
]


cell_month <- bind_cols(
  cell_month,
  landcover_values
)


# 12. Join environmental covariates
# to mosquito counts

monthly_model_data <- cell_month_counts |>
  left_join(
    cell_month,
    by = c(
      "climate_cell_id",
      "month"
    )
  )


# 13. Check final model data

View(
  monthly_model_data
)


# 14. Save monthly model data

saveRDS(
  monthly_model_data,
  "data/clean/kc_anopheles_monthly_climate_model_data.rds"
)
