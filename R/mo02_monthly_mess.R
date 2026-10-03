
# MONTHLY MULTIVARIATE ENVIRONMENTAL SIMILARITY SURFACE ANALYSIS


# 1. Load packages

source("R/packages.R")


# 2. Load monthly environmental covariates

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


# 3. Load sampled households and survey rounds

survey_households <- read_csv(
  "data/clean/kc_anopheles_count_data.csv",
  show_col_types = FALSE
) |>
  distinct(
    health_zone,
    health_area,
    village,
    house_number,
    collection_month
  )


# 4. Load household coordinates

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


survey_households <- survey_households |>
  left_join(
    coords,
    by = c(
      "health_zone",
      "health_area",
      "village",
      "house_number"
    )
  )


# 5. Assign sampled households to WorldClim cells

survey_households$climate_cell_id <- cellFromXY(
  climate_grid,
  survey_households |>
    select(
      long_dd,
      lat_dd
    ) |>
    as.matrix()
)


# 6. Match calendar months to survey rounds

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


survey_households <- survey_households |>
  left_join(
    month_lookup,
    by = "collection_month"
  )


# 7. Keep unique sampled WorldClim cell × month combinations

sampled_cells <- survey_households |>
  distinct(
    climate_cell_id,
    month
  )


# 8. Extract monthly climate at sampled cells

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


sampled_cells <- sampled_cells |>
  mutate(
    tavg = extract_monthly_value(
      sampled_cells,
      monthly_tavg_kc
    ),
    prec = extract_monthly_value(
      sampled_cells,
      monthly_prec_kc
    ),
    vapr = extract_monthly_value(
      sampled_cells,
      monthly_vapr_kc
    )
  )


# 9. Extract static land-cover PCs at sampled cells

sampled_cells <- bind_cols(
  sampled_cells,
  landcover_pcs_wc[
    sampled_cells$climate_cell_id
  ]
)


# 10. Define sampled environmental reference space

sampled_environment <- sampled_cells |>
  select(
    tavg,
    prec,
    vapr,
    landcover_wc_pc1,
    landcover_wc_pc2,
    landcover_wc_pc3,
    landcover_wc_pc4,
    landcover_wc_pc5
  ) |>
  as.data.frame()


# 11. Load Kasaï-Central boundary

kc_boundary <- vect(
  "data/clean/kc_health_zones.gpkg"
) |>
  aggregate()


# 12. Calculate MESS for one calendar month

calculate_monthly_mess <- function(
    month_index
) {
  
  
  # Monthly climate + static land cover
  
  monthly_environment <- c(
    monthly_tavg_kc[[month_index]],
    monthly_prec_kc[[month_index]],
    monthly_vapr_kc[[month_index]],
    landcover_pcs_wc
  )
  
  
  names(
    monthly_environment
  ) <- c(
    "tavg",
    "prec",
    "vapr",
    "landcover_wc_pc1",
    "landcover_wc_pc2",
    "landcover_wc_pc3",
    "landcover_wc_pc4",
    "landcover_wc_pc5"
  )
  
  
  # Convert layers for dismo::mess()
  
  monthly_environment_raster <- raster::stack(
    lapply(
      1:nlyr(monthly_environment),
      function(i) {
        
        raster::raster(
          monthly_environment[[i]]
        )
      }
    )
  )
  
  
  names(
    monthly_environment_raster
  ) <- names(
    monthly_environment
  )
  
  
  # Calculate MESS
  
  mess <- dismo::mess(
    x = monthly_environment_raster,
    v = sampled_environment
  ) |>
    rast()
  
  
  # Restrict to Kasaï-Central
  
  mess <- mask(
    mess,
    kc_boundary
  )
  
  
  # Remove infinite values
  
  mess[
    is.infinite(
      values(mess)
    )
  ] <- NA
  
  
  mess
}


# 13. Calculate MESS for all 12 months

monthly_mess <- rast(
  lapply(
    1:12,
    calculate_monthly_mess
  )
)

names(
  monthly_mess
) <- month.name


# 14. Check monthly MESS results

global(
  monthly_mess,
  fun = c(
    "min",
    "max",
    "mean"
  ),
  na.rm = TRUE
)


# 15. Use the same colour scale for all months

mess_limit <- max(
  abs(
    values(
      monthly_mess
    )
  ),
  na.rm = TRUE
)


# 16. Create monthly MESS maps

create_monthly_mess_map <- function(
    month_name
) {
  
  ggplot() +
    
    geom_spatraster(
      data = monthly_mess[[
        month_name
      ]]
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
    
    geom_spatvector(
      data = kc_boundary,
      fill = NA,
      colour = "black",
      linewidth = 0.3
    ) +
    
    geom_point(
      data = coords,
      aes(
        x = long_dd,
        y = lat_dd
      ),
      colour = "black",
      size = 1.2
    ) +
    
    theme_void() +
    
    labs(
      title = month_name,
      fill = "Multivariate\nEnvironmental\nSimilarity"
    )
}


# 17. Create MESS maps for all 12 months

monthly_mess_maps <- lapply(
  month.name,
  create_monthly_mess_map
)


# 18. Combine monthly MESS maps

monthly_mess_figure <- patchwork::wrap_plots(
  monthly_mess_maps,
  ncol = 4,
  guides = "collect"
)

monthly_mess_figure


# 19. Create monthly MESS masks

monthly_mess_mask <- ifel(
  monthly_mess >= 0,
  1,
  NA
)

names(
  monthly_mess_mask
) <- month.name


# 20. Save outputs

dir.create(
  "outputs/spatial/monthly_climate_landcover",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)


writeRaster(
  monthly_mess,
  "outputs/spatial/monthly_climate_landcover/kc_monthly_mess.tif",
  overwrite = TRUE
)


writeRaster(
  monthly_mess_mask,
  "outputs/spatial/monthly_climate_landcover/kc_monthly_mess_mask.tif",
  overwrite = TRUE
)


ggsave(
  "outputs/figures/kc_monthly_mess.png",
  monthly_mess_figure,
  width = 14,
  height = 11,
  units = "in",
  dpi = 600,
  bg = "white"
)