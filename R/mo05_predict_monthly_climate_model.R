
# SPATIAL PREDICTION WITH MONTHLY CLIMATE AND LAND-COVER COVARIATES

# Generates monthly spatial predictions from the hierarchical
# negative-binomial GAM fitted to raster-cell mosquito counts.
#
# Monthly covariates:
# - mean temperature;
# - precipitation; and
# - water vapour pressure.
#
# Static covariates:
# - five land-cover principal components.
#
# Predictions are standardised to one household and converted
# to the predicted probability of detecting at least one mosquito.
#
# Predictions are displayed only in environmental conditions
# represented by the model-fitting data using MESS.
#
# Observed mosquito abundance is summarised at raster-cell level as:
#
# observed mosquitoes per household sampling occasion =
# total mosquitoes of that taxon in the raster cell /
# number of sampled households contributing to that cell.


# 1. Load packages

source("R/packages.R")


# 2. Load fitted monthly climate and land-cover model

monthly_climate_landcover_gam <- readRDS(
  "outputs/anopheles_monthly_climate_landcover_gam.rds"
)


# 3. Load monthly climate rasters

monthly_tavg_kc <- rast(
  "data/clean/monthly_tavg_kc.tif"
)

monthly_prec_kc <- rast(
  "data/clean/monthly_prec_kc.tif"
)

monthly_vapr_kc <- rast(
  "data/clean/monthly_vapr_kc.tif"
)

climate_grid <- monthly_tavg_kc[[1]]


# 4. Load static land-cover principal components

landcover_pcs_wc <- rast(
  "data/clean/landcover_pcs_worldclim_grid.tif"
)


# 5. Load Kasaï-Central boundary

# Dissolve the health-zone polygons to create
# one Kasaï-Central provincial boundary.

kc_boundary <- vect(
  "data/clean/kc_health_zones.gpkg"
) |>
  aggregate() |>
  fillHoles()


# Project the boundary to the prediction-grid CRS.

kc_boundary <- project(
  kc_boundary,
  crs(climate_grid)
)


# 6. Define Anopheles taxa

# All taxa included in the fitted model.

taxa <- c(
  "An. gambiae s.l.",
  "An. funestus gp",
  "An. hancocki",
  "An. moucheti",
  "An. paludis",
  "An. sp.",
  "An. ziemanni"
)


# Identified taxa included in final maps.
# Unidentified An. sp. is excluded.

map_taxa <- c(
  "An. gambiae s.l.",
  "An. funestus gp",
  "An. paludis",
  "An. hancocki",
  "An. moucheti",
  "An. ziemanni"
)


# 7. Create monthly prediction function

predict_monthly_detection <- function(month_index) {
  
  # Combine climate conditions for the selected month
  # with the static land-cover principal components.
  
  monthly_covariates <- c(
    monthly_tavg_kc[[month_index]],
    monthly_prec_kc[[month_index]],
    monthly_vapr_kc[[month_index]],
    landcover_pcs_wc
  )
  
  
  # Covariate names must exactly match those
  # used when fitting the GAM.
  
  names(monthly_covariates) <- c(
    "tavg",
    "prec",
    "vapr",
    "landcover_wc_pc1",
    "landcover_wc_pc2",
    "landcover_wc_pc3",
    "landcover_wc_pc4",
    "landcover_wc_pc5"
  )
  
  
  # Retrieve species levels used in the fitted model.
  
  model_species_levels <- levels(
    monthly_climate_landcover_gam$model$species
  )
  
  
  # Predict expected mosquito counts for each taxon.
  #
  # Setting n_households = 1 standardises predictions
  # to one household sampling occasion.
  
  predicted_counts <- rast(
    lapply(
      taxa,
      function(taxon_name) {
        
        predict(
          monthly_covariates,
          monthly_climate_landcover_gam,
          const = data.frame(
            species = factor(
              taxon_name,
              levels = model_species_levels
            ),
            n_households = 1
          ),
          type = "response",
          na.rm = TRUE
        )
      }
    )
  )
  
  names(predicted_counts) <- taxa
  
  
  # Negative-binomial dispersion parameter.
  
  theta <- monthly_climate_landcover_gam$family$getTheta(
    TRUE
  )
  
  
  # Convert expected count per household to the
  # predicted probability of detecting at least
  # one mosquito.
  
  household_detection_probability <-
    1 - (
      theta /
        (
          theta +
            predicted_counts
        )
    )^theta
  
  names(household_detection_probability) <- taxa
  
  
  # Restrict predictions to Kasaï-Central.
  
  mask(
    household_detection_probability,
    kc_boundary
  )
}


# 8. Generate predictions for all 12 calendar months

monthly_detection_predictions <- lapply(
  1:12,
  predict_monthly_detection
)

names(monthly_detection_predictions) <- month.name


# 9. Load monthly MESS outputs

monthly_mess <- rast(
  "outputs/spatial/monthly_climate_landcover/kc_monthly_mess.tif"
)

monthly_mess_mask <- rast(
  "outputs/spatial/monthly_climate_landcover/kc_monthly_mess_mask.tif"
)

names(monthly_mess) <- month.name
names(monthly_mess_mask) <- month.name


# 10. Apply MESS masks to monthly predictions

# MESS >= 0 = environmentally represented.
# MESS < 0  = environmentally dissimilar.

monthly_detection_predictions_mess <- lapply(
  1:12,
  function(i) {
    
    mask(
      monthly_detection_predictions[[i]],
      monthly_mess_mask[[i]]
    )
  }
)

names(monthly_detection_predictions_mess) <- month.name


# Create polygons representing environmentally
# dissimilar areas.

mess_excluded_polygons <- lapply(
  month.name,
  function(month_name) {
    
    as.polygons(
      ifel(
        monthly_mess[[month_name]] < 0,
        1,
        NA
      ),
      dissolve = TRUE,
      na.rm = TRUE
    )
  }
)

names(mess_excluded_polygons) <- month.name


# 11. Prepare observed raster-cell collections

# Observed mosquito abundance is summarised for each
# raster cell × survey month × taxon as:
#
# Observed mosquitoes per household sampling occasion =
#
# total mosquitoes of that taxon in the raster cell
# ---------------------------------------------------
# number of sampled households contributing to that cell
#
# The cleaned count dataset already contains
# household latitude and longitude.

count_data <- read_csv(
  "data/clean/kc_anopheles_count_data.csv",
  show_col_types = FALSE
)


# Convert survey rounds to calendar months.
#
# month_1  = April
# month_2  = May
# month_3  = June
# month_4  = July
# month_5  = August
# month_6  = September
# month_7  = October
# month_8  = November
# month_9  = December
# month_10 = January
# month_11 = February
# month_12 = March.

round_lookup <- tibble(
  collection_month = paste0(
    "month_",
    1:12
  ),
  month_name = month.name[
    c(
      4:12,
      1:3
    )
  ]
)


# Use the household coordinates already present
# in the cleaned count dataset.

household_observations <-
  count_data


# Convert sampled households to spatial points.

household_points <- vect(
  household_observations,
  geom = c(
    "long_dd",
    "lat_dd"
  ),
  crs = "EPSG:4326"
)


# Project sampled households to the climate-grid CRS.

household_points <- project(
  household_points,
  crs(climate_grid)
)


# Assign each sampled household to the climate
# raster cell containing its coordinates.

household_observations$raster_cell_id <-
  cellFromXY(
    climate_grid,
    crds(
      household_points
    )
  )


# Summarise observations at:
#
# raster cell × survey month × taxon.

cell_stats <-
  household_observations |>
  filter(
    !is.na(
      raster_cell_id
    )
  ) |>
  group_by(
    raster_cell_id,
    collection_month,
    species = identification_taxon
  ) |>
  summarise(
    n_households = n(),
    
    total_count = sum(
      species_count,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) |>
  mutate(
    mosquitoes_per_household_sampling_occasion =
      total_count /
      n_households
  ) |>
  left_join(
    round_lookup,
    by = "collection_month"
  )


# Obtain the centre coordinates of each sampled
# raster cell for plotting.

sampled_cell_ids <- sort(
  unique(
    cell_stats$raster_cell_id
  )
)

cell_centres <- as_tibble(
  xyFromCell(
    climate_grid,
    sampled_cell_ids
  )
)

cell_centres$raster_cell_id <-
  sampled_cell_ids


# Add raster-cell centre coordinates.

cell_stats <-
  cell_stats |>
  left_join(
    cell_centres,
    by = "raster_cell_id"
  )


# Convert raster-cell observations to spatial points.

cell_points <- vect(
  cell_stats,
  geom = c(
    "x",
    "y"
  ),
  crs = crs(
    climate_grid
  )
)


# 12. Define observed abundance scale

# Use the same observed-abundance scale across all mapped taxa
# to allow direct comparison between species.
#
# Observed values at or above 10 mosquitoes per household
# sampling occasion are displayed at the upper end of the scale.
#
# The original observed values remain unchanged in cell_stats.

observed_cap <- 10


# Fixed legend breaks.

observed_breaks <- c(
  0,
  2,
  4,
  6,
  8,
  10
)


# Fixed legend labels.

observed_labels <- c(
  "0",
  "2",
  "4",
  "6",
  "8",
  "≥ 10"
)


# 13. Define taxon names for figure text

taxon_markdown <- function(taxon_name) {
  
  switch(
    taxon_name,
    
    "An. gambiae s.l." =
      "*An. gambiae* s.l.",
    
    "An. funestus gp" =
      "*An. funestus* gp",
    
    "An. sp." =
      "*An.* sp.",
    
    paste0(
      "*",
      taxon_name,
      "*"
    )
  )
}


# 14. Define observed-point colour scale

# Zero represents a sampled raster cell where the
# selected taxon was not collected during that survey month.
#
# Observed abundance increases from:
#
# dark black
# -> yellow
# -> light orange
# -> orange
# -> red.
#
# Colour uses a log(1 + x) transformation because
# observed mosquito abundance is strongly right-skewed.

observed_colour_scale <- function(...) {
  
  scale_colour_gradientn(
    colours = c(
      "#1A1A1A",
      "#FFFFB2",
      "#FECC5C",
      "#FD8D3C",
      "#E31A1C"
    ),
    
    values = c(
      0,
      0.001,
      0.25,
      0.60,
      1
    ),
    
    ...
  )
}


# 15. Create observed raster-cell layers

observed_layers <- function(
    taxon_name,
    month_name,
    compact = FALSE
) {
  
  # Select sampled raster cells for one
  # taxon and survey month.
  
  obs <- cell_points[
    cell_points$species ==
      taxon_name &
      cell_points$month_name ==
      month_name,
  ]
  
  
  if (
    nrow(obs) == 0
  ) {
    
    return(
      list()
    )
  }
  
  
  # Cap observed values at 10 for plotting.
  #
  # Values at or above 10 therefore share the
  # darkest red colour and largest point size.
  #
  # This affects only the visual display.
  # Original values remain unchanged in cell_stats.
  
  obs$plot_value <- pmin(
    obs$mosquitoes_per_household_sampling_occasion,
    observed_cap
  )
  
  
  # Cells based on fewer than four sampled households
  # are shown semi-transparently.
  #
  # Transparency is used only as a visual indication
  # and is not displayed as a separate legend.
  
  obs$point_alpha <- ifelse(
    obs$n_households < 4,
    0.45,
    1
  )
  
  
  # Draw larger observed values first so smaller
  # observations remain visible on top.
  
  obs <- obs[
    order(
      -obs$plot_value
    ),
  ]
  
  
  legend_name <- paste0(
    "Observed ",
    taxon_markdown(
      taxon_name
    ),
    "<br>per household<br>sampling occasion"
  )
  
  
  list(
    
    # Solid coloured circles represent observed
    # abundance within sampled raster cells.
    
    geom_spatvector(
      data = obs,
      aes(
        colour = plot_value,
        size = plot_value,
        alpha = point_alpha
      ),
      shape = 16
    ),
    
    
    # Thin dark outline so pale points remain
    # visible against the prediction surface.
    
    geom_spatvector(
      data = obs,
      aes(
        size = plot_value,
        alpha = point_alpha
      ),
      shape = 1,
      colour = "#1A1A1A",
      stroke =
        if (
          compact
        ) {
          0.25
        } else {
          0.35
        }
    ),
    
    
    # Use the specified transparency values
    # without creating an additional legend.
    
    scale_alpha_identity(
      guide = "none"
    ),
    
    
    # Observed-point colour scale.
    
    observed_colour_scale(
      name = legend_name,
      transform = "log1p",
      limits = c(
        0,
        observed_cap
      ),
      breaks = observed_breaks,
      labels = observed_labels,
      oob = squish,
      guide = guide_legend(
        order = 2
      )
    ),
    
    
    # Point size also increases with observed
    # mosquitoes per household sampling occasion.
    
    scale_size_continuous(
      name = legend_name,
      limits = c(
        0,
        observed_cap
      ),
      breaks = observed_breaks,
      labels = observed_labels,
      range =
        if (
          compact
        ) {
          c(
            0.5,
            3
          )
        } else {
          c(
            1,
            5.5
          )
        },
      guide = guide_legend(
        order = 2
      )
    )
  )
}


# 16. Create monthly prediction map

create_monthly_prediction_map <- function(
    taxon_name,
    month_name,
    compact = FALSE,
    show_observed = TRUE
) {
  
  # Select the MESS-masked prediction for
  # the requested taxon and calendar month.
  
  prediction_continuous <-
    monthly_detection_predictions_mess[[
      month_name
    ]][[
      taxon_name
    ]]
  
  
  # Select environmentally dissimilar areas
  # for the requested month.
  
  mess_excluded <-
    mess_excluded_polygons[[
      month_name
    ]]
  
  
  p <- ggplot()
  
  
  # Display environmentally dissimilar areas
  # in warm beige.
  
  if (
    nrow(mess_excluded) > 0
  ) {
    
    p <- p +
      geom_spatvector(
        data = mess_excluded,
        fill = "#EFE6D8",
        colour = NA,
        show.legend = FALSE
      )
  }
  
  
  # Add predicted probability of detection.
  
  p <- p +
    
    geom_spatraster(
      data = prediction_continuous
    ) +
    
    scale_fill_gradientn(
      colours = c(
        "#F7FBFF",
        "#C6DBEF",
        "#9ECAE1",
        "#4292C6",
        "#2F80BD",
        "#084081"
      ),
      
      values = c(
        0,
        0.10,
        0.25,
        0.50,
        0.75,
        1
      ),
      
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
        "0.00",
        "0.25",
        "0.50",
        "0.75",
        "1.00"
      ),
      
      name =
        "Predicted probability<br>of detection in a household",
      
      na.value =
        "transparent",
      
      guide = guide_colourbar(
        order = 1
      )
    ) +
    
    
    # Kasaï-Central boundary.
    
    geom_spatvector(
      data = kc_boundary,
      fill = NA,
      colour = "black",
      linewidth =
        if (
          compact
        ) {
          0.25
        } else {
          0.4
        }
    )
  
  
  # Overlay observed abundance in sampled raster cells.
  
  if (
    show_observed
  ) {
    
    p <- p +
      observed_layers(
        taxon_name,
        month_name,
        compact
      )
  }
  
  
  p <- p +
    
    theme_void(
      base_size = 10
    ) +
    
    theme(
      legend.title =
        element_markdown(
          size = 10,
          lineheight = 1.15
        )
    )
  
  
  # Compact map used in the 12-month figure.
  
  if (
    compact
  ) {
    
    p <- p +
      
      labs(
        title = month_name
      ) +
      
      theme(
        plot.title =
          element_text(
            size = 10,
            face = "bold",
            hjust = 0.5
          ),
        
        plot.margin =
          margin(
            2,
            2,
            2,
            2
          )
      )
    
  } else {
    
    # Individual monthly map.
    
    p <- p +
      
      labs(
        title = paste0(
          "Predicted probability of detecting ",
          taxon_markdown(
            taxon_name
          ),
          " in a household — ",
          month_name,
          "<br>across environmentally similar areas, Kasaï-Central, DRC"
        )
      ) +
      
      theme(
        plot.title =
          element_markdown(
            size = 13,
            face = "bold",
            hjust = 0.5,
            lineheight = 1.2
          )
      )
  }
  
  
  p
}


# 17. Create 12-month figure for one taxon

create_taxon_monthly_figure <- function(
    taxon_name,
    show_observed = TRUE
) {
  
  monthly_maps <- lapply(
    month.name,
    function(month_name) {
      
      create_monthly_prediction_map(
        taxon_name = taxon_name,
        month_name = month_name,
        compact = TRUE,
        show_observed = show_observed
      )
    }
  )
  
  
  wrap_plots(
    monthly_maps,
    ncol = 4,
    guides = "collect"
  ) +
    
    plot_annotation(
      title = paste0(
        "Monthly predicted probability of detecting ",
        taxon_markdown(
          taxon_name
        ),
        " in a household",
        "<br>across environmentally similar areas, Kasaï-Central, DRC"
      ),
      
      theme = theme(
        plot.title =
          element_markdown(
            size = 15,
            hjust = 0.5,
            lineheight = 1.2
          )
      )
    )
}


# 18. Create, display, and save monthly figures

dir.create(
  "outputs/figures/monthly_climate_landcover",
  recursive = TRUE,
  showWarnings = FALSE
)


# An. gambiae s.l.

gambiae_figure <- create_taxon_monthly_figure(
  "An. gambiae s.l."
)

print(
  gambiae_figure
)

ggsave(
  "outputs/figures/monthly_climate_landcover/kc_an_gambiae_monthly_predictions.png",
  gambiae_figure,
  width = 14,
  height = 11,
  units = "in",
  dpi = 300,
  bg = "white"
)


# An. funestus gp

funestus_figure <- create_taxon_monthly_figure(
  "An. funestus gp"
)

print(
  funestus_figure
)

ggsave(
  "outputs/figures/monthly_climate_landcover/kc_an_funestus_monthly_predictions.png",
  funestus_figure,
  width = 14,
  height = 11,
  units = "in",
  dpi = 300,
  bg = "white"
)


# An. paludis

paludis_figure <- create_taxon_monthly_figure(
  "An. paludis"
)

print(
  paludis_figure
)

ggsave(
  "outputs/figures/monthly_climate_landcover/kc_an_paludis_monthly_predictions.png",
  paludis_figure,
  width = 14,
  height = 11,
  units = "in",
  dpi = 300,
  bg = "white"
)


# An. hancocki

hancocki_figure <- create_taxon_monthly_figure(
  "An. hancocki"
)

print(
  hancocki_figure
)

ggsave(
  "outputs/figures/monthly_climate_landcover/kc_an_hancocki_monthly_predictions.png",
  hancocki_figure,
  width = 14,
  height = 11,
  units = "in",
  dpi = 300,
  bg = "white"
)



# An. moucheti

moucheti_figure <- create_taxon_monthly_figure(
  "An. moucheti"
)

print(
  moucheti_figure
)

ggsave(
  "outputs/figures/monthly_climate_landcover/kc_an_moucheti_monthly_predictions.png",
  moucheti_figure,
  width = 14,
  height = 11,
  units = "in",
  dpi = 300,
  bg = "white"
)


# An. ziemanni

ziemanni_figure <- create_taxon_monthly_figure(
  "An. ziemanni"
)

print(
  ziemanni_figure
)

ggsave(
  "outputs/figures/monthly_climate_landcover/kc_an_ziemanni_monthly_predictions.png",
  ziemanni_figure,
  width = 14,
  height = 11,
  units = "in",
  dpi = 300,
  bg = "white"
)


# 19. Save spatial outputs

dir.create(
  "outputs/spatial/monthly_climate_landcover",
  recursive = TRUE,
  showWarnings = FALSE
)


# Save unmasked monthly predictions.

saveRDS(
  monthly_detection_predictions,
  "outputs/spatial/monthly_climate_landcover/kc_monthly_detection_predictions.rds"
)


# Save MESS-masked monthly predictions.

saveRDS(
  monthly_detection_predictions_mess,
  "outputs/spatial/monthly_climate_landcover/kc_monthly_detection_predictions_mess.rds"
)


# Save observed raster-cell statistics.
#
# These retain the original observed abundance values,
# not the capped plotting values.

write_csv(
  cell_stats,
  "outputs/spatial/monthly_climate_landcover/kc_observed_raster_cell_statistics.csv"
)
