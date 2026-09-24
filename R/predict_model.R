
# SPATIAL PREDICTION OF INDOOR VECTORS SPECIES 

# 1. Load packages

source(
  "R/packages.R"
)


# 2. Load prepared modelling dataframe

# The modelling dataframe was created previously
# in prepare_model_data.R.

# It is required here for:
#
# - taxon names;
# - sampled household coordinates;
# - observed mosquito counts; and
# - environmental conditions represented at sampled households.

model_data <- readRDS(
  "data/clean/kc_anopheles_model_data.rds"
)


# Inspect the modelling dataframe.

model_data |>
  glimpse()


dim(
  model_data
)


# 3. Load fitted hierarchical GAM

# The model was fitted and saved previously
# in fit_model.R.

anopheles_hierarchical_gam <- readRDS(
  "outputs/anopheles_hierarchical_gam.rds"
)


# Inspect the fitted model.

summary(
  anopheles_hierarchical_gam
)


# 4. Load environmental covariates for spatial prediction

# These raster layers contain the same nine environmental
# PCA predictors used to fit the GAM.

covs <- rast(
  "data/clean/covariates.tif"
)


covs


names(
  covs
)


# 5. Prepare species-specific layers for spatial prediction

# Confirm the exact taxon names used in the fitted model.

levels(
  model_data$species
)


# Create a template raster for the species variable.

# The first environmental covariate is used only
# to inherit the spatial extent, resolution and CRS.

cov_species_dummy <- covs[[1]] * 0


names(
  cov_species_dummy
) <- "species"


# Create one copy of the species template
# for each Anopheles taxon.

species_layer_funestus <-
  species_layer_gambiae <-
  species_layer_hancocki <-
  species_layer_moucheti <-
  species_layer_paludis <-
  species_layer_an_sp <-
  species_layer_ziemanni <-
  cov_species_dummy


# Assign the corresponding taxon name.

species_layer_funestus[] <-
  "An. funestus gp"


species_layer_gambiae[] <-
  "An. gambiae s.l."


species_layer_hancocki[] <-
  "An. hancocki"


species_layer_moucheti[] <-
  "An. moucheti"


species_layer_paludis[] <-
  "An. paludis"


species_layer_an_sp[] <-
  "An. sp."


species_layer_ziemanni[] <-
  "An. ziemanni"


# 6. Generate taxon-specific spatial predictions

# Use the fitted hierarchical GAM to predict
# Anopheles mosquito counts across Kasaï-Central.

# type = "response" returns predictions
# on the original count scale.

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


# Assign informative layer names.

names(
  predicted_count_funestus
) <- "An. funestus gp"


names(
  predicted_count_gambiae
) <- "An. gambiae s.l."


names(
  predicted_count_hancocki
) <- "An. hancocki"


names(
  predicted_count_moucheti
) <- "An. moucheti"


names(
  predicted_count_paludis
) <- "An. paludis"


names(
  predicted_count_an_sp
) <- "An. sp."


names(
  predicted_count_ziemanni
) <- "An. ziemanni"


# 7. Combine taxon-specific predicted counts

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


# 8. Convert predicted counts to household detection probability

# NOTE:
#
# This keeps the probability calculation from the
# current modelling workflow.
#
# The probability formulation will be considered
# separately from this script-organisation change.

expected_count_per_household <-
  predicted_counts


household_detection_probability <-
  1 - exp(
    -expected_count_per_household
  )


household_detection_probability


# 9. Restrict predictions to Kasaï-Central

# Load boundaries of the 26 health zones included
# in the Kasaï-Central study.

kasai_central_health_zones <- vect(
  "data/clean/kc_health_zones.gpkg"
)


# Dissolve the health-zone polygons
# to create one Kasaï-Central boundary.

kasai_central_boundary <- aggregate(
  kasai_central_health_zones
)


# Mask predictions to Kasaï-Central.

household_detection_probability_kc <- mask(
  household_detection_probability,
  kasai_central_boundary
)


# 10. Prepare observed site counts for plotting

# model_data already contains:
#
# - household mosquito counts;
# - household coordinates; and
# - taxon information.

# Combine household-level counts within each surveyed site.

observed_site_counts <- model_data |>
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


# Identify the maximum observed site count
# across all taxa.

maximum_site_count_all_taxa <- max(
  observed_site_counts$total_count,
  na.rm = TRUE
)


# Create a common proportional-circle size.

# The same formula is used for every taxon,
# so identical mosquito counts have identical point sizes.

observed_site_counts <- observed_site_counts |>
  mutate(
    point_size =
      1.8 +
      4.2 * sqrt(
        total_count /
          maximum_site_count_all_taxa
      )
  )


# 11. Define taxon order

taxa <- c(
  "An. gambiae s.l.",
  "An. funestus gp",
  "An. hancocki",
  "An. moucheti",
  "An. paludis",
  "An. sp.",
  "An. ziemanni"
)


# 11. Define taxon order

taxa <- c(
  "An. gambiae s.l.",
  "An. funestus gp",
  "An. hancocki",
  "An. moucheti",
  "An. paludis",
  "An. sp.",
  "An. ziemanni"
)


# 12. Create function for taxon-specific prediction maps

create_prediction_map <- function(
    taxon_name
) {
  
  # Select prediction raster for the taxon.
  
  prediction_raster <-
    household_detection_probability_kc[[
      taxon_name
    ]]
  
  
  # Select surveyed sites where the taxon was observed.
  
  site_counts <-
    observed_site_counts |>
    filter(
      species == taxon_name,
      total_count > 0
    )
  
  
  # Create map.
  
  ggplot() +
    
    # Add predicted probability surface.
    
    tidyterra::geom_spatraster(
      data = prediction_raster
    ) +
    
    
    # Apply common probability colour scale.
    #
    # Light blue = low probability.
    # Dark blue = high probability.
    #
    # The same 0-1 scale is used for all taxa,
    # allowing direct comparison between maps.
    
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
      name =
        "Household\nprobability\nof detection",
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
    
    
    # Add Kasaï-Central boundary.
    
    tidyterra::geom_spatvector(
      data = kasai_central_boundary,
      fill = NA,
      colour = "black",
      linewidth = 0.4
    ) +
    
    
    # Overlay observed mosquito counts.
    
    geom_point(
      data = site_counts,
      mapping = aes(
        x = long_dd,
        y = lat_dd,
        size = point_size
      ),
      shape = 21,
      fill = "black",
      colour = "white",
      stroke = 0.4
    ) +
    
    
    # Use the point sizes calculated previously.
    
    scale_size_identity(
      guide = "none"
    ) +
    
    
    # Add taxon name.
    
    labs(
      title = bquote(
        italic(.(taxon_name))
      )
    ) +
    
    
    # Remove axes and map background.
    
    theme_void() +
    
    
    # Format title and legend.
    
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 14
      ),
      legend.position = "right",
      legend.box.spacing =
        grid::unit(
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


# 13. Create prediction maps for all seven taxa

prediction_maps <- lapply(
  taxa,
  create_prediction_map
)


names(
  prediction_maps
) <- taxa


# 14. Display individual prediction maps

prediction_maps[[
  "An. gambiae s.l."
]]


prediction_maps[[
  "An. funestus gp"
]]


prediction_maps[[
  "An. hancocki"
]]


prediction_maps[[
  "An. moucheti"
]]


prediction_maps[[
  "An. paludis"
]]


prediction_maps[[
  "An. sp."
]]


prediction_maps[[
  "An. ziemanni"
]]


# Combine all seven taxon-specific maps.

combined_prediction_maps <-
  patchwork::wrap_plots(
    prediction_maps,
    ncol = 4,
    guides = "collect"
  ) &
  
  theme(
    legend.position = "right"
  )


combined_prediction_maps


# 15. Calculate Multivariate Environmental Similarity Surface

# MESS compares environmental conditions
# across Kasaï-Central with environmental conditions
# represented at the 650 sampled households.

covraster <- brick(
  covs
)


# model_data contains one row per household × taxon.

# Therefore, retain only one environmental record
# per sampled household.

sampled_environmental_covariates <-
  model_data |>
  distinct(
    health_zone,
    health_area,
    village,
    house_number,
    .keep_all = TRUE
  ) |>
  select(
    starts_with("bioclim_pc"),
    starts_with("landcover_pc")
  ) |>
  as.data.frame()


# Confirm the expected 650 household records.

dim(
  sampled_environmental_covariates
)


# Calculate MESS.

kc_mess <- dismo::mess(
  x = covraster,
  v = sampled_environmental_covariates
) |>
  rast()


# Restrict MESS to Kasaï-Central.

kc_mess <- terra::mask(
  kc_mess,
  kasai_central_boundary
)


# Inspect MESS distribution.

terra::global(
  kc_mess,
  c(
    "min",
    "max",
    "mean"
  ),
  na.rm = TRUE
)


# 16. Create environmental-similarity mask

# MESS >= 0:
# environmental conditions are represented
# within the sampled environmental range.

# MESS < 0:
# environmental conditions are outside
# the sampled environmental range.

kc_mess_mask <- terra::ifel(
  kc_mess >= 0,
  1,
  NA
)


# 17. Quantify environmentally similar prediction area

# Identify all raster cells available
# for prediction within Kasaï-Central.

kc_prediction_area <- terra::mask(
  covs[[1]],
  kasai_central_boundary
)


# Count all prediction cells.

total_kc_cells <- terra::global(
  terra::ifel(
    !is.na(
      kc_prediction_area
    ),
    1,
    NA
  ),
  "sum",
  na.rm = TRUE
)[1, 1]


# Count environmentally similar cells.

environmentally_similar_cells <-
  terra::global(
    kc_mess_mask,
    "sum",
    na.rm = TRUE
  )[1, 1]


# Calculate percentage of Kasaï-Central
# represented by sampled environmental conditions.

percent_environmentally_similar <-
  100 *
  environmentally_similar_cells /
  total_kc_cells


percent_environmentally_similar


# 18. Save MESS surfaces

terra::writeRaster(
  kc_mess,
  "outputs/spatial/kc_mess.tif",
  overwrite = TRUE
)


terra::writeRaster(
  kc_mess_mask,
  "outputs/spatial/kc_mess_mask.tif",
  overwrite = TRUE
)


# 19. Apply MESS mask to model predictions

household_detection_probability_mess <-
  terra::mask(
    household_detection_probability_kc,
    kc_mess_mask
  )


household_detection_probability_mess


# 20. Create function for MESS-supported prediction maps

create_mess_prediction_map <- function(
    taxon_name
) {
  
  # Select prediction raster.
  
  prediction_raster <-
    household_detection_probability_mess[[
      taxon_name
    ]]
  
  
  # Select observed sites.
  
  site_counts <-
    observed_site_counts |>
    filter(
      species == taxon_name,
      total_count > 0
    )
  
  
  # Create map.
  
  ggplot() +
    
    # Fill unsupported environmental areas in grey.
    
    tidyterra::geom_spatvector(
      data = kasai_central_boundary,
      fill = "grey90",
      colour = NA
    ) +
    
    # Overlay supported predictions.
    
    tidyterra::geom_spatraster(
      data = prediction_raster
    ) +
    
    # Probability scale.
    
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
      name =
        "Household\nprobability\nof detection",
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
    
    # Add Kasaï-Central boundary.
    
    tidyterra::geom_spatvector(
      data = kasai_central_boundary,
      fill = NA,
      colour = "black",
      linewidth = 0.4
    ) +
    
    # Overlay observed counts.
    
    geom_point(
      data = site_counts,
      mapping = aes(
        x = long_dd,
        y = lat_dd,
        size = point_size
      ),
      shape = 21,
      fill = "black",
      colour = "white",
      stroke = 0.4
    ) +
    
    scale_size_identity(
      guide = "none"
    ) +
    
    labs(
      title = bquote(
        italic(.(taxon_name))
      )
    ) +
    
    theme_void() +
    
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 14
      ),
      legend.position = "right",
      legend.box.spacing =
        grid::unit(
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


# 21. Create MESS-supported prediction maps

mess_prediction_maps <- lapply(
  taxa,
  create_mess_prediction_map
)


names(
  mess_prediction_maps
) <- taxa


# Display individual MESS-supported maps.

mess_prediction_maps[[
  "An. gambiae s.l."
]]


mess_prediction_maps[[
  "An. funestus gp"
]]


mess_prediction_maps[[
  "An. hancocki"
]]


mess_prediction_maps[[
  "An. moucheti"
]]


mess_prediction_maps[[
  "An. paludis"
]]


mess_prediction_maps[[
  "An. sp."
]]


mess_prediction_maps[[
  "An. ziemanni"
]]


# Combine all MESS-supported maps.

combined_prediction_maps_mess <-
  patchwork::wrap_plots(
    mess_prediction_maps,
    ncol = 4,
    guides = "collect"
  ) +
  
  patchwork::plot_annotation(
    title =
      "Predicted household probability of detection within environmentally similar areas"
  ) &
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 11
    ),
    legend.position = "right"
  )


combined_prediction_maps_mess


# 22. Create transparent poster version

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


combined_prediction_maps_mess_poster


# Save if required.

# ggsave(
#   filename =
#     "outputs/figures/kc_anopheles_mess_predictions_blue_poster.png",
#   plot =
#     combined_prediction_maps_mess_poster,
#   width = 14,
#   height = 8,
#   units = "in",
#   dpi = 600,
#   bg = "transparent"
# )


# 23. Save spatial prediction outputs

terra::writeRaster(
  household_detection_probability_kc,
  "outputs/spatial/kc_household_detection_probability.tif",
  overwrite = TRUE
)


terra::writeRaster(
  household_detection_probability_mess,
  "outputs/spatial/kc_household_detection_probability_mess.tif",
  overwrite = TRUE
)


# 24. Create extra poster figure with five Anopheles taxa ---------------------


# Load repeated household × taxon × collection-round data.

# This dataset is required here because model_data
# contains pooled counts and therefore no longer
# contains collection_month.

count_data <- read_csv(
  "data/clean/kc_anopheles_count_data.csv",
  show_col_types = FALSE
)


# Calculate the number of collection months
# in which each Anopheles taxon was detected
# at each surveyed site.

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


# Add monthly detection information
# to observed site counts.

observed_site_counts_poster <-
  observed_site_counts |>
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


# Check monthly detection information.

observed_site_counts_poster |>
  select(
    species,
    health_zone,
    total_count,
    months_detected,
    proportion_months_detected
  )


# 25. Define five taxa included in poster

poster_taxa <- c(
  "An. gambiae s.l.",
  "An. funestus gp",
  "An. hancocki",
  "An. moucheti",
  "An. paludis"
)


# 26. Create plotting function for five-taxa poster

create_mess_prediction_map_poster <-
  function(
    taxon_name
  ) {
    
    # Select prediction raster.
    
    prediction_raster <-
      household_detection_probability_mess[[
        taxon_name
      ]]
    
    
    # Select observed sites.
    
    site_counts <-
      observed_site_counts_poster |>
      filter(
        species == taxon_name,
        total_count > 0
      )
    
    
    # Create map.
    
    ggplot() +
      
      # Environmentally unsupported areas.
      
      tidyterra::geom_spatvector(
        data = kasai_central_boundary,
        fill = "grey90",
        colour = NA
      ) +
      
      # Predicted probability surface.
      
      tidyterra::geom_spatraster(
        data = prediction_raster
      ) +
      
      # Probability colour scale.
      
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
        name =
          "Household\nprobability\nof detection",
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
      
      # Kasaï-Central boundary.
      
      tidyterra::geom_spatvector(
        data = kasai_central_boundary,
        fill = NA,
        colour = "black",
        linewidth = 0.4
      ) +
      
      # Observed mosquito information.
      #
      # Point size = total mosquito count.
      # Point colour = proportion of months detected.
      
      geom_point(
        data = site_counts,
        mapping = aes(
          x = long_dd,
          y = lat_dd,
          size = point_size,
          colour =
            proportion_months_detected
        ),
        shape = 16
      ) +
      
      scale_size_identity(
        guide = "none"
      ) +
      
      # Temporal persistence colour scale.
      
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
        name =
          "Months\ndetected"
      ) +
      
      labs(
        title = bquote(
          italic(.(taxon_name))
        )
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


# 27. Create five poster maps

poster_prediction_maps <- lapply(
  poster_taxa,
  create_mess_prediction_map_poster
)


names(
  poster_prediction_maps
) <- poster_taxa


# 28. Combine five taxon-specific poster maps

combined_prediction_maps_mess_5_taxa <-
  patchwork::wrap_plots(
    poster_prediction_maps,
    ncol = 3,
    guides = "collect"
  ) +
  
  patchwork::plot_annotation(
    title =
      "Predicted household probability of detection within environmentally similar areas"
  ) &
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 11
    ),
    legend.position = "right"
  )


combined_prediction_maps_mess_5_taxa


# 29. Create transparent poster version

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


combined_prediction_maps_mess_5_taxa_poster


# 30. Save five-taxa poster figure

ggsave(
  filename =
    "outputs/figures/kc_anopheles_5_taxa_detection_months_poster.png",
  plot =
    combined_prediction_maps_mess_5_taxa_poster,
  width = 18,
  height = 12,
  units = "in",
  dpi = 600,
  bg = "transparent"
)