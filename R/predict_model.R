
# SPATIAL PREDICTION OF INDOOR VECTOR SPECIES 

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
# - raster cells represented in the sampling data;
# - collection months;
# - household sampling effort; and
# - environmental conditions represented in the model data.

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
# PCA covariates used to fit the GAM.

covs <- rast(
  "data/clean/covariates.tif"
)

# Inspect the covariate raster.

covs


# Confirm the number of covariates layers.

nlyr(
  covs
)


# Check covariates names.

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


# Create a sampling-effort layer for prediction.

# Setting n_households = 1 standardises predictions
# to one sampled household.

n_households_layer <- covs[[1]] * 0 + 1


names(
  n_households_layer
) <- "n_households"


# 6. Generate taxon-specific spatial predictions

# Use the fitted hierarchical GAM to predict
# Anopheles mosquito counts across Kasaï-Central.

# Predictions are standardised to one household
# because n_households = 1 in the prediction data.

# type = "response" returns predictions
# on the original count scale.

predicted_count_funestus <- predict(
  c(
    covs,
    species_layer_funestus,
    n_households_layer
  ),
  anopheles_hierarchical_gam,
  type = "response"
)


predicted_count_gambiae <- predict(
  c(
    covs,
    species_layer_gambiae,
    n_households_layer
  ),
  anopheles_hierarchical_gam,
  type = "response"
)


predicted_count_hancocki <- predict(
  c(
    covs,
    species_layer_hancocki,
    n_households_layer
  ),
  anopheles_hierarchical_gam,
  type = "response"
)


predicted_count_moucheti <- predict(
  c(
    covs,
    species_layer_moucheti,
    n_households_layer
  ),
  anopheles_hierarchical_gam,
  type = "response"
)


predicted_count_paludis <- predict(
  c(
    covs,
    species_layer_paludis,
    n_households_layer
  ),
  anopheles_hierarchical_gam,
  type = "response"
)


predicted_count_an_sp <- predict(
  c(
    covs,
    species_layer_an_sp,
    n_households_layer
  ),
  anopheles_hierarchical_gam,
  type = "response"
)


predicted_count_ziemanni <- predict(
  c(
    covs,
    species_layer_ziemanni,
    n_households_layer
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
# Predictions were standardised to one household
# by setting n_households = 1 during spatial prediction.


expected_count_per_household <-
  predicted_counts


# Extract the fitted negative-binomial dispersion parameter.

theta <- anopheles_hierarchical_gam$family$getTheta(
  TRUE
)


theta


# Calculate the probability of detecting at least one mosquito
# in one household using the negative-binomial distribution.

household_detection_probability <-
  1 - (
    theta /
      (
        theta +
          expected_count_per_household
      )
  )^theta


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

# 10. Define taxon order ------------------------------------------------------

taxa <- c(
  "An. gambiae s.l.",
  "An. funestus gp",
  "An. hancocki",
  "An. moucheti",
  "An. paludis",
  "An. sp.",
  "An. ziemanni"
)


# Taxa included in the combined figure.
# Unidentified An. sp. is excluded.

map_taxa <- c(
  "An. gambiae s.l.",
  "An. funestus gp",
  "An. paludis",
  "An. hancocki",
  "An. moucheti",
  "An. ziemanni"
)


# 11. Calculate Multivariate Environmental Similarity Surface ----------------

# MESS compares environmental conditions across Kasaï-Central
# with those represented by the sampled raster cells.

covraster <- raster::brick(
  "data/clean/covariates.tif"
)


# Retain one environmental record per sampled raster cell.

sampled_environmental_covariates <-
  model_data |>
  distinct(
    cell_id,
    .keep_all = TRUE
  ) |>
  select(
    starts_with("bioclim_pc"),
    starts_with("landcover_pc")
  ) |>
  as.data.frame()


# Check dimensions and predictor names.

dim(
  sampled_environmental_covariates
)

names(
  sampled_environmental_covariates
)

names(
  covraster
)


# Calculate MESS.

kc_mess_raw <-
  dismo::mess(
    x = covraster,
    v = sampled_environmental_covariates
  ) |>
  terra::rast()


# Restrict MESS to Kasaï-Central.

kc_mess_raw <-
  terra::mask(
    kc_mess_raw,
    kasai_central_boundary
  )


# 12. Clean MESS surface ------------------------------------------------------

# Infinite values occurred where environmental
# covariates were unavailable.

kc_mess_clean <- kc_mess_raw


kc_mess_clean[
  is.infinite(
    kc_mess_clean
  )
] <- NA


# Inspect cleaned MESS.

terra::global(
  kc_mess_clean,
  c(
    "min",
    "max",
    "mean"
  ),
  na.rm = TRUE
)


# 13. Create environmental-similarity mask -----------------------------------

# MESS >= 0 = environmentally represented.
# MESS < 0  = environmentally dissimilar.
# NA        = environmental data unavailable.

kc_mess_mask <-
  terra::ifel(
    kc_mess_clean >= 0,
    1,
    NA
  )


# 14. Apply MESS mask to predictions -----------------------------------------

household_detection_probability_mess <-
  terra::mask(
    household_detection_probability_kc,
    kc_mess_mask
  )


# Inspect final prediction ranges.

household_detection_probability_mess


terra::global(
  household_detection_probability_mess,
  c(
    "min",
    "max"
  ),
  na.rm = TRUE
)


# 15. Save spatial outputs ----------------------------------------------------

dir.create(
  "outputs/spatial",
  recursive = TRUE,
  showWarnings = FALSE
)


terra::writeRaster(
  kc_mess_clean,
  "outputs/spatial/kc_mess.tif",
  overwrite = TRUE
)


terra::writeRaster(
  kc_mess_mask,
  "outputs/spatial/kc_mess_mask.tif",
  overwrite = TRUE
)


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


# 16. Calculate observed abundance across 12 months --------------------------

# For each raster cell × taxon:
#
# total mosquitoes
# -----------------------------
# household sampling occasions
#
# = mean mosquitoes per household sampling occasion.

observed_cell_abundance_12m <-
  model_data |>
  group_by(
    cell_id,
    species
  ) |>
  summarise(
    total_mosquitoes = sum(
      count,
      na.rm = TRUE
    ),
    
    total_household_sampling_occasions = sum(
      n_households,
      na.rm = TRUE
    ),
    
    mean_mosquitoes_per_household_sampling_occasion =
      total_mosquitoes /
      total_household_sampling_occasions,
    
    .groups = "drop"
  )


# 17. Add raster-cell coordinates --------------------------------------------

# Coordinates are used only to display the aggregated
# raster-cell observations as points.

sampled_cell_ids <-
  sort(
    unique(
      observed_cell_abundance_12m$cell_id
    )
  )


cell_coordinates <-
  terra::xyFromCell(
    covs[[1]],
    sampled_cell_ids
  ) |>
  as.data.frame() |>
  mutate(
    cell_id = sampled_cell_ids,
    .before = 1
  )


observed_cell_abundance_12m <-
  observed_cell_abundance_12m |>
  left_join(
    cell_coordinates,
    by = "cell_id"
  )


# Check.

observed_cell_abundance_12m


stopifnot(
  sum(
    is.na(
      observed_cell_abundance_12m$x
    )
  ) == 0
)


# 18. Define common map scales ------------------------------------------------

prediction_class_labels <- c(
  "0–0.25",
  "0.25–0.50",
  "0.50–0.75",
  "0.75–1.00"
)


masked_class_label <-
  "Masked (environmentally dissimilar)"


all_prediction_classes <- c(
  masked_class_label,
  prediction_class_labels
)


prediction_class_colours <- setNames(
  c(
    "grey92",
    "#BDD7E7",
    "#6BAED6",
    "#3182BD",
    "#08519C"
  ),
  all_prediction_classes
)


# Probability classes are for visual display only.

probability_reclassification <- matrix(
  c(
    0.00, 0.25, 1,
    0.25, 0.50, 2,
    0.50, 0.75, 3,
    0.75, 1.000001, 4
  ),
  ncol = 3,
  byrow = TRUE
)


# Cells specifically excluded because MESS < 0.

environmentally_dissimilar_cells <-
  terra::ifel(
    kc_mess_clean < 0,
    0,
    NA
  )


# Use the same observed-abundance scale for every taxon.

maximum_observed_abundance <- max(
  observed_cell_abundance_12m$
    mean_mosquitoes_per_household_sampling_occasion,
  na.rm = TRUE
)


observed_abundance_breaks <- c(
  0,
  0.5,
  1,
  2,
  5
)


observed_abundance_breaks <-
  observed_abundance_breaks[
    observed_abundance_breaks <=
      maximum_observed_abundance
  ]


# 19. Define taxon names for map titles --------------------------------------

taxon_expression <- function(
    taxon_name
) {
  
  switch(
    taxon_name,
    
    "An. gambiae s.l." =
      quote(
        italic("An. gambiae") *
          " s.l."
      ),
    
    "An. funestus gp" =
      quote(
        italic("An. funestus") *
          " gp"
      ),
    
    "An. sp." =
      quote(
        italic("An.") *
          " sp."
      ),
    
    bquote(
      italic(
        .(taxon_name)
      )
    )
  )
}


taxon_title <- function(
    taxon_name
) {
  
  bquote(
    "Probability of detecting " *
      .(
        taxon_expression(
          taxon_name
        )
      ) *
      " in households"
  )
}


# 20. Create final prediction-map function -----------------------------------

create_final_prediction_map <- function(
    taxon_name,
    compact = FALSE,
    show_scale_bar = !compact
) {
  
  # Classify predicted probabilities.
  
  prediction_classified <-
    household_detection_probability_mess[[
      taxon_name
    ]] |>
    terra::classify(
      rcl = probability_reclassification,
      include.lowest = TRUE,
      right = FALSE
    ) |>
    terra::cover(
      environmentally_dissimilar_cells
    ) |>
    terra::mask(
      kasai_central_boundary
    ) |>
    terra::as.factor()
  
  
  # Add readable class labels.
  
  levels(
    prediction_classified
  ) <- data.frame(
    ID = 0:4,
    probability_class =
      all_prediction_classes
  )
  
  
  # Observed abundance for this taxon.
  
  observed_taxon <-
    observed_cell_abundance_12m |>
    filter(
      species == taxon_name
    ) |>
    arrange(
      mean_mosquitoes_per_household_sampling_occasion
    )
  
  
  # Base map.
  
  p <- ggplot() +
    
    # Predicted detection probability.
    
    tidyterra::geom_spatraster(
      data = prediction_classified
    ) +
    
    
    scale_fill_manual(
      values =
        prediction_class_colours,
      limits =
        all_prediction_classes,
      breaks = c(
        rev(
          prediction_class_labels
        ),
        masked_class_label
      ),
      name =
        "Household probability\nof detection",
      na.value =
        "transparent",
      na.translate =
        FALSE,
      drop =
        FALSE,
      guide =
        guide_legend(
          order = 1
        )
    ) +
    
    
    # Kasaï-Central boundary.
    
    tidyterra::geom_spatvector(
      data =
        kasai_central_boundary,
      fill =
        NA,
      colour =
        "black",
      linewidth =
        if (compact) 0.3 else 0.4
    ) +
    
    
    # Second fill scale for observed abundance.
    
    ggnewscale::new_scale_fill() +
    
    
    geom_point(
      data =
        observed_taxon,
      aes(
        x = x,
        y = y,
        fill =
          mean_mosquitoes_per_household_sampling_occasion
      ),
      shape =
        21,
      size =
        if (compact) 2 else 3,
      stroke =
        if (compact) 0.3 else 0.4,
      colour =
        "black"
    ) +
    
    
    scale_fill_distiller(
      palette =
        "YlOrRd",
      direction =
        1,
      transform =
        "log1p",
      limits = c(
        0,
        maximum_observed_abundance
      ),
      breaks =
        observed_abundance_breaks,
      name =
        "Mean mosquitoes per\nhousehold sampling occasion",
      guide =
        guide_colourbar(
          order = 2,
          barheight =
            grid::unit(
              35,
              "mm"
            ),
          frame.colour =
            "black",
          ticks.colour =
            "black"
        )
    ) +
    
    
    theme_void(
      base_size = 10
    )
  
  
  # Compact map for combined figure.
  
  if (
    compact
  ) {
    
    p <- p +
      
      labs(
        title =
          taxon_expression(
            taxon_name
          )
      ) +
      
      theme(
        plot.title =
          element_text(
            size = 11,
            hjust = 0.5
          ),
        plot.margin =
          margin(
            4,
            4,
            4,
            4
          )
      )
    
    
  } else {
    
    # Full individual map.
    
    p <- p +
      
      labs(
        title =
          taxon_title(
            taxon_name
          ),
        
        subtitle =
          "Kasaï-Central, DRC, predictions shown only in areas environmentally similar to sampled sites"
      ) +
      
      theme(
        plot.title =
          element_text(
            size = 13,
            face = "bold",
            hjust = 0
          ),
        plot.subtitle =
          element_text(
            size = 9.5,
            colour = "grey30",
            hjust = 0,
            margin =
              margin(
                b = 6
              )
          ),
        plot.title.position =
          "plot",
        legend.title =
          element_text(
            size = 9,
            lineheight = 1.1
          ),
        legend.text =
          element_text(
            size = 8.5
          ),
        legend.spacing.y =
          grid::unit(
            4,
            "mm"
          ),
        legend.box.just =
          "left",
        plot.margin =
          margin(
            8,
            8,
            8,
            8
          ),
        plot.background =
          element_rect(
            fill = "white",
            colour = NA
          )
      )
  }
  
  
  p
}


# 21. Create individual maps --------------------------------------------------

# Create one individual map for each identified taxon.
# An. sp. is excluded.

final_prediction_maps <-
  lapply(
    map_taxa,
    create_final_prediction_map,
    compact = FALSE
  )


names(
  final_prediction_maps
) <- map_taxa


# Display individual maps.

final_prediction_maps[[
  "An. gambiae s.l."
]]

final_prediction_maps[[
  "An. funestus gp"
]]

final_prediction_maps[[
  "An. paludis"
]]

final_prediction_maps[[
  "An. hancocki"
]]

final_prediction_maps[[
  "An. moucheti"
]]

final_prediction_maps[[
  "An. ziemanni"
]]


# 22. Save individual maps ----------------------------------------------------

dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)


for (
  taxon_name in map_taxa
) {
  
  file_taxon_name <-
    gsub(
      "[^A-Za-z0-9]+",
      "_",
      taxon_name
    )
  
  
  ggsave(
    filename =
      file.path(
        "outputs/figures",
        paste0(
          file_taxon_name,
          "_detection_prediction.png"
        )
      ),
    plot =
      final_prediction_maps[[
        taxon_name
      ]],
    width =
      7.5,
    height =
      8,
    units =
      "in",
    dpi =
      600,
    bg =
      "white"
  )
}
# 23. Create combined maps ----------------------------------------------------

combined_taxon_maps <-
  lapply(
    seq_along(
      map_taxa
    ),
    function(
    i
    ) {
      
      p <- create_final_prediction_map(
        taxon_name =
          map_taxa[i],
        compact =
          TRUE,
        show_scale_bar =
          i == 1
      )
      
      
      # Keep legends only for the first panel.
      
      if (
        i > 1
      ) {
        
        p <- p +
          theme(
            legend.position = "none"
          )
      }
      
      
      p
    }
  )


# 24. Combine six identified taxa --------------------------------------------

combined_taxon_maps[-1] <- lapply(
  combined_taxon_maps[-1],
  function(p) p + theme(legend.position = "none")
)


combined_all_taxa_prediction_map <-
  patchwork::wrap_plots(
    combined_taxon_maps,
    ncol = 3,
    guides = "collect"
  ) +
  
  patchwork::plot_annotation(
    title =
      "Predicted distributions masked to environmentally similar raster cells, Kasaï-Central, DRC",
    
    tag_levels =
      "a",
    
    theme = theme(
      plot.title =
        element_text(
          size = 15,
          face = "bold",
          hjust = 0.5,
          margin = margin(
            b = 10
          )
        ),
      
      plot.background =
        element_rect(
          fill = "white",
          colour = NA
        )
    )
  ) &
  
  theme(
    plot.tag =
      element_text(
        size = 11,
        face = "bold"
      ),
    
    legend.box =
      "vertical",
    
    legend.title =
      element_text(
        size = 9,
        face = "bold"
      ),
    
    legend.text =
      element_text(
        size = 8.5
      ),
    
    legend.spacing.y =
      grid::unit(
        5,
        "mm"
      )
  )


# Display combined figure.

combined_all_taxa_prediction_map


# 25. Save combined figure ----------------------------------------------------

ggsave(
  filename =
    "outputs/figures/kc_all_anopheles_detection_predictions.png",
  plot =
    combined_all_taxa_prediction_map,
  width =
    13,
  height =
    9,
  units =
    "in",
  dpi =
    600,
  bg =
    "white"
)
