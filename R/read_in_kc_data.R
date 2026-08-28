# 56. Create the PAMCA key map of representative vector-composition patterns ----

# Define the three health zones selected to represent contrasting
# vector-composition patterns in the PAMCA poster:
#
# - Mutoto: An. gambiae s.l. dominance;
# - Kananga: An. funestus gp dominance; and
# - Bobozo: secondary vector species dominance.

pamca_map_key <- tibble(
  health_zone = c(
    "Mutoto",
    "Kananga",
    "Bobozo"
  ),
  selected_pattern = c(
    "An. gambiae s.l. dominance",
    "An. funestus gp dominance",
    "Secondary vector species dominance"
  )
)


# Define colours consistent with the species-composition figures.

pamca_pattern_colors <- c(
  "An. gambiae s.l. dominance" = "firebrick",
  "An. funestus gp dominance" = "goldenrod",
  "Secondary vector species dominance" = "grey40"
)


# Select the sampled household locations in the three
# representative health zones and assign their corresponding
# vector-composition pattern.

pamca_household_points <- kc_sf |>
  dplyr::filter(
    health_zone %in% c(
      "Mutoto",
      "Kananga",
      "Bobozo"
    )
  ) |>
  dplyr::left_join(
    pamca_map_key,
    by = "health_zone"
  )


# Create the external boundary of Kasaï-Central.

kc_outline <- sf::st_union(
  kc_hz
)


# Create label positions for the three representative health zones.

pamca_hz_labels <- kc_hz |>
  dplyr::filter(
    health_zone %in% c(
      "Mutoto",
      "Kananga",
      "Bobozo"
    )
  ) |>
  sf::st_point_on_surface()


# Create the PAMCA key map.

p_pamca_key_map <- ggplot() +
  
  # Display all 26 Kasaï-Central health zones.
  
  geom_sf(
    data = kc_hz,
    fill = "grey97",
    colour = "grey60",
    linewidth = 0.4
  ) +
  
  # Draw a stronger external boundary around Kasaï-Central.
  
  geom_sf(
    data = kc_outline,
    fill = NA,
    colour = "grey25",
    linewidth = 0.8
  ) +
  
  # Add the sampled household locations from the three
  # representative health zones.
  
  geom_sf(
    data = pamca_household_points,
    aes(
      colour = selected_pattern
    ),
    size = 6
  ) +
  
  # Apply colours corresponding to the representative
  # vector-composition patterns.
  
  scale_colour_manual(
    values = pamca_pattern_colors
  ) +
  
  # Keep the map closely fitted to Kasaï-Central.
  
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  
  # Remove axes, title, and legend.
  
  theme_void() +
  
  theme(
    legend.position = "none",
    plot.margin = margin(
      5,
      5,
      5,
      5
    )
  )


# Display the PAMCA key map.

p_pamca_key_map

# 57. Save the PAMCA key map in high resolution -------------------------------

# Save the PAMCA key map as a high-resolution PNG.

ggsave(
  filename = "outputs/figures/pamca_representative_vector_patterns_map.png",
  plot = p_pamca_key_map,
  width = 5.14,
  height = 6.74,
  units = "in",
  dpi = 300,
  bg = "white"
)

# 58. Save household location data for subsequent spatial analyses -------------

# Save the household location dataset imported from the
# "location" worksheet of the Kasaï-Central entomological database.

write_csv(
  kc_location,
  "data/clean/kc_household_coords.csv"
)

# Confirm that the household-coordinate dataset was saved successfully.

file.exists(
  "data/clean/kc_household_coords.csv"
)


# 59. Save cleaned entomological datasets for count-data preparation

# The subsequent count-data workflow requires two complementary datasets:
#
# 1. kc_mosq:
#    contains all household collection events, including collection month,
#    date, household identifiers, and the total number of Anopheles collected.
#
# 2. kc_species:
#    contains the individual mosquitoes identified during those
#    household collection events.

# Prepare the cleaned species-identification dataset.
#
# kc_species_plot already contains:
#
# - collection dates converted to Date format; and
# - the standardised "An. sp." taxonomic label.
#
# collection_month_label was created only for visualisation,
# so it is removed from the cleaned source dataset.

kc_species_clean <- kc_species_plot |>
  select(
    -collection_month_label
  )


# Save the cleaned household collection-event dataset.
#
# Keep kc_mosq rather than kc_mosq_clean because kc_mosq
# retains all 7,800 household collection events:
# 650 fixed households × 12 collection months.
#
# Preserving these monthly observations is necessary for subsequent
# temporal analyses and count-data preparation.

write_csv(
  kc_mosq,
  "data/clean/kc_mosquito_collections.csv"
)


# Save the cleaned individual mosquito-identification dataset.

write_csv(
  kc_species_clean,
  "data/clean/kc_species_identifications.csv"
)


# Confirm that both cleaned entomological datasets
# were saved successfully.

file.exists(
  "data/clean/kc_mosquito_collections.csv"
)

file.exists(
  "data/clean/kc_species_identifications.csv"
)


# Confirm that the total number of Anopheles recorded in the
# household collection dataset matches the number of individual
# mosquito records in the species-identification dataset.

sum(
  kc_mosq$n_anopheles_collected
)

nrow(
  kc_species_clean
)


#### For PAMCA

# Load DRC provincial boundaries -----------------------------------------------

drc_provinces <- geodata::gadm(
  country = "COD",
  level = 1,
  path = "data/downloads"
) |>
  sf::st_as_sf() |>
  janitor::clean_names() |>
  dplyr::rename(
    province = name_1
  )


# Check the provincial boundary dataset.

nrow(
  drc_provinces
)

sort(
  drc_provinces$province
)


# Create the DRC outer boundary.

drc_outline <- drc_provinces |>
  summarise()


# Calculate the mean GPS location of the households surveyed
# in Mikalayi.

mikalayi_sentinel_site <- kc_location |>
  filter(
    health_zone == "Mikalayi"
  ) |>
  summarise(
    long_dd = mean(
      long_dd,
      na.rm = TRUE
    ),
    lat_dd = mean(
      lat_dd,
      na.rm = TRUE
    )
  ) |>
  st_as_sf(
    coords = c(
      "long_dd",
      "lat_dd"
    ),
    crs = 4326
  )


# Create the DRC study-location map.

p_drc_kc_key_map <- ggplot() +
  
  # Display the 26 DRC provinces.
  
  geom_sf(
    data = drc_provinces,
    fill = "white",
    colour = "grey45",
    linewidth = 0.30
  ) +
  
  # Highlight Kasaï-Central Province.
  
  geom_sf(
    data = drc_provinces |>
      filter(
        province == "Kasaï-Central"
      ),
    aes(
      fill = "Kasaï-Central Province"
    ),
    colour = "black",
    linewidth = 0.45
  ) +
  
  # Draw the external boundary of the DRC.
  
  geom_sf(
    data = drc_outline,
    fill = NA,
    colour = "black",
    linewidth = 1
  ) +
  
  # Add the Mikalayi sentinel site.
  
  geom_sf(
    data = mikalayi_sentinel_site,
    aes(
      shape = "NMCP entomological sentinel site (Mikalayi)"
    ),
    size = 3.5,
    colour = "black"
  ) +
  
  # Define the colour of Kasaï-Central Province.
  
  scale_fill_manual(
    values = c(
      "Kasaï-Central Province" = "firebrick"
    )
  ) +
  
  # Define the symbol for the Mikalayi sentinel site.
  
  scale_shape_manual(
    values = c(
      "NMCP entomological sentinel site (Mikalayi)" = 16
    )
  ) +
  
  # Remove coordinate axes and keep the map tightly fitted.
  
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  
  # Remove legend titles.
  
  labs(
    fill = NULL,
    shape = NULL
  ) +
  
  # Control legend order.
  
  guides(
    fill = guide_legend(
      order = 1
    ),
    shape = guide_legend(
      order = 2
    )
  ) +
  
  # Use a clean map theme.
  
  theme_void() +
  
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    
    legend.text = element_text(
      size = 9
    ),
    
    legend.key.width = grid::unit(
      0.45,
      "cm"
    ),
    
    legend.key.height = grid::unit(
      0.45,
      "cm"
    ),
    
    plot.margin = margin(
      5,
      5,
      5,
      5
    )
  )


# Display the map.

p_drc_kc_key_map


# Save the DRC study-location map in high resolution ----------------------------

ggsave(
  filename = "outputs/figures/drc_kasai_central_mikalayi_location_map.png",
  plot = p_drc_kc_key_map,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)
