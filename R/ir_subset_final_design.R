# IR DESIGN STRATEGIE
# Random selection of Anopheles gambiae s.l. samples for 8 PCR plates

# PURPOSE:

# This script creates a comprehensive subset of Anopheles gambiae s.l.
# specimens for ir molecular analysis.

# The script starts directly from the main entomological database and uses
# the worksheet called "species".

# It calculates the required sampling quotas, randomly selects the individual
# mosquito specimens, assigns the selected specimens to eight PCR plates,
# validates the final sampling design and exports a new Excel workbook.

# PRIMARY ANALYSIS PERIOD

# The mass mosquito-net campaign occurred during the month of September 2025
# in KC, which is consider as month 6 in our survey.

# To use comparable periods of equal duration, the primary analysis includes
# three months immediately before and three months immediately after the
# mass campaign.

# Before the mass campaign:
#   - month_3 : June 2025
#   - month_4 : July 2025
#   - month_5 : August 2025

# Campaign and transition period:
#   - month_6 : September
#   - excluded from the primary sampling design

# After the mass campaign:
#   - month_7
#   - month_8
#   - month_9

# Months 1 and 2 are not included in the primary analysis but may later be
# considered in an earlier-baseline analysis.

# Months 10, 11 and 12 may later be considered as a longer-term
# post-campaign follow-up period.


# MOSQUITO SPECIES

# Only specimens identified as:
#
#   An. gambiae s.l.
#
# are included in the current insecticide-resistance sampling design,
# later we will do the same for An. funestus group.


# INTERVENTION GROUPS

# The intervention was assigned at the health-zone level.

# Every household within the same health zone belongs to the same
# intervention group.

# Dual-active-ingredient mosquito-net health zones:

#   1. Benaleka
#   2. Bilomba
#   3. Bobozo
#   4. Bunkonde
#   5. Demba
#   6. Dibaya
#   7. Kananga
#   8. Lubondaie
#   9. Lubunga
#  10. Luiza
#  11. Masuika
#  12. Mikalayi
#  13. Muetshi
#  14. Mutoto
#  15. Tshikaji
#  16. Tshikula


# PBO mosquito-net health zones:

#   1. Benatshiadi
#   2. Kalomba
#   3. Katende
#   4. Luambo
#   5. Ndekesha
#   6. Ndesha
#   7. Tshibala
#   8. Yangala


# No-net comparison health zones:

#   1. Katoka
#   2. Lukonga


#MAIN SAMPLING VARIABLES

# The sampling design considers:

#   1. Intervention group
#   2. Health zone
#   3. Collection month
#   4. Sampling period
#   5. Available number of An. gambiae s.l. specimens
#   6. Required number of specimens
#   7. Household variation
#   8. Individual mosquito identifier
#   9. PCR plate assignment

#TREATMENT-BY-MONTH TARGETS

# The total number required for each primary sampling period is 376 specimens.


# Dual-active-ingredient mosquito nets:

# Before the mass campaign:
#   - month_3: 80 specimens
#   - month_4: 80 specimens
#   - month_5: 80 specimens
#   - total:   240 specimens

# After the mass campaign:
#   - month_7: 80 specimens
#   - month_8: 80 specimens
#   - month_9: 80 specimens
#   - total:   240 specimens


# PBO mosquito nets:

# Before the mass campaign:
#   - month_3: 35 specimens
#   - month_4: 35 specimens
#   - month_5: 34 specimens
#   - total:   104 specimens

# After the mass campaign:
#   - month_7: 34 specimens
#   - month_8: 35 specimens
#   - month_9: 35 specimens
#   - total:   104 specimens


# No-net comparison group:

# Before the mass campaign:
#   - month_3: 10 specimens
#   - month_4: 10 specimens
#   - month_5: 12 specimens
#   - total:    32 specimens

# After the mass campaign:
#   - month_7: 11 specimens
#   - month_8: 10 specimens
#   - month_9: 11 specimens
#   - total:    32 specimens


# Total for each sampling period:

# Before the mass campaign:
#   240 dual-AI + 104 PBO + 32 no-net = 376 specimens

# After the mass campaign:
#   240 dual-AI + 104 PBO + 32 no-net = 376 specimens


# HEALTH-ZONE AND MONTH QUOTA RULES

# The health-zone-by-month allocation begins with a maximum of four specimens
# per health-zone and collection-month combination.

# When fewer than four specimens are available, all available specimens are
# retained.

# Additional specimens are then allocated until the required treatment-month
# target is reached.

# The allocation must:

#   - never exceed the number of available specimens;
#   - preserve representation across health zones;
#   - preserve representation across collection months;
#   - maintain the treatment-by-month targets;
#   - maintain 376 specimens before and 376 specimens after the campaign;
#   - minimise unnecessary differences between the before and after totals
#     within each health zone.

# Some differences may remain when the number of available specimens is
# genuinely limited in particular health-zone-by-month combinations.

# INDIVIDUAL MOSQUITO SELECTION RULES

# Individual mosquito records are selected randomly from each approved
# health-zone-by-month quota.

# A reproducible random seed is used:

#   set.seed(123)

# Household-selection rules:

#   1. When the required sample size is four specimens or fewer, specimens
#      should be selected from one household when sufficient specimens are
#      available in that household.

#   2. When the required sample size is five specimens or more, specimens
#      should preferably be selected from no more than three households.

#   3. Additional households may be used only when the preferred household
#      limit cannot provide the required number of specimens.

#   4. Households not previously selected in another month of the same
#      health zone are prioritised.

#   5. Previously used households may be reused only when necessary.

#   6. Each An. gambiae s.l must have a unique mosquito_code.

#   7. The same An. gambiae must never be selected more than once.


# PCR PLATE DESIGN

# The final design contains eight PCR plates.

# Each plate contains:
#
#   - 47 specimens collected before the mass campaign
#   - 47 specimens collected after the mass campaign
#   - 94 mosquito specimens in total

# Across eight plates, the final dataset contains:

#   - 376 specimens before the mass campaign
#   - 376 specimens after the mass campaign
#   - 752 specimens in total

# The assignment should:

#   1. Preserve the approved health-zone-by-month sampling quotas.

#   2. Preserve the treatment-by-month sampling targets.

#   3. Maintain 47 before-campaign and 47 after-campaign specimens
#      on every plate.

#   4. Keep specimens from the same household together on the same plate
#      whenever possible.

#   5. Avoid splitting specimens from the same household across different
#      plates unless this is necessary to respect plate capacity or the
#      before-and-after balance.

#   6. Concentrate specimens from the same health zone on the same plate,
#      or on the smallest possible number of plates.

#   7. When a health zone must be divided between plates, place its specimens
#      on consecutive plates whenever possible.

#   8. Organise specimens on each plate by:
#
#          Sampling period
#              -> Treatment
#                  -> Health zone
#                      -> Collection month
#                          -> Household
#                              -> Mosquito code

#   9. Avoid unnecessary mixing of health zones within the same section
#      of a plate, making the physical transfer and verification of samples
#      easier.

#  10. Ensure that each mosquito_code appears only once in the complete
#      plate assignment.

# Step 1: load the required R packages

library(tidyverse)  # Data cleaning, filtering, sampling and organisation
library(readxl)     # Import Excel files into R
library(writexl)    # Export the final datasets to Excel
library(janitor)    # Clean and standardise column names

# Step 2. Select the main entomological database

database_file <- "V:/1. PhD_Journey 2025_2026/PhD_Workspace/Thesis_Databases/kc_entomo_database/20260527_drc_entomo_database_kc.xlsx"

# Step 3. Explore the sheet names in the database

sheet_names <- excel_sheets(database_file)

sheet_names

# Step 4. Import the species sheet

species_data <- read_excel(
  path = database_file,
  sheet = "species"
) %>%
  clean_names()

nrow(species_data)  

ncol(species_data)  

# Step 5. Check the column names in the species dataset

names(species_data)

# Remove the unused x13 column

species_data <- species_data %>%
  select(-x13)

names(species_data)

ncol(species_data)

# Step 6. Check the values in the key sampling columns

# Check the mosquito species names

sort(unique(species_data$identification_taxon))


# Check the collection-month names

sort(unique(species_data$collection_month))


# Check the health-zone names

sort(unique(species_data$health_zone))

# Step 7. Explore the availability of Anopheles gambiae s.l.
# by health zone and collection month

# Define the heatmap titles and labels

plot_title <- "Anopheles gambiae s.l. specimens by health zone and month"

plot_subtitle <- "Availability across 26 health zones during 12 months of collection (Apr 2025 - March 2026)"

x_axis_title <- "Collection month"

y_axis_title <- "Health zone"

legend_title <- "Number of gambiae samples"

gambiae_availability_by_health_zone_month <- species_data %>%
  
  # Keep only Anopheles gambiae s.l.
  filter(
    identification_taxon == "An. gambiae s.l."
  ) %>%
  
  # Extract the month number from collection_month
  mutate(
    month_number = parse_number(collection_month)
  ) %>%
  
  # Count the available specimens
  count(
    health_zone,
    month_number,
    name = "available_gambiae_samples"
  ) %>%
  
  # Add missing health-zone and month combinations
  # and give them a value of zero
  complete(
    health_zone,
    month_number = 1:12,
    fill = list(
      available_gambiae_samples = 0
    )
  ) %>%
  
  # Recreate the collection-month name
  mutate(
    collection_month = paste0(
      "month_",
      month_number
    )
  )

gambiae_availability_by_health_zone_month

# Step 7.1. Validate the Anopheles gambiae s.l. availability table

gambiae_availability_validation <- 
  gambiae_availability_by_health_zone_month %>%
  summarise(
    number_of_health_zones = n_distinct(health_zone),
    number_of_months = n_distinct(month_number),
    number_of_cells = n(),
    total_available_gambiae = sum(available_gambiae_samples)
  )

gambiae_availability_validation

# Step 7.2. Calculate the total number of Anopheles gambiae s.l.
# specimens in each health zone

gambiae_total_by_health_zone <- 
  gambiae_availability_by_health_zone_month %>%
  group_by(health_zone) %>%
  summarise(
    total_available_gambiae = sum(available_gambiae_samples),
    .groups = "drop"
  ) %>%
  arrange(desc(total_available_gambiae))

gambiae_total_by_health_zone

print(
  gambiae_total_by_health_zone,
  n = 26
)

# Step 7.3. Save the health-zone order

health_zone_order <- gambiae_total_by_health_zone %>%
  pull(health_zone)

health_zone_order

# Step 7.4. Prepare the data for the heatmap

gambiae_heatmap_data <- 
  gambiae_availability_by_health_zone_month %>%
  mutate(
    
    # Give each collection month its real month and year
    collection_month_label = case_when(
      collection_month == "month_1"  ~ "Apr 25",
      collection_month == "month_2"  ~ "May 25",
      collection_month == "month_3"  ~ "Jun 25",
      collection_month == "month_4"  ~ "Jul 25",
      collection_month == "month_5"  ~ "Aug 25",
      collection_month == "month_6"  ~ "Sep 25",
      collection_month == "month_7"  ~ "Oct 25",
      collection_month == "month_8"  ~ "Nov 25",
      collection_month == "month_9"  ~ "Dec 25",
      collection_month == "month_10" ~ "Jan 26",
      collection_month == "month_11" ~ "Feb 26",
      collection_month == "month_12" ~ "Mar 26"
    ),
    
    # Keep the months in the correct chronological order
    collection_month_label = factor(
      collection_month_label,
      levels = c(
        "Apr 25",
        "May 25",
        "Jun 25",
        "Jul 25",
        "Aug 25",
        "Sep 25",
        "Oct 25",
        "Nov 25",
        "Dec 25",
        "Jan 26",
        "Feb 26",
        "Mar 26"
      )
    ),
    
    # Put health zones with the highest abundance at the top
    health_zone = factor(
      health_zone,
      levels = rev(health_zone_order)
    ),
    
    # Identify cells containing fewer than 5 specimens
    low_availability = available_gambiae_samples < 5
  )

gambiae_heatmap_data


# Step 7.5. Create the Anopheles gambiae s.l. availability heatmap

gambiae_availability_heatmap <- ggplot(
  data = gambiae_heatmap_data,
  aes(
    x = collection_month_label,
    y = health_zone,
    fill = available_gambiae_samples
  )
) +
  
  # Create one coloured cell for each health-zone and month combination
  geom_tile(
    color = "white",
    linewidth = 0.3
  ) +
  
  # Display the number of available specimens inside each cell
  geom_text(
    aes(
      label = available_gambiae_samples,
      color = low_availability
    ),
    size = 3
  ) +
  
  # Use red text when fewer than 5 specimens are available
  scale_color_manual(
    values = c(
      "FALSE" = "black",
      "TRUE" = "red"
    ),
    guide = "none"
  ) +
  
  # Define the heatmap colour scale
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkblue",
    name = legend_title
  ) +
  
  # Add a vertical line after September 2025, the campaign month
  geom_vline(
    xintercept = 6.5,
    color = "black",
    linewidth = 0.8
  ) +
  
  # Add the figure titles and axis labels
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = x_axis_title,
    y = y_axis_title
  ) +
  
  # Use a simple plot appearance
  theme_minimal() +
  
  # Adjust the title, axis labels and month labels
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5
    ),
    
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    
    axis.text.y = element_text(
      size = 9
    ),
    
    panel.grid = element_blank()
  )

gambiae_availability_heatmap


# Step 8. Define the primary sampling months and periods

months_before_mass_campaign <- c(
  "month_3",
  "month_4",
  "month_5"
)

months_after_mass_campaign <- c(
  "month_7",
  "month_8",
  "month_9"
)

selected_sampling_months <- c(
  months_before_mass_campaign,
  months_after_mass_campaign
)

months_before_mass_campaign

months_after_mass_campaign

selected_sampling_months

# Step 9. Define the intervention group for each health zone
# Health zones that received dual nets

dual_ai_net_health_zones <- c(
  "Benaleka",
  "Bilomba",
  "Bobozo",
  "Bunkonde",
  "Demba",
  "Dibaya",
  "Kananga",
  "Lubondaie",
  "Lubunga",
  "Luiza",
  "Masuika",
  "Mikalayi",
  "Muetshi",
  "Mutoto",
  "Tshikaji",
  "Tshikula"
)

# Health zones that received PBO nets

pbo_net_health_zones <- c(
  "Benatshiadi",
  "Kalomba",
  "Katende",
  "Luambo",
  "Ndekesha",
  "Ndesha",
  "Tshibala",
  "Yangala"
)


# Health zones with no net 

no_net_health_zones <- c(
  "Katoka",
  "Lukonga"
)

length(dual_ai_net_health_zones)

length(pbo_net_health_zones)

length(no_net_health_zones)

# Step 9.1. Add the intervention group to the species dataset

species_data_with_intervention <- species_data %>%
  mutate(
    intervention_group = case_when(
      health_zone %in% dual_ai_net_health_zones ~ "dual_AI_net",
      health_zone %in% pbo_net_health_zones ~ "pbo_net",
      health_zone %in% no_net_health_zones ~ "no_net",
      TRUE ~ NA_character_
    )
  )

species_data_with_intervention %>%
  distinct(
    health_zone,
    intervention_group
  ) %>%
  arrange(
    intervention_group,
    health_zone
  )

species_data_with_intervention %>%
  distinct(
    health_zone,
    intervention_group
  ) %>%
  count(
    intervention_group,
    name = "number_of_health_zones"
  )

# Step 10. Select Anopheles gambiae s.l. records
# from the primary before- and after-campaign periods
# across the six selected collection months

eligible_gambiae_data <- species_data_with_intervention %>%
  filter(
    identification_taxon == "An. gambiae s.l.",
    collection_month %in% selected_sampling_months
  ) %>%
  mutate(
    sampling_period = case_when(
      collection_month %in% months_before_mass_campaign ~
        "before_mass_campaign",
      
      collection_month %in% months_after_mass_campaign ~
        "after_mass_campaign",
      
      TRUE ~ NA_character_
    )
  )

nrow(eligible_gambiae_data)

# Step 10.1. Check the number of eligible Anopheles gambiae s.l.
# specimens available in each sampling period

eligible_gambiae_by_sampling_period <- eligible_gambiae_data %>%
  count(
    sampling_period,
    name = "available_gambiae_samples"
  )

eligible_gambiae_by_sampling_period
# Both periods have far more than the required 376 specimens, 
# so the overall availability is sufficient.

# Step 10.2. Check the number of eligible Anopheles gambiae s.l.
# specimens by intervention group and sampling period

eligible_gambiae_by_intervention_and_period <- eligible_gambiae_data %>%
  count(
    intervention_group,
    sampling_period,
    name = "available_gambiae_samples"
  ) %>%
  arrange(
    intervention_group,
    sampling_period
  )

eligible_gambiae_by_intervention_and_period

# Step 10.3. Check the number of eligible Anopheles gambiae s.l.
# specimens by intervention group and collection month

eligible_gambiae_by_intervention_and_month <- eligible_gambiae_data %>%
  count(
    intervention_group,
    sampling_period,
    collection_month,
    name = "available_gambiae_samples"
  ) %>%
  mutate(
    month_number = parse_number(collection_month)
  ) %>%
  arrange(
    intervention_group,
    sampling_period,
    month_number
  )

eligible_gambiae_by_intervention_and_month


# The total sample size was determined by the capacity of eight PCR plates, 
# with 94 specimens per plate, giving 752 specimens. 
# To ensure temporal balance, an equal number of specimens was allocated 
# to before campaign and after campaign periods, with 376 specimens per period. 
# Each plate contained 47 before campaign and 47 after campaign specimens. 
# Within each period, 240 specimens were allocated to the dual net group, 
# 104 to the PBO net group and 32 to the no nets distribution group. 
# These totals were distributed as evenly as possible across the three selected
# months, while respecting specimen availability and maintaining representation
# across health zones.
# note that this is a design and laboratory-capacity-based sample size, 
# not a sample size obtained from a statistical power calculation.

# Step 11. Define the required number of Anopheles gambiae s.l.
# specimens by intervention group and collection month

sampling_targets_by_intervention_and_month <- tribble(
  ~intervention_group, ~collection_month, ~required_gambiae_samples,
  
  # Before the mass campaign: month_3, month_4 and month_5
  
  "dual_AI_net", "month_3", 80,
  "dual_AI_net", "month_4", 80,
  "dual_AI_net", "month_5", 80,
  
  "pbo_net", "month_3", 35,
  "pbo_net", "month_4", 35,
  "pbo_net", "month_5", 34,
  
  "no_net", "month_3", 10,
  "no_net", "month_4", 10,
  "no_net", "month_5", 12,
  
  # After the mass campaign: month_7, month_8 and month_9
  
  "dual_AI_net", "month_7", 80,
  "dual_AI_net", "month_8", 80,
  "dual_AI_net", "month_9", 80,
  
  "pbo_net", "month_7", 34,
  "pbo_net", "month_8", 35,
  "pbo_net", "month_9", 35,
  
  "no_net", "month_7", 11,
  "no_net", "month_8", 10,
  "no_net", "month_9", 11
)

sampling_targets_by_intervention_and_month

# Step 11.1. Add the sampling period to the monthly sampling targets

sampling_targets_by_intervention_and_month <-
  sampling_targets_by_intervention_and_month %>%
  mutate(
    sampling_period = case_when(
      collection_month %in% months_before_mass_campaign ~
        "before_mass_campaign",
      
      collection_month %in% months_after_mass_campaign ~
        "after_mass_campaign",
      
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    intervention_group,
    sampling_period,
    collection_month,
    required_gambiae_samples
  )

sampling_targets_by_intervention_and_month

# Step 11.2. Check the required number of Anopheles gambiae s.l.
# samples in each sampling period

required_samples_by_sampling_period <-
  sampling_targets_by_intervention_and_month %>%
  group_by(sampling_period) %>%
  summarise(
    required_gambiae_samples =
      sum(required_gambiae_samples),
    .groups = "drop"
  )

required_samples_by_sampling_period

# Step 11.3. Check the required number of Anopheles gambiae s.l.
# samples in each intervention group

required_samples_by_intervention_group <-
  sampling_targets_by_intervention_and_month %>%
  group_by(intervention_group) %>%
  summarise(
    required_gambiae_samples =
      sum(required_gambiae_samples),
    .groups = "drop"
  )

required_samples_by_intervention_group


# Step 11.4. Compare the required samples with the available samples
# for each intervention group and collection month

sampling_target_availability_check <-
  sampling_targets_by_intervention_and_month %>%
  left_join(
    eligible_gambiae_by_intervention_and_month,
    by = c(
      "intervention_group",
      "sampling_period",
      "collection_month"
    )
  ) %>%
  mutate(
    remaining_gambiae_samples =
      available_gambiae_samples -
      required_gambiae_samples,
    
    target_exceeds_availability =
      required_gambiae_samples >
      available_gambiae_samples
  ) %>%
  select(
    intervention_group,
    sampling_period,
    collection_month,
    available_gambiae_samples,
    required_gambiae_samples,
    remaining_gambiae_samples,
    target_exceeds_availability
  )

sampling_target_availability_check

# Step 11.5. Check whether any intervention-month target
# exceeds the available number of specimens

sampling_target_availability_check %>%
  filter(
    target_exceeds_availability == TRUE
  )

# Step 12. Calculate the number of eligible Anopheles gambiae s.l.
# specimens available in each health zone and collection month

available_gambiae_by_health_zone_and_month <-
  eligible_gambiae_data %>%
  count(
    intervention_group,
    health_zone,
    collection_month,
    name = "available_gambiae_samples"
  ) %>%
  mutate(
    sampling_period = case_when(
      collection_month %in% months_before_mass_campaign ~
        "before_mass_campaign",
      
      collection_month %in% months_after_mass_campaign ~
        "after_mass_campaign",
      
      TRUE ~ NA_character_
    ),
    
    month_number = parse_number(collection_month)
  ) %>%
  select(
    intervention_group,
    sampling_period,
    health_zone,
    collection_month,
    month_number,
    available_gambiae_samples
  ) %>%
  arrange(
    intervention_group,
    health_zone,
    month_number
  )

available_gambiae_by_health_zone_and_month

nrow(available_gambiae_by_health_zone_and_month)

# the table has 152 rows, but the complete design should contain
# 26 health zones × 6 selected months = 156 combinations
# this means that four health-zone–month combinations had zero eligible
# An. gambiae s.l. specimens, so count() did not create rows for them.

# Step 12.1. Add missing health-zone and collection-month combinations

complete_gambiae_availability_by_health_zone_and_month <-
  available_gambiae_by_health_zone_and_month %>%
  complete(
    nesting(
      intervention_group,
      health_zone
    ),
    collection_month = selected_sampling_months,
    fill = list(
      available_gambiae_samples = 0
    )
  ) %>%
  mutate(
    sampling_period = case_when(
      collection_month %in% months_before_mass_campaign ~
        "before_mass_campaign",
      
      collection_month %in% months_after_mass_campaign ~
        "after_mass_campaign",
      
      TRUE ~ NA_character_
    ),
    
    month_number = parse_number(collection_month)
  ) %>%
  select(
    intervention_group,
    sampling_period,
    health_zone,
    collection_month,
    month_number,
    available_gambiae_samples
  ) %>%
  arrange(
    intervention_group,
    health_zone,
    month_number
  )

nrow(complete_gambiae_availability_by_health_zone_and_month)

# Step 12.2. Identify health-zone and month combinations
# with zero available Anopheles gambiae s.l. specimens

health_zone_months_with_zero_availability <-
  complete_gambiae_availability_by_health_zone_and_month %>%
  filter(
    available_gambiae_samples == 0
  )

health_zone_months_with_zero_availability

# Step 13. Create the initial sampling quota
# for each health zone and collection month

initial_sampling_quota_by_health_zone_and_month <-
  complete_gambiae_availability_by_health_zone_and_month %>%
  mutate(
    initial_gambiae_quota = pmin(
      available_gambiae_samples,
      4
    )
  )

initial_sampling_quota_by_health_zone_and_month

# Step 13.1. Check that combinations with zero availability
# also received an initial sampling quota of zero

initial_sampling_quota_by_health_zone_and_month %>%
  filter(
    available_gambiae_samples == 0
  ) %>%
  select(
    intervention_group,
    sampling_period,
    health_zone,
    collection_month,
    available_gambiae_samples,
    initial_gambiae_quota
  )


# Confirm that zero availability produces a zero initial quota

initial_sampling_quota_by_health_zone_and_month %>%
  filter(
    available_gambiae_samples == 0
  ) %>%
  select(
    health_zone,
    collection_month,
    available_gambiae_samples,
    initial_gambiae_quota
  ) %>%
  print(
    width = Inf
  )

# Step 13.2. Calculate the initial quota by intervention group
# and collection month

initial_quota_by_intervention_and_month <-
  initial_sampling_quota_by_health_zone_and_month %>%
  group_by(
    intervention_group,
    sampling_period,
    collection_month
  ) %>%
  summarise(
    initial_gambiae_quota =
      sum(initial_gambiae_quota),
    .groups = "drop"
  )

initial_quota_by_intervention_and_month

# Step 13.3. Compare the initial quotas with the required targets

initial_quota_target_check <-
  initial_quota_by_intervention_and_month %>%
  left_join(
    sampling_targets_by_intervention_and_month,
    by = c(
      "intervention_group",
      "sampling_period",
      "collection_month"
    )
  ) %>%
  mutate(
    additional_gambiae_needed =
      required_gambiae_samples -
      initial_gambiae_quota
  ) %>%
  arrange(
    sampling_period,
    intervention_group,
    collection_month
  )

initial_quota_target_check

# Display all columns in the initial quota comparison table

initial_quota_target_check %>%
  print(
    width = Inf
  )

# Step 13.4. Check whether any initial quota
# is greater than the required target

initial_quota_target_check %>%
  filter(
    additional_gambiae_needed < 0
  )

# Step 13.5. Calculate the remaining sampling capacity
# in each health zone and collection month

sampling_quota_with_remaining_capacity <-
  initial_sampling_quota_by_health_zone_and_month %>%
  mutate(
    remaining_sampling_capacity =
      available_gambiae_samples -
      initial_gambiae_quota
  )

sampling_quota_with_remaining_capacity

# Step 13.6. Check that no health-zone and month combination
# has a negative remaining sampling capacity

sampling_quota_with_remaining_capacity %>%
  filter(
    remaining_sampling_capacity < 0
  )

# Step 14. Add the required intervention-month target
# to every health-zone and month combination

sampling_quota_allocation_input <-
  sampling_quota_with_remaining_capacity %>%
  left_join(
    sampling_targets_by_intervention_and_month,
    by = c(
      "intervention_group",
      "sampling_period",
      "collection_month"
    )
  ) %>%
  select(
    intervention_group,
    sampling_period,
    health_zone,
    collection_month,
    month_number,
    available_gambiae_samples,
    initial_gambiae_quota,
    remaining_sampling_capacity,
    required_gambiae_samples
  )

sampling_quota_allocation_input

# Step 14.1. Create a function to distribute additional samples
# until each intervention-group and month target is reached

allocate_final_health_zone_quota <- function(data) {
  
  # Get the required total for this intervention group and month
  
  required_target <- unique(
    data$required_gambiae_samples
  )
  
  
  # Check that there is only one target
  
  if (length(required_target) != 1) {
    stop("More than one sampling target was found.")
  }
  
  
  # Check that enough specimens are available
  
  if (
    required_target >
    sum(data$available_gambiae_samples)
  ) {
    stop("The required target exceeds availability.")
  }
  
  
  # Start with the initial quota
  
  data <- data %>%
    arrange(health_zone) %>%
    mutate(
      final_gambiae_quota =
        as.integer(initial_gambiae_quota)
    )
  
  
  # Continue adding samples until the target is reached
  
  while (
    sum(data$final_gambiae_quota) <
    required_target
  ) {
    
    # Find health zones that can still provide specimens
    
    possible_rows <- which(
      data$final_gambiae_quota <
        data$available_gambiae_samples
    )
    
    
    # Stop if no additional specimens are available
    
    if (length(possible_rows) == 0) {
      stop("The target could not be reached.")
    }
    
    
    # Find the smallest current quota
    
    smallest_quota <- min(
      data$final_gambiae_quota[possible_rows]
    )
    
    
    # Keep health zones with the smallest quota
    
    candidate_rows <- possible_rows[
      data$final_gambiae_quota[possible_rows] ==
        smallest_quota
    ]
    
    
    # Calculate the remaining capacity
    
    remaining_capacity <-
      data$available_gambiae_samples -
      data$final_gambiae_quota
    
    
    # Select the candidate with the greatest remaining capacity
    
    selected_row <- candidate_rows[
      which.max(
        remaining_capacity[candidate_rows]
      )
    ]
    
    
    # Add one specimen to the selected health zone
    
    data$final_gambiae_quota[selected_row] <-
      data$final_gambiae_quota[selected_row] + 1
  }
  
  
  # Calculate how many specimens remain available
  
  data <- data %>%
    mutate(
      remaining_after_final_quota =
        available_gambiae_samples -
        final_gambiae_quota
    )
  
  
  return(data)
}

allocate_final_health_zone_quota

# Step 14.2. Apply the quota-allocation function
# to each intervention group and collection month

final_sampling_quota_by_health_zone_and_month <-
  sampling_quota_allocation_input %>%
  group_by(
    intervention_group,
    sampling_period,
    collection_month
  ) %>%
  group_modify(
    ~ allocate_final_health_zone_quota(.x)
  ) %>%
  ungroup() %>%
  arrange(
    intervention_group,
    sampling_period,
    month_number,
    health_zone
  )

final_sampling_quota_by_health_zone_and_month

# Step 14.3. Check that the final health-zone quotas
# reach each intervention-group and month target

final_quota_target_validation <-
  final_sampling_quota_by_health_zone_and_month %>%
  group_by(
    intervention_group,
    sampling_period,
    collection_month
  ) %>%
  summarise(
    calculated_gambiae_quota =
      sum(final_gambiae_quota),
    
    required_gambiae_samples =
      first(required_gambiae_samples),
    
    .groups = "drop"
  ) %>%
  mutate(
    quota_difference =
      calculated_gambiae_quota -
      required_gambiae_samples
  ) %>%
  arrange(
    sampling_period,
    intervention_group,
    collection_month
  )

final_quota_target_validation

# Step 14.4. Check whether any final quota
# differs from the required intervention-month target

final_quota_target_validation %>%
  filter(
    quota_difference != 0
  )

# Step 14.5. Calculate the final sampling quota
# before and after the mass campaign in each health zone

final_quota_by_health_zone_and_period <-
  final_sampling_quota_by_health_zone_and_month %>%
  group_by(
    intervention_group,
    health_zone,
    sampling_period
  ) %>%
  summarise(
    final_gambiae_quota =
      sum(final_gambiae_quota),
    .groups = "drop"
  )

final_quota_by_health_zone_and_period

# Step 14.6. Compare the before- and after-campaign quotas
# within each health zone

health_zone_before_after_quota_check <-
  final_quota_by_health_zone_and_period %>%
  pivot_wider(
    names_from = sampling_period,
    values_from = final_gambiae_quota,
    values_fill = 0
  ) %>%
  mutate(
    quota_difference =
      after_mass_campaign -
      before_mass_campaign,
    
    absolute_quota_difference =
      abs(quota_difference),
    
    health_zone_total =
      before_mass_campaign +
      after_mass_campaign
  ) %>%
  arrange(
    intervention_group,
    desc(absolute_quota_difference),
    health_zone
  )

health_zone_before_after_quota_check

# Step 15. Confirm the availability-based sampling quota

# Perfect before-and-after balance within every health zone is not required.
# Differences between periods are accepted when they reflect the number of
# Anopheles gambiae s.l. specimens available in the main database.

final_ir_sampling_quota <-
  final_sampling_quota_by_health_zone_and_month %>%
  mutate(
    
    # Rename the final quota using a clear name
    gambiae_samples_for_ir =
      as.integer(final_gambiae_quota),
    
    # Calculate the number of specimens remaining
    # after applying the IR sampling quota
    remaining_gambiae_samples =
      available_gambiae_samples -
      gambiae_samples_for_ir
  ) %>%
  select(
    intervention_group,
    sampling_period,
    health_zone,
    collection_month,
    month_number,
    available_gambiae_samples,
    gambiae_samples_for_ir,
    remaining_gambiae_samples
  ) %>%
  arrange(
    intervention_group,
    health_zone,
    month_number
  )

final_ir_sampling_quota

# Step 15.1. Validate the final IR sampling quota

final_ir_sampling_quota_validation <-
  final_ir_sampling_quota %>%
  summarise(
    number_of_health_zones =
      n_distinct(health_zone),
    
    number_of_months =
      n_distinct(collection_month),
    
    number_of_health_zone_month_combinations =
      n(),
    
    total_available_gambiae =
      sum(available_gambiae_samples),
    
    total_gambiae_samples_for_ir =
      sum(gambiae_samples_for_ir),
    
    total_remaining_gambiae =
      sum(remaining_gambiae_samples),
    
    minimum_remaining_gambiae =
      min(remaining_gambiae_samples)
  )

final_ir_sampling_quota_validation

# Step 16. Prepare the eligible individual mosquito records
# for random selection

eligible_gambiae_individual_data <-
  eligible_gambiae_data %>%
  mutate(
    
    # Extract the collection-month number
    month_number =
      parse_number(collection_month),
    
    # Create a unique household identifier
    household_id = paste(
      health_zone,
      unique_initials_of_health_area_and_village,
      house_number,
      sep = "_"
    )
  ) %>%
  arrange(
    intervention_group,
    health_zone,
    month_number,
    household_id,
    mosquito_code
  )

# Step 16.1. Validate the eligible individual mosquito records

eligible_individual_data_validation <-
  eligible_gambiae_individual_data %>%
  summarise(
    
    total_eligible_mosquitoes =
      n(),
    
    unique_mosquito_codes =
      n_distinct(mosquito_code),
    
    duplicated_mosquito_codes =
      sum(duplicated(mosquito_code)),
    
    missing_mosquito_codes =
      sum(
        is.na(mosquito_code) |
          str_squish(as.character(mosquito_code)) == ""
      ),
    
    missing_household_information =
      sum(
        is.na(health_zone) |
          is.na(unique_initials_of_health_area_and_village) |
          is.na(house_number)
      )
  )

eligible_individual_data_validation

# Step 17. Prepare the reproducible random selection

# The random seed ensures that the same mosquito records are selected
# each time the script is run using the same database and sampling quotas.

set.seed(123)

# Create an empty list for the selected mosquito records

selected_gambiae_records <- list()

# Create an empty list to track households already used
# within each health zone

used_households_by_health_zone <- list()

# Step 18. Select individual mosquitoes from as few households as possible

# IR frequency is not expected to vary between households
# within the same health zone and collection month.

# The selection will:

#   1. Prefer households containing enough mosquitoes to meet the quota.
#   2. Use one household when the complete quota can be obtained from
#      that household.
#   3. Add a second or third household only when the first household
#      cannot provide enough mosquitoes.
#   4. Use more than three households only when this is necessary to
#      reach the required health-zone and month quota.
#   5. Randomly select the individual mosquitoes from the chosen households.
#   6. Keep mosquitoes from the same household together during the later
#      PCR plate assignment whenever possible.


# Step 18.1. Create a function to select mosquitoes
# from the smallest practical number of households

select_gambiae_from_few_households <- function(
    data,
    sample_size
) {
  
  # Convert the required quota to an integer
  
  sample_size <- as.integer(sample_size)
  
  
  # Return an empty dataset when the quota is zero
  
  if (sample_size == 0) {
    return(
      data %>%
        slice(0)
    )
  }
  
  
  # Stop if the requested quota exceeds availability
  
  if (sample_size > nrow(data)) {
    stop(
      "The required sample size exceeds the number of available mosquitoes."
    )
  }
  
  # Count the mosquitoes available in every household
  
  household_availability <- data %>%
    count(
      household_id,
      name = "available_mosquitoes"
    ) %>%
    mutate(
      
      # Randomly order households with similar availability
      
      random_order = runif(n())
    ) %>%
    arrange(
      desc(available_mosquitoes),
      random_order
    ) %>%
    mutate(
      
      # Calculate the cumulative number available
      
      cumulative_available =
        cumsum(available_mosquitoes)
    )
  
  # Find the minimum number of households needed
  # to provide the required quota
  
  number_of_households_needed <- which(
    household_availability$cumulative_available >=
      sample_size
  )[1]
  
  # Keep only the households needed to reach the quota
  
  selected_households <- household_availability %>%
    slice_head(
      n = number_of_households_needed
    ) %>%
    pull(household_id)
  
  # Create the mosquito-selection pool
  
  mosquito_selection_pool <- data %>%
    filter(
      household_id %in% selected_households
    )
  
  # Randomly select the required individual mosquitoes
  
  selected_mosquitoes <- mosquito_selection_pool %>%
    slice_sample(
      n = sample_size
    ) %>%
    arrange(
      household_id,
      mosquito_code
    )
  
  # Return the selected mosquito records
  
  return(
    selected_mosquitoes
  )
}

# Step 19. Select the individual Anopheles gambiae s.l. mosquitoes
# for every health-zone and collection-month quota

# Set the random seed immediately before individual selection
# to make the selection reproducible

set.seed(123)

# Create a new empty list to store the selected mosquito records

selected_gambiae_records_by_quota <- vector(
  mode = "list",
  length = nrow(final_ir_sampling_quota)
)

# Select individual mosquitoes for each health-zone and month combination

for (quota_row in seq_len(nrow(final_ir_sampling_quota))) {
  
  # Read the information for the current quota row
  
  current_intervention_group <-
    final_ir_sampling_quota$intervention_group[quota_row]
  
  current_sampling_period <-
    final_ir_sampling_quota$sampling_period[quota_row]
  
  current_health_zone <-
    final_ir_sampling_quota$health_zone[quota_row]
  
  current_collection_month <-
    final_ir_sampling_quota$collection_month[quota_row]
  
  current_sample_size <-
    final_ir_sampling_quota$gambiae_samples_for_ir[quota_row]
  
  
  # Keep the eligible mosquitoes belonging to the current
  # intervention group, health zone and collection month
  
  current_mosquito_pool <-
    eligible_gambiae_individual_data %>%
    filter(
      intervention_group == current_intervention_group,
      sampling_period == current_sampling_period,
      health_zone == current_health_zone,
      collection_month == current_collection_month
    )
  
  
  # Select the required individual mosquitoes
  
  selected_for_current_quota <-
    select_gambiae_from_few_households(
      data = current_mosquito_pool,
      sample_size = current_sample_size
    )
  
  
  # Store the selected records in the new list
  
  selected_gambiae_records_by_quota[[quota_row]] <-
    selected_for_current_quota
}

# Step 19.1. Combine all selected mosquito records

selected_gambiae_individuals_from_species <-
  bind_rows(
    selected_gambiae_records_by_quota
  ) %>%
  arrange(
    intervention_group,
    sampling_period,
    health_zone,
    month_number,
    household_id,
    mosquito_code
  )

nrow(selected_gambiae_individuals_from_species)

# Step 19.2. Validate the selected individual mosquitoes

selected_gambiae_individuals_validation <-
  selected_gambiae_individuals_from_species %>%
  summarise(
    
    # Total number of selected mosquito records
    total_selected_mosquitoes =
      n(),
    
    # Number of unique mosquito codes
    unique_selected_mosquitoes =
      n_distinct(mosquito_code),
    
    # Number of duplicated mosquito codes
    duplicated_selected_mosquitoes =
      sum(duplicated(mosquito_code)),
    
    # Number selected before the mass campaign
    before_campaign_mosquitoes =
      sum(
        sampling_period == "before_mass_campaign"
      ),
    
    # Number selected after the mass campaign
    after_campaign_mosquitoes =
      sum(
        sampling_period == "after_mass_campaign"
      )
  )

selected_gambiae_individuals_validation

# Step 19.3. Count the selected mosquitoes
# by health zone and collection month

selected_gambiae_by_health_zone_and_month <-
  selected_gambiae_individuals_from_species %>%
  count(
    intervention_group,
    sampling_period,
    health_zone,
    collection_month,
    name = "selected_gambiae_samples"
  )

# Step 19.4. Compare the selected individuals
# with the final IR sampling quota
#Join these selected totals to the final quota
individual_selection_quota_check <-
  final_ir_sampling_quota %>%
  left_join(
    selected_gambiae_by_health_zone_and_month,
    by = c(
      "intervention_group",
      "sampling_period",
      "health_zone",
      "collection_month"
    )
  ) %>%
  mutate(
    
    # Replace a missing selected count with zero
    selected_gambiae_samples =
      replace_na(selected_gambiae_samples, 0L),
    
    # Compare the selected number with the required quota
    quota_difference =
      selected_gambiae_samples -
      gambiae_samples_for_ir
  ) %>%
  select(
    intervention_group,
    sampling_period,
    health_zone,
    collection_month,
    available_gambiae_samples,
    gambiae_samples_for_ir,
    selected_gambiae_samples,
    quota_difference
  )

# Check for differences between the required quota
# and the number of individuals selected

individual_selection_quota_check %>%
  filter(
    quota_difference != 0
  )

# Step 19.5. Count the selected mosquitoes from each household

selected_gambiae_by_household <-
  selected_gambiae_individuals_from_species %>%
  count(
    intervention_group,
    sampling_period,
    health_zone,
    collection_month,
    household_id,
    name = "selected_gambiae_samples"
  )

selected_gambiae_by_household

# Step 19.6. Summarise household use
# within each health-zone and collection-month quota

selected_household_summary <-
  selected_gambiae_by_household %>%
  group_by(
    intervention_group,
    sampling_period,
    health_zone,
    collection_month
  ) %>%
  summarise(
    
    # Total number of selected mosquitoes
    total_selected_gambiae =
      sum(selected_gambiae_samples),
    
    # Number of households used
    number_of_selected_households =
      n_distinct(household_id),
    
    # Largest number selected from one household
    largest_household_contribution =
      max(selected_gambiae_samples),
    
    .groups = "drop"
  ) %>%
  mutate(
    month_number =
      parse_number(collection_month)
  ) %>%
  arrange(
    intervention_group,
    health_zone,
    month_number
  )

selected_household_summary

# Show the health-zone and month combinations
# using the largest number of households

selected_household_summary %>%
  arrange(
    desc(number_of_selected_households),
    intervention_group,
    health_zone,
    month_number
  )

# Step 19.7. Validate the number of households used
# for the individual mosquito selection

selected_household_use_validation <-
  selected_household_summary %>%
  summarise(
    
    number_of_health_zone_months_with_samples =
      n(),
    
    minimum_households_used =
      min(number_of_selected_households),
    
    maximum_households_used =
      max(number_of_selected_households),
    
    average_households_used =
      round(
        mean(number_of_selected_households),
        2
      )
  )

selected_household_use_validation

# Step 19.8. Display the complete household-use validation

selected_household_use_validation %>%
  print(
    width = Inf
  )

# Step 20. Define the eight PCR plates

plate_names <- paste(
  "Plate",
  1:8
)

plate_names

# Step 20.1. Define the intervention-group targets
# for each plate and sampling period

plate_period_targets <- expand_grid(
  plate = plate_names,
  sampling_period = c(
    "before_mass_campaign",
    "after_mass_campaign"
  )
) %>%
  mutate(
    dual_ai_net_target = 30,
    pbo_net_target = 13,
    no_net_target = 4,
    
    total_samples_per_period =
      dual_ai_net_target +
      pbo_net_target +
      no_net_target
  )

plate_period_targets

# Step 20.2. Validate the complete PCR plate plan

plate_target_validation <-
  plate_period_targets %>%
  summarise(
    number_of_plates =
      n_distinct(plate),
    
    number_of_plate_periods =
      n(),
    
    samples_per_plate_period =
      unique(total_samples_per_period),
    
    total_planned_samples =
      sum(total_samples_per_period)
  )

plate_target_validation

# Step 20.3. Organise the plate targets
# by plate, sampling period and intervention group

plate_intervention_targets <-
  plate_period_targets %>%
  pivot_longer(
    cols = c(
      dual_ai_net_target,
      pbo_net_target,
      no_net_target
    ),
    names_to = "intervention_group",
    values_to = "required_samples"
  ) %>%
  mutate(
    intervention_group = recode(
      intervention_group,
      "dual_ai_net_target" = "dual_AI_net",
      "pbo_net_target" = "pbo_net",
      "no_net_target" = "no_net"
    )
  ) %>%
  select(
    plate,
    sampling_period,
    intervention_group,
    required_samples
  )

plate_intervention_targets

nrow(plate_intervention_targets)

# Step 20.4. Count the selected mosquitoes
# by intervention group and sampling period

selected_samples_by_intervention_and_period <-
  selected_gambiae_individuals_from_species %>%
  count(
    intervention_group,
    sampling_period,
    name = "selected_samples"
  )

selected_samples_by_intervention_and_period

# Step 20.5. Calculate the total number of samples required
# across the eight plates for each intervention group and sampling period

total_plate_targets_by_intervention_and_period <-
  plate_intervention_targets %>%
  group_by(
    intervention_group,
    sampling_period
  ) %>%
  summarise(
    required_samples =
      sum(required_samples),
    .groups = "drop"
  )

total_plate_targets_by_intervention_and_period

# Step 20.6. Compare the selected samples
# with the total PCR plate targets

selected_samples_plate_target_check <-
  selected_samples_by_intervention_and_period %>%
  left_join(
    total_plate_targets_by_intervention_and_period,
    by = c(
      "intervention_group",
      "sampling_period"
    )
  ) %>%
  mutate(
    difference =
      selected_samples -
      required_samples
  ) %>%
  arrange(
    sampling_period,
    intervention_group
  )

selected_samples_plate_target_check

# Step 21. Define the number of samples required
# from each intervention group on one plate

samples_per_plate_by_intervention <-
  plate_intervention_targets %>%
  distinct(
    intervention_group,
    required_samples
  ) %>%
  rename(
    samples_per_plate = required_samples
  )

samples_per_plate_by_intervention

# Step 21.1. Assign the selected mosquitoes to the eight PCR plates

# Mosquitoes are first organised by health zone and household.
# This concentrates specimens from the same household and health zone
# on the same plate, or on consecutive plates when splitting is necessary.

selected_gambiae_with_plate <-
  selected_gambiae_individuals_from_species %>%
  
  # Add the number required per plate
  # for each intervention group
  
  left_join(
    samples_per_plate_by_intervention,
    by = "intervention_group"
  ) %>%
  
  # Assign plates separately within every intervention group
  # and sampling period
  
  group_by(
    intervention_group,
    sampling_period
  ) %>%
  
  # Keep health zones and households together
  
  arrange(
    health_zone,
    household_id,
    month_number,
    mosquito_code,
    .by_group = TRUE
  ) %>%
  
  # Assign consecutive groups of specimens to Plates 1–8
  
  mutate(
    position_within_intervention_period =
      row_number(),
    
    plate_number =
      as.integer(
        ceiling(
          position_within_intervention_period /
            samples_per_plate
        )
      ),
    
    plate = paste(
      "Plate",
      plate_number
    )
  ) %>%
  
  ungroup()

selected_gambiae_with_plate

# Step 21.2. Check the number of mosquitoes assigned
# to each plate and sampling period

plate_period_assignment_check <-
  selected_gambiae_with_plate %>%
  count(
    plate,
    sampling_period,
    name = "assigned_samples"
  ) %>%
  mutate(
    plate_number = parse_number(plate),
    
    sampling_period = factor(
      sampling_period,
      levels = c(
        "before_mass_campaign",
        "after_mass_campaign"
      )
    )
  ) %>%
  arrange(
    plate_number,
    sampling_period
  ) %>%
  select(
    plate,
    sampling_period,
    assigned_samples
  )

plate_period_assignment_check

# Step 21.3. Check the intervention-group composition
# of each plate and sampling period

plate_intervention_assignment_check <-
  selected_gambiae_with_plate %>%
  count(
    plate,
    sampling_period,
    intervention_group,
    name = "assigned_samples"
  ) %>%
  mutate(
    plate_number = parse_number(plate),
    
    sampling_period = factor(
      sampling_period,
      levels = c(
        "before_mass_campaign",
        "after_mass_campaign"
      )
    )
  ) %>%
  arrange(
    plate_number,
    sampling_period,
    intervention_group
  ) %>%
  select(
    plate,
    sampling_period,
    intervention_group,
    assigned_samples
  )

plate_intervention_assignment_check

# Step 21.4. Check whether every plate contains the correct number
# of samples from each intervention group

plate_intervention_target_check <-
  plate_intervention_assignment_check %>%
  left_join(
    plate_intervention_targets,
    by = c(
      "plate",
      "sampling_period",
      "intervention_group"
    )
  ) %>%
  mutate(
    difference =
      assigned_samples -
      required_samples
  ) %>%
  arrange(
    parse_number(plate),
    sampling_period,
    intervention_group
  )

plate_intervention_target_check

# Step 21.5. Check the total number of mosquitoes
# assigned to each complete PCR plate

plate_total_assignment_check <-
  selected_gambiae_with_plate %>%
  count(
    plate,
    name = "total_assigned_samples"
  ) %>%
  mutate(
    plate_number = parse_number(plate)
  ) %>%
  arrange(
    plate_number
  ) %>%
  select(
    plate,
    total_assigned_samples
  )

plate_total_assignment_check

# Step 21.6. Validate the complete PCR plate assignment

complete_plate_assignment_validation <-
  selected_gambiae_with_plate %>%
  summarise(
    number_of_plates =
      n_distinct(plate),
    
    total_assigned_mosquitoes =
      n(),
    
    unique_assigned_mosquitoes =
      n_distinct(mosquito_code),
    
    duplicated_mosquitoes =
      sum(duplicated(mosquito_code)),
    
    before_campaign_mosquitoes =
      sum(
        sampling_period == "before_mass_campaign"
      ),
    
    after_campaign_mosquitoes =
      sum(
        sampling_period == "after_mass_campaign"
      )
  )

complete_plate_assignment_validation

# Step 22. Prepare the final individual mosquito list
# organised by PCR plate, sampling period and collection month

final_plate_specimen_list <-
  selected_gambiae_with_plate %>%
  
  # Create temporary variables for the required order
  
  mutate(
    
    # Before-campaign specimens must appear first
    
    sampling_period_order = case_when(
      sampling_period == "before_mass_campaign" ~ 1,
      sampling_period == "after_mass_campaign"  ~ 2
    ),
    
    # Order the collection months chronologically
    
    month_order = parse_number(
      collection_month
    ),
    
    # Order the intervention groups within each month
    
    intervention_group_order = case_when(
      intervention_group == "dual_AI_net" ~ 1,
      intervention_group == "pbo_net"     ~ 2,
      intervention_group == "no_net"      ~ 3
    )
  ) %>%
  
  # Organise specimens for easier laboratory sorting
  
  arrange(
    plate_number,
    sampling_period_order,
    month_order,
    intervention_group_order,
    health_zone,
    household_id,
    mosquito_code
  ) %>%
  
  # Add a position from 1 to 94 within every plate
  
  group_by(
    plate_number
  ) %>%
  
  mutate(
    sample_position_within_plate = row_number()
  ) %>%
  
  ungroup() %>%
  
  # Keep and organise the final variables
  
  select(
    plate = plate_number,
    sample_position_within_plate,
    sampling_period,
    collection_month,
    date,
    intervention_group,
    health_zone,
    health_area,
    village,
    unique_initials_of_health_area_and_village,
    house_number,
    household_id,
    collection_id,
    mosquito_number,
    mosquito_code,
    identification_taxon,
    abdominal_stage
  )

# Step 23. Define where the final Excel file will be saved

outputs_file <- "V:/1. PhD_Journey 2025_2026/PhD_Workspace/Thesis_Databases/kc_entomo_database/individual_samples_for_ir.xlsx"

# Step 24. Export the final individual mosquito list to Excel

write_xlsx(
  list(
    individual_plate_list = final_plate_specimen_list
  ),
  path = outputs_file
)

# Step 25. Prepare the data for comparing available
# and selected Anopheles gambiae s.l. specimens

gambiae_availability_selection_heatmap_data <-
  final_ir_sampling_quota %>%
  
  # Put available and selected sample numbers
  # into one comparison column
  
  pivot_longer(
    cols = c(
      available_gambiae_samples,
      gambiae_samples_for_ir
    ),
    names_to = "sample_status",
    values_to = "number_of_gambiae"
  ) %>%
  
  # Give the comparison groups clear names
  
  mutate(
    sample_status = recode(
      sample_status,
      "available_gambiae_samples" =
        "Available in the main database",
      "gambiae_samples_for_ir" =
        "Selected for IR analysis"
    ),
    
    # Give each collection month its real month and year
    
    collection_month_label = case_when(
      collection_month == "month_3" ~ "Jun 2025",
      collection_month == "month_4" ~ "Jul 2025",
      collection_month == "month_5" ~ "Aug 2025",
      collection_month == "month_7" ~ "Oct 2025",
      collection_month == "month_8" ~ "Nov 2025",
      collection_month == "month_9" ~ "Dec 2025"
    ),
    
    # Keep the months in chronological order
    
    collection_month_label = factor(
      collection_month_label,
      levels = c(
        "Jun 2025",
        "Jul 2025",
        "Aug 2025",
        "Oct 2025",
        "Nov 2025",
        "Dec 2025"
      )
    ),
    
    # Use the same health-zone order as the first heatmap
    
    health_zone = factor(
      health_zone,
      levels = rev(health_zone_order)
    ),
    
    # Keep availability before selection in the comparison
    
    sample_status = factor(
      sample_status,
      levels = c(
        "Available in the main database",
        "Selected for IR analysis"
      )
    )
  )

gambiae_availability_selection_heatmap_data

# Step 25.1. Check the comparison heatmap dataset

gambiae_availability_selection_heatmap_data %>%
  summarise(
    number_of_health_zones =
      n_distinct(health_zone),
    
    number_of_months =
      n_distinct(collection_month),
    
    number_of_comparison_groups =
      n_distinct(sample_status),
    
    number_of_rows =
      n()
  )

# Step 25. Prepare the data for comparing available
# and selected Anopheles gambiae s.l. specimens

gambiae_available_selected_heatmap_data <-
  final_ir_sampling_quota %>%
  
  # Place the available and selected numbers
  # in the same comparison column
  
  pivot_longer(
    cols = c(
      available_gambiae_samples,
      gambiae_samples_for_ir
    ),
    names_to = "sample_group",
    values_to = "number_of_gambiae"
  ) %>%
  
  mutate(
    
    # Give the two comparison groups clear names
    
    sample_group = recode(
      sample_group,
      
      "available_gambiae_samples" =
        "Available in the main database",
      
      "gambiae_samples_for_ir" =
        "Selected for IR analysis"
    ),
    
    
    # Give each collection month its real month and year
    
    collection_month_label = case_when(
      collection_month == "month_3" ~ "Jun 2025",
      collection_month == "month_4" ~ "Jul 2025",
      collection_month == "month_5" ~ "Aug 2025",
      collection_month == "month_7" ~ "Oct 2025",
      collection_month == "month_8" ~ "Nov 2025",
      collection_month == "month_9" ~ "Dec 2025"
    ),
    
    
    # Keep the months in chronological order
    
    collection_month_label = factor(
      collection_month_label,
      levels = c(
        "Jun 2025",
        "Jul 2025",
        "Aug 2025",
        "Oct 2025",
        "Nov 2025",
        "Dec 2025"
      )
    ),
    
    
    # Keep the same health-zone order as the first heatmap
    
    health_zone = factor(
      health_zone,
      levels = rev(health_zone_order)
    ),
    
    
    # Display availability first and selection second
    
    sample_group = factor(
      sample_group,
      levels = c(
        "Available in the main database",
        "Selected for IR analysis"
      )
    )
  )

gambiae_available_selected_heatmap_data

# Step 25.1. Validate the comparison heatmap dataset

gambiae_available_selected_heatmap_validation <-
  gambiae_available_selected_heatmap_data %>%
  summarise(
    number_of_health_zones =
      n_distinct(health_zone),
    
    number_of_months =
      n_distinct(collection_month),
    
    number_of_comparison_groups =
      n_distinct(sample_group),
    
    number_of_rows =
      n()
  )

gambiae_available_selected_heatmap_validation

# Step 25.2. Prepare the available-sample heatmap data

available_gambiae_heatmap_data <-
  gambiae_available_selected_heatmap_data %>%
  filter(
    sample_group == "Available in the main database"
  ) %>%
  mutate(
    low_availability =
      number_of_gambiae < 5
  )

# Step 25.2. Prepare the available-sample heatmap data

available_gambiae_heatmap_data <-
  gambiae_available_selected_heatmap_data %>%
  filter(
    sample_group == "Available in the main database"
  ) %>%
  mutate(
    low_availability =
      number_of_gambiae < 5
  )

# Step 25.3. Create the heatmap of available
# Anopheles gambiae s.l. specimens

available_gambiae_heatmap <-
  ggplot(
    data = available_gambiae_heatmap_data,
    aes(
      x = collection_month_label,
      y = health_zone,
      fill = number_of_gambiae
    )
  ) +
  
  # Create the heatmap cells
  
  geom_tile(
    color = "white",
    linewidth = 0.3
  ) +
  
  # Display the available number inside each cell
  
  geom_text(
    aes(
      label = number_of_gambiae,
      color = low_availability
    ),
    size = 3
  ) +
  
  # Display counts below 5 in red
  
  scale_color_manual(
    values = c(
      "FALSE" = "black",
      "TRUE" = "red"
    ),
    guide = "none"
  ) +
  
  # Define the heatmap colour scale
  
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkblue",
    name = "Available specimens"
  ) +
  
  # Separate the before- and after-campaign months
  
  geom_vline(
    xintercept = 3.5,
    color = "black",
    linewidth = 0.8
  ) +
  
  # Add titles and labels
  
  labs(
    title = "Anopheles gambiae s.l. specimens available",
    subtitle = "Availability in the main database during the six selected months",
    x = "Collection month",
    y = "Health zone"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5
    ),
    
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    
    axis.text.y = element_text(
      size = 9
    ),
    
    panel.grid = element_blank()
  )

available_gambiae_heatmap

# Step 25.4. Prepare the heatmap data for specimens
# selected for insecticide-resistance analysis

selected_gambiae_heatmap_data <-
  gambiae_available_selected_heatmap_data %>%
  filter(
    sample_group == "Selected for IR analysis"
  )

selected_gambiae_heatmap_data

# Step 25.5. Create the heatmap of Anopheles gambiae s.l.
# specimens selected for IR analysis

selected_gambiae_heatmap <-
  ggplot(
    data = selected_gambiae_heatmap_data,
    aes(
      x = collection_month_label,
      y = health_zone,
      fill = number_of_gambiae
    )
  ) +
  
  # Create one cell for each health-zone and month combination
  
  geom_tile(
    color = "white",
    linewidth = 0.3
  ) +
  
  # Display the number selected inside each cell
  
  geom_text(
    aes(
      label = number_of_gambiae
    ),
    color = "black",
    size = 3
  ) +
  
  # Define the heatmap colour scale
  
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkblue",
    name = "Selected specimens"
  ) +
  
  # Separate the before- and after-campaign periods
  
  geom_vline(
    xintercept = 3.5,
    color = "black",
    linewidth = 0.8
  ) +
  
  # Add the titles and axis labels
  
  labs(
    title = "Anopheles gambiae s.l. specimens selected for IR analysis",
    subtitle = "Final sample selected from the six primary collection months",
    x = "Collection month",
    y = "Health zone"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5
    ),
    
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    
    axis.text.y = element_text(
      size = 9
    ),
    
    panel.grid = element_blank()
  )

selected_gambiae_heatmap
