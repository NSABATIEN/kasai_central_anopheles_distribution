# 1. Load packages

# Load the packages required for data manipulation
# and preparation of the species count dataset.

source(
  "R/packages.R"
)

# 2. Read the cleaned entomological datasets

# Use the read_csv() function from the readr package
# to load the cleaned household collection-event dataset.

# Each row represents one household collection event
# during one of the 12 collection rounds.

kc_mosq <- read_csv(
  "data/clean/kc_mosquito_collections.csv",
  show_col_types = FALSE
)


# Load the cleaned individual mosquito identification dataset.

# Each row represents one individual Anopheles mosquito
# identified from the household collections.

kc_species <- read_csv(
  "data/clean/kc_species_identifications.csv",
  show_col_types = FALSE
)

# Use the glimpse() function from the dplyr package
# to confirm that both datasets were imported correctly.

kc_mosq |>
  glimpse()

kc_species |>
  glimpse()

# 3. Validate the household collection-event identifiers

# The household collection dataset uses unique_collection
# to identify each household sampling event.

# The species-identification dataset uses collection_id
# to identify the household collection event from which
# each individual mosquito originated.

# Before creating species counts, confirm that:

# - each of the 7,800 household collection events has a unique identifier; and
# - every collection_id recorded in the species dataset can be matched
#   to a household sampling event in the collection dataset.

# Use the summarise() function from the dplyr package
# and n_distinct() to compare the number of rows with
# the number of unique household collection identifiers.

collection_id_check <- kc_mosq |>
  summarise(
    n_collection_events = n(),
    n_unique_collection_ids = n_distinct(
      unique_collection
    )
  )

collection_id_check

# Check whether every sampling event represented in the species dataset
# can be matched to the household collection dataset using the
# five-variable sampling-event key.

unmatched_sampling_events <- kc_species |>
  distinct(
    collection_month,
    health_zone,
    health_area,
    village,
    house_number
  ) |>
  anti_join(
    kc_mosq |>
      distinct(
        collection_month,
        health_zone,
        health_area,
        village,
        house_number
      ),
    by = c(
      "collection_month",
      "health_zone",
      "health_area",
      "village",
      "house_number"
    )
  )

# Count unmatched sampling events.

nrow(
  unmatched_sampling_events
)

# 4. Create the household sampling-event frame

# The kc_mosq dataset contains the complete record of household sampling
# across the 12 collection rounds.

# Use the select() function from the dplyr package
# to retain the variables needed to define each sampling event.

# The collection date is taken from kc_mosq because this dataset
# represents the actual household sampling record.

sampling_frame <- kc_mosq |>
  select(
    collection_month,
    date,
    health_zone,
    health_area,
    village,
    unique_initials_of_health_area_and_village,
    house_number,
    n_anopheles_collected
  )

# Use the glimpse() function from the dplyr package
# to inspect the structure of the sampling frame.

sampling_frame |>
  glimpse()

# Confirm that the sampling frame still contains
# all 7,800 household collection events.

nrow(
  sampling_frame
)

# Use the distinct() function from the dplyr package
# to count the number of unique households represented
# across the 12 collection rounds.

sampling_frame |>
  distinct(
    health_zone,
    health_area,
    village,
    house_number
  ) |>
  nrow()

# 5. Count Anopheles taxa within each household sampling event

# The kc_species dataset contains one row for each individual mosquito.

# Use the count() function from the dplyr package
# to calculate how many mosquitoes of each taxon were identified
# within each household sampling event.

# Use the same five-variable sampling-event key validated previously:

# - collection_month
# - health_zone
# - health_area
# - village
# - house_number

# The date is not used as part of the grouping because some collection dates
# differ between the household collection and species-identification datasets.

species_event_counts <- kc_species |>
  count(
    collection_month,
    health_zone,
    health_area,
    village,
    house_number,
    identification_taxon,
    name = "species_count"
  )

# Use the glimpse() function from the dplyr package
# to inspect the species-specific household counts.

species_event_counts |>
  glimpse()

# Confirm that the species-event counts still represent
# all individual mosquitoes in the species dataset.

sum(
  species_event_counts$species_count
)

# 5B. Check the Anopheles taxa represented in the count dataset

# Use the count() function from the dplyr package
# to confirm which Anopheles taxa are represented
# in the species-event count dataset.

# sort = TRUE orders taxa from the most frequently represented
# event-taxon combinations to the least frequently represented.

species_event_counts |>
  count(
    identification_taxon,
    sort = TRUE
  )

# 6. Create the complete household-event-by-taxon count dataset

# Define the seven Anopheles taxa represented in the KC surveillance data.

# Keeping the taxa explicitly defined makes the expected structure
# of the modelling dataset clear and reproducible.

anopheles_taxa <- tibble(
  identification_taxon = c(
    "An. gambiae s.l.",
    "An. funestus gp",
    "An. paludis",
    "An. hancocki",
    "An. sp.",
    "An. moucheti",
    "An. ziemanni"
  )
)


# Use the crossing() function from the tidyr package
# to create every possible combination of:

# - 7,800 observed household sampling events; and
# - 7 Anopheles taxa.

# Because every row in sampling_frame represents a confirmed survey event,
# missing species records can subsequently be interpreted as non-detections.

count_data <- crossing(
  sampling_frame,
  anopheles_taxa
)


# Use the left_join() function from the dplyr package
# to add the observed species counts to the complete sampling structure.

count_data <- count_data |>
  left_join(
    species_event_counts,
    by = c(
      "collection_month",
      "health_zone",
      "health_area",
      "village",
      "house_number",
      "identification_taxon"
    )
  )


# Use the replace_na() function from the tidyr package
# to convert missing species counts to zero.

# These zeros represent genuine non-detections during
# confirmed household sampling events.

count_data <- count_data |>
  mutate(
    species_count = replace_na(
      species_count,
      0L
    )
  )

# Use the glimpse() function from the dplyr package
# to inspect the completed species count dataset.

count_data |>
  glimpse()

# The expected number of rows is:

# 7,800 sampling events × 7 taxa = 54,600 rows.

nrow(
  count_data
)

# Confirm that adding zero-count non-detections
# did not change the total number of observed mosquitoes.

sum(
  count_data$species_count
)

# 7. Validate species counts against total Anopheles collected

# Use the group_by() and summarise() functions from the dplyr package
# to sum the seven species-specific counts within each household
# sampling event.

# The resulting total should equal n_anopheles_collected
# recorded in the original household collection dataset.

event_count_check <- count_data |>
  group_by(
    collection_month,
    health_zone,
    health_area,
    village,
    house_number
  ) |>
  summarise(
    species_total = sum(
      species_count
    ),
    n_anopheles_collected = first(
      n_anopheles_collected
    ),
    .groups = "drop"
  ) |>
  mutate(
    count_match =
      species_total == n_anopheles_collected
  )

# Confirm that all 7,800 sampling events are represented.

nrow(
  event_count_check
)

# Count sampling events where the species-specific total
# agrees or disagrees with the original Anopheles count.

event_count_check |>
  count(
    count_match
  )

# 8. Create a unique household identifier

# The same 650 households were sampled repeatedly across
# the 12 collection rounds.

# Use the paste() function from base R
# to create a stable household identifier from:

# - health zone;
# - health area;
# - village; and
# - house number.

# This identifier does not depend on the collection date,
# which avoids the date-based identifier inconsistencies
# identified earlier.

count_data <- count_data |>
  mutate(
    household_id = paste(
      health_zone,
      health_area,
      village,
      house_number,
      sep = "_"
    )
  )

# Confirm that the dataset contains exactly
# 650 unique household identifiers.

n_distinct(
  count_data$household_id
)

# Count the number of records associated with each household.

household_record_check <- count_data |>
  count(
    household_id,
    name = "n_records"
  )

# Inspect the distribution of records per household.

count(
  household_record_check,
  n_records
)

# 9. Create a species detection variable

# The species_count variable records the number of mosquitoes
# of each taxon observed during each household sampling event.

# For detection-based species distribution modelling,
# we also need a binary response indicating whether
# a taxon was detected during that sampling event.

# Use the if_else() function from the dplyr package
# to convert species-specific counts into a binary detection variable.
#
# 1 = one or more mosquitoes detected
# 0 = no mosquitoes detected

count_data <- count_data |>
  mutate(
    species_detected = if_else(
      species_count > 0,
      1L,
      0L
    )
  )

# Count the number of zeros and ones
# in the species detection variable.

table(
  count_data$species_detected
)

# 10. Summarise species counts and detections

# Use the group_by() and summarise() functions from the dplyr package
# to describe the amount of information available for each Anopheles taxon.

# For each taxon, calculate:

# - total_count: total number of mosquitoes observed;
# - n_detections: number of household sampling events with detection;
# - n_non_detections: number of household sampling events with no detection;
# - detection_proportion: proportion of the 7,800 sampling events
#   in which the taxon was detected.

species_modelling_summary <- count_data |>
  group_by(
    identification_taxon
  ) |>
  summarise(
    total_count = sum(
      species_count
    ),
    n_detections = sum(
      species_detected
    ),
    n_non_detections = sum(
      species_detected == 0
    ),
    detection_proportion = mean(
      species_detected
    ),
    .groups = "drop"
  ) |>
  arrange(
    desc(total_count)
  )

species_modelling_summary

# 11. Assess spatial and temporal support for each Anopheles taxon

# A species may have relatively few detections but still contain useful
# information if those detections are distributed across several
# health zones, households, and collection months.

# Conversely, a species with detections concentrated in only one
# location or one period may be difficult to model reliably.

# Use the filter() function from the dplyr package
# to retain sampling events where each taxon was detected.

# Then use summarise() and n_distinct() from dplyr
# to calculate the spatial and temporal coverage of detections.

species_support_summary <- count_data |>
  filter(
    species_detected == 1
  ) |>
  group_by(
    identification_taxon
  ) |>
  summarise(
    n_detections = n(),
    n_households_detected = n_distinct(
      household_id
    ),
    n_health_zones_detected = n_distinct(
      health_zone
    ),
    n_months_detected = n_distinct(
      collection_month
    ),
    .groups = "drop"
  ) |>
  arrange(
    desc(n_detections)
  )

species_support_summary

# The modelling summary provides an overview of the abundance and
# detection frequency of each Anopheles taxon.

# Taxa with higher total counts and a larger number of detection events
# are likely to provide more information for species distribution models.

# In contrast, rare taxa may have insufficient detections to support
# robust spatial modelling.

# The detection proportion represents the proportion of the 7,800
# household sampling events in which a taxon was observed.

# Taxa with very low detection proportions may require alternative
# modelling approaches or may not contain sufficient information for
# reliable distribution modelling.
# - presence-only models ??????????????

# 11B. Display the complete spatial and temporal support summary

# Use print() from base R with width = Inf
# so that all columns, including n_months_detected,
# are displayed in the console.

print(
  species_support_summary,
  width = Inf
)

# 12. Add household coordinates to the count dataset

# The species count dataset currently identifies each household
# but does not yet contain its geographic coordinates.

# Use the read_csv() function from the readr package
# to load the cleaned household coordinate dataset created
# during the entomological data-cleaning workflow.

kc_household_coords <- read_csv(
  "data/clean/kc_household_coords.csv",
  show_col_types = FALSE
)

# Use the left_join() function from the dplyr package
# to attach latitude and longitude to every household-event-taxon record.

# The join uses the four variables that uniquely identify
# each of the 650 sampled households.

count_data <- count_data |>
  left_join(
    kc_household_coords,
    by = c(
      "health_zone",
      "health_area",
      "village",
      "house_number"
    )
  )

# Confirm that no modelling records are missing
# household latitude or longitude.

count_data |>
  summarise(
    missing_latitude = sum(
      is.na(lat_dd)
    ),
    missing_longitude = sum(
      is.na(long_dd)
    )
  )

# 13. Create collection-round and calendar-month variables

# The collection_month variable identifies the 12 surveillance rounds
# using labels such as "month_1", "month_2", ..., "month_12".

# Use the parse_number() function from the readr package
# to extract the numerical survey round.

# Keep this separate from calendar month because collection rounds
# and calendar months are not necessarily identical.

count_data <- count_data |>
  mutate(
    collection_round = parse_number(
      collection_month
    )
  )


# Use the month() and year() functions from the lubridate package
# to derive the actual calendar month and year from the collection date.

# These variables can later be used when modelling temporal seasonality.

count_data <- count_data |>
  mutate(
    calendar_month = month(
      date
    ),
    calendar_year = year(
      date
    )
  )

# Check which calendar months occur within each collection round.

# This is an important sanity check before deciding which time
# variable should be used for seasonal modelling.

count_data |>
  distinct(
    collection_round,
    calendar_month
  ) |>
  arrange(
    collection_round,
    calendar_month
  )

# 14. Order the collection-round variable

# Use the factor() function from base R
# to convert collection_month into an ordered factor.

# Explicitly defining the levels prevents R from ordering
# the rounds alphabetically, which would otherwise place
# "month_10" before "month_2".

count_data <- count_data |>
  mutate(
    collection_month = factor(
      collection_month,
      levels = paste0(
        "month_",
        1:12
      ),
      ordered = TRUE
    )
  )

# Confirm that the 12 surveillance rounds
# are stored in the intended chronological order.

levels(
  count_data$collection_month
)

# Confirm that converting collection_month to an ordered factor
# did not introduce any missing values.

sum(
  is.na(count_data$collection_month)
)

# 15. Perform final structural checks on the modelling count dataset

# Use the summarise() and n_distinct() functions from the dplyr package
# to confirm that the final dataset retains the expected study structure.

# Expected structure:

# - 54,600 rows = 7,800 sampling events × 7 taxa
# - 7,800 household sampling events
# - 650 unique households
# - 7 Anopheles taxa
# - 10,150 mosquitoes in total
# - 4,800 positive event-by-taxon detections
# - no missing household coordinates

final_count_data_check <- count_data |>
  summarise(
    n_rows = n(),
    n_sampling_events = n_distinct(
      collection_month,
      health_zone,
      health_area,
      village,
      house_number
    ),
    n_households = n_distinct(
      household_id
    ),
    n_taxa = n_distinct(
      identification_taxon
    ),
    total_mosquito_count = sum(
      species_count
    ),
    n_detections = sum(
      species_detected
    ),
    missing_latitude = sum(
      is.na(lat_dd)
    ),
    missing_longitude = sum(
      is.na(long_dd)
    )
  )

final_count_data_check

# Confirm that every household sampling event contains
# exactly one record for each of the seven Anopheles taxa.

taxa_per_sampling_event_check <- count_data |>
  count(
    collection_month,
    health_zone,
    health_area,
    village,
    house_number,
    name = "n_taxa"
  ) |>
  count(
    n_taxa
  )

taxa_per_sampling_event_check

print(
  final_count_data_check,
  width = Inf
)

# 16. Save the modelling-ready Anopheles count dataset

# Use the write_csv() function from the readr package
# to save the final household-event-by-taxon dataset.

# This dataset contains:

# - 7,800 confirmed household sampling events;
# - 650 repeatedly sampled households;
# - 7 Anopheles taxa;
# - species-specific counts and detection/non-detection;
# - survey-round and calendar-time information; and
# - household geographic coordinates.

# The resulting file will be used as the entomological input
# for the species distribution modelling workflow.

# write_csv(
#   count_data,
#   "data/clean/kc_anopheles_count_data.csv"
# )

# Confirm that the modelling dataset was successfully saved.

file.exists(
  "data/clean/kc_anopheles_count_data.csv"
)

# Reload the saved dataset to confirm that it can be read
# correctly in a new modelling script.

count_data_check <- read_csv(
  "data/clean/kc_anopheles_count_data.csv",
  show_col_types = FALSE
)

count_data_check |>
  glimpse()

nrow(
  count_data_check
)
