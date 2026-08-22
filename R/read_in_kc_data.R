###### READ AND EXPLORE THE KASAÏ-CENTRAL ENTOMOLOGICAL DATABASE ###############

# This script reads the most recent version of the Kasaï-Central mosquito
# database and prepares datasets for exploratory analyses,
# species composition analyses and spatial visualisation.

# The database contains household mosquito collection records, species
# identification data, and household location information collected across
# health zones in Kasaï-Central Province, Democratic Republic of the Congo.

# The workflow:

## Reads the most recent entomological database export.
## Imports mosquito collection, species, and location data.
## Cleans and standardises variables required for analysis.
## Summarises mosquito count across space and time.
## Examines Anopheles species composition across health zones.
## Prepares spatial datasets for mapping and exploratory analyses.
## Produces figures describing mosquito count, species composition,
##and spatial patterns across Kasaï-Central.

# 1. Load packages -------------------------------------------------------------

# Load all packages required for data import, cleaning,
# spatial processing, analysis, and visualisation.

source("R/packages.R")


# 2. Define the location of the KC entomological database ----------------------

# Define the folder containing the raw Kasaï-Central entomological database.

# The raw database is stored outside the GitHub repository because it contains
# project data that should not be version-controlled with the analysis code.

# Only this path needs to be changed when the script is run on another computer.
# e.g. KC Database location on David's computer
# dhd_data_path <- "~/pCloud Drive/R/data/va/vic/kc/raw"

# KC Database location on Vic's computer

data_folder_path <- "V:/1. PhD_Journey 2025_2026/PhD_Workspace/Thesis_Databases/kc_entomo_database"


# 3. Find all KC database export -----------------------------------------------

# The KC database is updated regularly, and previous versions are retained for
# record-keeping.

# Search the data folder for Kasaï-Central database exports.
#
# Database files follow the naming convention:
# YYYYMMDD_drc_entomo_database_kc.xlsx
#
# The regular expression identifies files matching this convention:
# ^          = start of filename
# [0-9]{8}   = eight-digit export date (YYYYMMDD)
# \.xlsx$    = Excel file extension and end of filename
#
# full.names = TRUE returns the complete file path for each matching file.

files <- list.files(
  data_folder_path,
  pattern = "^[0-9]{8}_drc_entomo_database_kc.xlsx?$",
  full.names = TRUE
)


# 4. Select the most recent database export ------------------------------------

# Select the most recent database export using the YYYYMMDD date
# recorded at the beginning of each filename.

# Use the file.info() function from base R to obtain information about
# each database file.
#
# mtime stores the file modification time.
#
# which.max() identifies the file with the most recent modification date.

latest_file <- files[
  which.max(file.info(files)$mtime)
]


# 5. Check the selected database export ----------------------------------------

# Display the selected file to confirm that the expected
# database export will be used in the analysis.

print(latest_file)


# 6. Check the Excel workbook structure ----------------------------------------

# Use the excel_sheets() function from the readxl package.

# Display the worksheet names in the selected database export.
# This provides a quick check that the expected database structure is present.

# the KC database contains the following sheets:

# - data: mosquito collection records
# - data_tracking: collection tracking information
# - species: species identification records
# - species_tracking: species processing and tracking information
# - treatment: household intervention information
# - location: household geographic information
# - time_collection: collection timing data
# - housing_details: household characteristics
# - definitions: database variable definitions

excel_sheets(latest_file)


# 7. Import the required datasets ---------------------------------------------

# Import the datasets required for the subsequent analyses:
#
# - data: household mosquito collection records;
# - species: individual mosquito identification records; and
# - location: household geographic coordinates.
# - time :  sampling effort of each collector spent collecting mosquitoes
# clean_names() standardises variable names to lower case and replaces
# spaces and special characters with underscores.

kc_mosq <- read_excel(
  latest_file,
  sheet = "data"
) |>
  clean_names()


kc_species <- read_excel(
  latest_file,
  sheet = "species"
) |>
  clean_names()


kc_location <- read_excel(
  latest_file,
  sheet = "location"
) |>
  clean_names()

kc_time <- read_excel(
  latest_file,
  sheet = "time_collection"
) |>
  clean_names() |>
  mutate(
    time_on = hms::as_hms(start_time_anopheles_collection),
    time_off = hms::as_hms(end_time_anopheles_collection),
    duration_mins = (time_off - time_on) / 60
  )


# 8. Check the imported mosquito collection dataset -----------------------------

# Inspect the structure of the household mosquito collection dataset
# to confirm that the expected variables and data types were imported.

kc_mosq |>
  glimpse()


# Confirm that the mosquito collection dataset contains the expected
# 7,800 household collection events:
# 26 health zones × 25 households × 12 collection rounds.

nrow(kc_mosq)


# 9. Convert the collection date variable --------------------------------------

# Convert the collection date to a Date object.
#
# Dates in the database are recorded in day-month-year order
# (e.g. 05052025 = 5 May 2025).

# lubridate::dmy tells R to interpret character dates in
# day-month-year order during the conversion.

kc_mosq <- kc_mosq |>
  mutate(
    date = convert_to_date(
      date,
      character_fun = lubridate::dmy
    )
  )


# Confirm that the date variable was converted successfully.

class(kc_mosq$date)

# 10. Check collection months against calendar months -------------------------

# Compare the planned collection month (month_1 to month_12)
# with the calendar month derived from the recorded collection date.
#
# format() converts each Date value to year-month format (YYYY-MM).

collection_month_check <- kc_mosq |>
  mutate(
    calendar_month = format(
      date,
      "%Y-%m"
    )
  )


# Display the calendar months represented within each collection month.

collection_month_check |>
  distinct(
    collection_month,
    calendar_month
  ) |>
  arrange(
    parse_number(collection_month),
    calendar_month
  )

# The quality-control check showed that month_1 includes collections
# from both April and May 2025.
#
# From month_2 onward, collection months followed the expected calendar
# sequence, except for six month_9 records identified as date-entry errors.
#
# The timing of month_1 and month_2 is examined in more detail after
# correcting these date-entry errors.

# 11. Identify and correct collection-date errors -----------------------------

# Identify month_9 records with collection years outside the expected
# 2025 calendar year.

month_9_date_errors <- kc_mosq |>
  filter(
    collection_month == "month_9",
    year(date) != 2025
  ) |>
  select(
    health_zone,
    health_area,
    village,
    house_number,
    collection_month,
    date
  )


# Display the identified records.

month_9_date_errors

# Correct the six month_9 date-entry errors identified in Katende village.
#
# Households H02 to H07 have the same collection day and month
# (18 December), but the recorded year increases incorrectly from
# 2026 to 2031. These dates are corrected to 18 December 2025.

kc_mosq <- kc_mosq |>
  mutate(
    date = if_else(
      health_zone == "Ndesha" &
        health_area == "Lubuwa" &
        village == "Katende" &
        collection_month == "month_9" &
        house_number %in%
          c(
            "H02",
            "H03",
            "H04",
            "H05",
            "H06",
            "H07"
          ),
      as.Date("2025-12-18"),
      date
    )
  )


# Confirm that no month_9 records remain with an incorrect collection year.

kc_mosq |>
  filter(
    collection_month == "month_9",
    year(date) != 2025
  )


# 12. Recheck collection months after correcting the date errors --------------

# Recreate the calendar-month variable using the corrected collection dates
# and confirm the calendar months represented within each collection month.

collection_month_check <- kc_mosq |>
  mutate(
    calendar_month = format(
      date,
      "%Y-%m"
    )
  )


# Display collection months in their numerical study order.

collection_month_check |>
  distinct(
    collection_month,
    calendar_month
  ) |>
  arrange(
    parse_number(collection_month),
    calendar_month
  )


# Examine the complete timing of the first two collection months.
#
# For each health zone, identify the first and last collection dates
# for month_1 and month_2.

collection_month_1_2_summary <- kc_mosq |>
  filter(
    collection_month %in%
      c(
        "month_1",
        "month_2"
      )
  ) |>
  group_by(
    health_zone,
    collection_month
  ) |>
  summarise(
    first_date = min(date),
    last_date = max(date),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = collection_month,
    values_from = c(
      first_date,
      last_date
    )
  ) |>
  arrange(
    first_date_month_1
  )


# Display the timing of month_1 and month_2 across all 26 health zones.

collection_month_1_2_summary |>
  print(n = 26)


# Summarise the province-wide timing of the first two collection months
# and confirm that month_1 was completed before month_2 began
# within every health zone.

collection_month_1_2_summary |>
  summarise(
    month_1_start = min(first_date_month_1),
    month_1_end = max(last_date_month_1),
    month_2_start = min(first_date_month_2),
    month_2_end = max(last_date_month_2),
    n_health_zones = n(),
    n_month_1_starting_in_april = sum(
      month(first_date_month_1) == 4
    ),
    n_month_1_starting_in_may = sum(
      month(first_date_month_1) == 5
    ),
    month_1_before_month_2 = all(
      last_date_month_1 < first_date_month_2
    )
  )


# Note on the timing of the first two collection months:
#
# The first collection month (month_1) was conducted between 30 April and
# 12 May 2025 across the 26 health zones. Seven health zones started
# month_1 on 30 April, while the remaining 19 health zones started in May.
#
# The second collection month (month_2) began later in May, after month_1
# had been completed within each health zone.
#
# Therefore, month_1 and month_2 can both contain May 2025 collection dates
# without indicating an error in the collection_month variable.

# 13. Create readable labels for the collection months ------------------------

# The database records collection months as month_1 to month_12.
#
# For plotting and summarising the data, assign readable calendar-month
# labels to the 12 collection months.
#
# The timing of month_1 and month_2 is documented in the quality-control
# checks above.
#
# factor() preserves the chronological order of the 12 collection months.

kc_mosq_plot <- kc_mosq |>
  mutate(
    collection_month_label = factor(
      collection_month,
      levels = paste0("month_", 1:12),
      labels = c(
        "Apr 2025",
        "May 2025",
        "Jun 2025",
        "Jul 2025",
        "Aug 2025",
        "Sep 2025",
        "Oct 2025",
        "Nov 2025",
        "Dec 2025",
        "Jan 2026",
        "Feb 2026",
        "Mar 2026"
      )
    )
  )


# Confirm that each collection month was assigned to the expected label
# and display the months in chronological order.

kc_mosq_plot |>
  count(
    collection_month,
    collection_month_label
  ) |>
  arrange(
    collection_month_label
  )

# 14. Visualise household Anopheles counts by collection month ----------------

# Visualise the distribution of household Anopheles counts across the
# 12 collection months.
#
# Each boxplot summarises the distribution of mosquito counts among the
# 650 household observations within a collection month:
#
# - the central line represents the median;
# - the box represents the interquartile range (IQR);
# - the whiskers extend to observations within 1.5 × IQR; and
# - points beyond the whiskers represent statistical outliers.
#
# These outliers are not necessarily data errors; they may represent
# households with genuinely high mosquito counts.
#
# The diamond added with stat_summary() represents the mean household count.
# Showing both the median and mean is useful because mosquito count data
# are typically right-skewed.

p_month <- kc_mosq_plot |>
  ggplot(
    aes(
      x = collection_month_label,
      y = n_anopheles_collected
    )
  ) +
  geom_boxplot() +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3
  ) +
  labs(
    title = expression(
      "Distribution of household " *
        italic("Anopheles") *
        " counts by month"
    ),
    subtitle = "Diamonds indicate mean household counts; n = 650 household observations per collection month",
    x = "Collection month",
    y = expression(
      "Number of " *
        italic("Anopheles") *
        " collected per household"
    )
  )

# Display the monthly household abundance plot.

p_month


# Calculate the mean Anopheles count per household for each collection month.
#
# Each collection month contains 650 household observations.
# The mean therefore represents the average number of Anopheles mosquitoes
# collected per household during each collection month.

mean_anopheles_by_month <- kc_mosq_plot |>
  group_by(
    collection_month,
    collection_month_label
  ) |>
  summarise(
    mean_anopheles_per_household = mean(
      n_anopheles_collected
    ),
    .groups = "drop"
  ) |>
  arrange(
    collection_month_label
  )


# Display the mean household Anopheles count for each collection month.

mean_anopheles_by_month


# Calculate the overall mean Anopheles count per household collection
# across all 12 collection months.

overall_mean_anopheles <- kc_mosq_plot |>
  summarise(
    mean_anopheles_per_household = mean(
      n_anopheles_collected
    )
  )


overall_mean_anopheles


# Inspect households with high Anopheles counts during the first
# collection month.
#
# This helps determine whether observations appearing as boxplot outliers
# correspond to plausible household mosquito collections.

kc_mosq_plot |>
  filter(
    collection_month == "month_1",
    n_anopheles_collected > 20
  ) |>
  select(
    health_zone,
    health_area,
    village,
    house_number,
    date,
    n_anopheles_collected
  ) |>
  arrange(
    desc(n_anopheles_collected)
  )


# 15. Summarise total Anopheles counts by collection month --------------------

# Calculate the total number of Anopheles mosquitoes collected across
# all 650 household observations during each collection month.
#
# This complements the household-level means and boxplots by describing
# the overall number of mosquitoes collected during each collection month.

total_anopheles_by_month <- kc_mosq_plot |>
  group_by(
    collection_month_label
  ) |>
  summarise(
    total_count = sum(
      n_anopheles_collected
    ),
    .groups = "drop"
  )


# Display the total Anopheles count for each collection month.

total_anopheles_by_month


# Visualise variation in total Anopheles counts across collection months.
#
# The line connects consecutive collection months to show temporal changes,
# while the points represent the total number of mosquitoes collected
# during each collection month.

p_month_total <- total_anopheles_by_month |>
  ggplot(
    aes(
      x = collection_month_label,
      y = total_count,
      group = 1
    )
  ) +
  geom_line() +
  geom_point() +
  labs(
    title = expression(
      "Total " *
        italic("Anopheles") *
        " counts by collection month"
    ),
    subtitle = "Each collection month included 650 household observations",
    x = "Collection month",
    y = expression(
      "Total number of " *
        italic("Anopheles") *
        " collected"
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )


# Display the total Anopheles count by collection month.

p_month_total

# A total of 10,150 Anopheles mosquitoes were collected across the
# 12 collection months.

# Mosquito collections were relatively high from April to November 2025,
# ranging from 953 to 1,154 mosquitoes per collection month.

# The highest counts were observed between June and August 2025:
#
# - June: 1,111 mosquitoes;
# - July: 1,154 mosquitoes; and
# - August: 1,150 mosquitoes.

# A marked decline in mosquito collections was observed from December 2025,
# when the total count decreased to 534 mosquitoes.

# Counts remained lower during the final collection months, with
# 529 mosquitoes in January 2026, 353 in February 2026, and
# 437 in March 2026.

# The mass ITN distribution campaign was implemented in September 2025.
# Although mosquito counts declined during the months following the campaign,
# these descriptive data alone cannot determine whether this pattern resulted
# from the intervention, seasonal variation, environmental conditions,
# or a combination of these factors.

# 16. Visualise household Anopheles counts by health zone and collection month -

# Compare the distribution of household Anopheles counts among the
# 26 health zones and across the 12 collection months.
#
# Each boxplot represents the distribution of mosquito counts among
# the 25 household observations within a health zone.
#
# Separate panels are used for each collection month to show how
# spatial differences in mosquito counts vary through time.

p_zone_month <- kc_mosq_plot |>
  ggplot(
    aes(
      x = n_anopheles_collected,
      y = health_zone
    )
  ) +
  geom_boxplot() +
  facet_wrap(
    ~collection_month_label,
    ncol = 4
  ) +
  labs(
    title = expression(
      "Distribution of household " *
        italic("Anopheles") *
        " counts by health zone and collection month"
    ),
    subtitle = "Each health zone included 25 household observations per collection month",
    x = expression(
      "Number of " *
        italic("Anopheles") *
        " collected per household"
    ),
    y = "Health zone"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    axis.text.y = element_text(
      size = 6
    )
  )


# Display the health-zone-by-month household count distributions.

p_zone_month


# Mosquito counts varied among health zones throughout the study period.

# Most household observations had relatively low mosquito counts,
# while some households recorded much higher counts.

# The distribution and variability of mosquito counts differed among
# health zones, showing clear spatial variation across the study area.

# Household mosquito counts were generally lower during the final
# collection months, from December 2025 to March 2026, compared with
# most earlier collection months.

# This pattern is consistent with the decline observed in the
# province-wide monthly mosquito counts.

# The mass ITN distribution campaign was implemented in September 2025.
# However, this descriptive figure alone cannot determine whether the
# subsequent decline in mosquito counts was related to the intervention,
# seasonal changes, environmental conditions, or a combination of these factors.

# 17. Prepare the species-level dataset ---------------------------------------

# The kc_species object contains the individual mosquito records
# imported from the "species" worksheet of the KC database.

## The kc_species object contains individual mosquito records imported
# from the "species" worksheet of the Kasaï-Central database.
#
# These data will be used to examine Anopheles species composition
# across collection months and health zones.
#
# Create readable labels for the 12 collection months using the same
# labels applied previously to the household mosquito dataset.
#
# factor() also preserves the chronological order of collection months
# in subsequent tables and figures.

kc_species_plot <- kc_species |>
  mutate(
    collection_month_label = factor(
      collection_month,
      levels = paste0(
        "month_",
        1:12
      ),
      labels = c(
        "Apr 2025",
        "May 2025",
        "Jun 2025",
        "Jul 2025",
        "Aug 2025",
        "Sep 2025",
        "Oct 2025",
        "Nov 2025",
        "Dec 2025",
        "Jan 2026",
        "Feb 2026",
        "Mar 2026"
      )
    )
  )


# Check the structure of the prepared species dataset.

kc_species_plot |>
  glimpse()


# 18. Convert the collection date in the species dataset ----------------------

# Convert the collection date to a Date object.
#
# Dates in the species worksheet are recorded in day-month-year order,
# for example 05052025 = 5 May 2025.
#
# Use the same date-conversion approach applied previously to the
# household mosquito dataset.

kc_species_plot <- kc_species_plot |>
  mutate(
    date = dmy(date)
  )

# Confirm that the date variable was converted successfully.

class(kc_species_plot$date)

# Confirm that all collection months are represented
# and assigned to the expected labels.

kc_species_plot |>
  count(
    collection_month,
    collection_month_label
  ) |>
  arrange(
    collection_month_label
  )

# 19. Check mosquito taxonomic identification ---------------------------------

# Count the number of mosquitoes assigned to each taxonomic group.
#
# sort = TRUE displays the taxa from the highest to the lowest count.
#
# This provides an initial overview of Anopheles species composition
# across Kasaï-Central.

kc_species_plot |>
  count(
    identification_taxon,
    sort = TRUE
  )

# 20. Standardise mosquito taxonomic labels ----------------------------------

# Standardise the label used for mosquitoes identified only
# to the genus level.
#
# "An. sp." indicates that the mosquito was identified as Anopheles,
# but species-level identification was not possible.
#
# This avoids treating "An. sp" and "An. sp." as different
# taxonomic groups in later analyses.

kc_species_plot <- kc_species_plot |>
  mutate(
    identification_taxon = recode(
      identification_taxon,
      "An. sp" = "An. sp."
    )
  )


# Check the taxonomic groups after standardisation.

kc_species_plot |>
  count(
    identification_taxon,
    sort = TRUE
  )


# 21. Summarise Anopheles species counts by collection month ------------------

# Count the number of mosquitoes of each taxon collected during
# each of the 12 collection months.
#
# complete() adds a count of zero when a taxon was not recorded
# during a particular collection month.
#
# This ensures that all 12 months are included when calculating
# the average monthly count for each taxon.

species_count_by_month <- kc_species_plot |>
  count(
    collection_month_label,
    identification_taxon,
    name = "species_count"
  ) |>
  complete(
    collection_month_label,
    identification_taxon,
    fill = list(
      species_count = 0
    )
  ) |>
  arrange(
    identification_taxon,
    collection_month_label
  )


# Display species counts for each collection month.

species_count_by_month


# Display the monthly count of each Anopheles taxon in a wide table.
#
# Each row represents one taxon and each column represents
# one collection month.
#
# A value of 0 indicates that the taxon was not recorded
# during that collection month.

species_count_check <- species_count_by_month |>
  pivot_wider(
    names_from = collection_month_label,
    values_from = species_count
  )


# Display the complete species-by-month count table.

View(species_count_check)

# 22. Summarise monthly counts for each Anopheles taxon -----------------------

# Calculate the total count and mean monthly count for each taxon
# across the 12 collection months.
#
# The mean represents the average number of mosquitoes of each taxon
# collected per month across the study period.

mean_species_count_by_month <- species_count_by_month |>
  group_by(
    identification_taxon
  ) |>
  summarise(
    total_count = sum(
      species_count
    ),
    mean_monthly_count = mean(
      species_count
    ),
    .groups = "drop"
  ) |>
  arrange(
    desc(total_count)
  )


# Display the total and mean monthly count for each taxon.

mean_species_count_by_month


# Visualise the distribution of monthly mosquito counts for each taxon.
#
# Each boxplot summarises the 12 monthly counts for one taxon.
#
# The diamond represents the mean monthly count.

p_species_monthly_count <- species_count_by_month |>
  ggplot(
    aes(
      x = identification_taxon,
      y = species_count
    )
  ) +
  geom_boxplot() +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3
  ) +
  labs(
    title = expression(
      "Distribution of monthly " *
        italic("Anopheles") *
        " counts by species"
    ),
    subtitle = "Diamonds indicate mean monthly counts; n = 12 collection months per species",
    x = "Species",
    y = "Monthly mosquito count"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


# Display the species monthly count distributions.

p_species_monthly_count

# Visualise monthly changes in Anopheles species counts.
#
# Each line represents one taxon and each point represents
# the total number of mosquitoes collected during one collection month.

p_species_by_month <- species_count_by_month |>
  ggplot(
    aes(
      x = collection_month_label,
      y = species_count,
      group = identification_taxon,
      colour = identification_taxon
    )
  ) +
  geom_line() +
  geom_point() +
  labs(
    title = expression(
      "Monthly variation in " *
        italic("Anopheles") *
        " species counts"
    ),
    x = "Collection month",
    y = "Mosquito count",
    colour = "Species"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      hjust = 0.5
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


# Display the monthly species count plot.

p_species_by_month


# 23. Prepare household-level Anopheles species counts ------------------------

# For each health-zone site and collection month, 25 fixed households
# were sampled.
#
# To examine household-level species counts, create one observation
# for each:
#
# - health-zone site;
# - collection month;
# - household; and
# - Anopheles taxon.
#
# The species dataset contains records only when mosquitoes were collected.
# Therefore, species that were not collected in a household must be
# represented by a count of zero.

# Keep the 7,800 household collection events from the household dataset.

household_collection_events <- kc_mosq_plot |>
  select(
    health_zone,
    health_area,
    village,
    house_number,
    collection_month,
    collection_month_label
  ) |>
  distinct()


# Confirm that the expected 7,800 household collection events are present:
# 26 health-zone sites × 25 households × 12 collection months.

nrow(
  household_collection_events
)


# Count the number of mosquitoes of each taxon recorded
# in each household during each collection month.

household_species_observed <- kc_species_plot |>
  count(
    health_zone,
    health_area,
    village,
    house_number,
    collection_month,
    collection_month_label,
    identification_taxon,
    name = "species_count"
  )

# Create all expected household-by-species combinations.
#
# Each of the 7,800 household collection events is combined with
# each of the seven Anopheles taxa.

household_species_count <- household_collection_events |>
  crossing(
    identification_taxon = unique(
      kc_species_plot$identification_taxon
    )
  ) |>
  left_join(
    household_species_observed,
    by = c(
      "health_zone",
      "health_area",
      "village",
      "house_number",
      "collection_month",
      "collection_month_label",
      "identification_taxon"
    )
  ) |>
  mutate(
    species_count = replace_na(
      species_count,
      0
    )
  )

# Confirm the expected number of household-by-species observations.

nrow(
  household_species_count
)


# Check that every taxon has exactly 25 household observations
# within each health-zone site and collection month.

household_species_check <- household_species_count |>
  group_by(
    health_zone,
    collection_month_label,
    identification_taxon
  ) |>
  summarise(
    n_households = n(),
    total_species_count = sum(
      species_count
    ),
    mean_species_count_per_household = mean(
      species_count
    ),
    .groups = "drop"
  )

View(
  household_species_check
)

# Identify any site-month-species combination that does not
# contain exactly 25 household observations.

household_species_check |>
  filter(
    n_households != 25
  )

# Visualise household An. gambiae s.l. counts by health zone and month.
#
# Each boxplot represents the distribution of An. gambiae s.l. counts
# among the 25 fixed households sampled in one health-zone site
# during one collection month.
#
# The diamond represents the mean household count.

p_gambiae_household <- household_species_count |>
  filter(
    identification_taxon == "An. gambiae s.l."
  ) |>
  ggplot(
    aes(
      x = species_count,
      y = health_zone
    )
  ) +
  geom_boxplot(
    outlier.shape = NA
  ) +
  geom_point(
    position = position_jitter(
      width = 0,
      height = 0.12
    ),
    size = 0.8,
    alpha = 0.5
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 2
  ) +
  facet_wrap(
    ~collection_month_label,
    ncol = 4
  ) +
  labs(
    title = expression(
      "Distribution of household " *
        italic("An. gambiae") *
        " s.l. counts by health zone and collection month"
    ),
    subtitle = "Each boxplot represents 25 fixed household observations",
    x = expression(
      italic("An. gambiae") *
        " s.l. count per household"
    ),
    y = "Health zone"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    axis.text.y = element_text(
      size = 6
    )
  )


# Display the household An. gambiae s.l. count distributions.

p_gambiae_household

# Each boxplot represents the distribution of An. gambiae s.l. counts
# among the 25 fixed households sampled within one health-zone site
# during one collection month.
#
# Small points represent individual household observations,
# and diamonds represent the mean household count.

# Create labels for the seven Anopheles taxa.
#
# These labels will be used by the plotting function so that
# scientific names are displayed correctly in italics.

species_plot_labels <- list(
  "An. gambiae s.l." = quote(
    italic("An. gambiae") ~ "s.l."
  ),
  "An. funestus gp" = quote(
    italic("An. funestus") ~ "gp"
  ),
  "An. paludis" = quote(
    italic("An. paludis")
  ),
  "An. hancocki" = quote(
    italic("An. hancocki")
  ),
  "An. sp." = quote(
    italic("An.") ~ "sp."
  ),
  "An. moucheti" = quote(
    italic("An. moucheti")
  ),
  "An. ziemanni" = quote(
    italic("An. ziemanni")
  )
)


# Create a function to produce the same household-level figure
# for any Anopheles taxon.
#
# species_name specifies the taxon to be displayed.
#
# For each health zone and collection month:
#
# - small points represent the 25 fixed households;
# - the boxplot shows the distribution of household counts; and
# - the diamond represents the mean household count.

plot_household_species_counts <- function(species_name) {
  # Retrieve the formatted scientific name for the selected taxon.

  species_label <- species_plot_labels[[species_name]]

  # Create the figure title and x-axis label.

  plot_title <- bquote(
    "Distribution of household " *
      .(species_label) *
      " counts by health zone and collection month"
  )

  x_axis_label <- bquote(
    .(species_label) *
      " count per household"
  )

  # Filter the household-level dataset to the selected taxon
  # and create the household count distribution plot.

  household_species_count |>
    filter(
      identification_taxon == species_name
    ) |>
    ggplot(
      aes(
        x = species_count,
        y = health_zone
      )
    ) +
    geom_boxplot(
      outlier.shape = NA
    ) +
    geom_point(
      position = position_jitter(
        width = 0,
        height = 0.12
      ),
      size = 0.8,
      alpha = 0.5
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 18,
      size = 2
    ) +
    facet_wrap(
      ~collection_month_label,
      ncol = 4
    ) +
    labs(
      title = plot_title,
      subtitle = "Each boxplot represents 25 fixed household observations",
      x = x_axis_label,
      y = "Health zone"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        hjust = 0.5
      ),
      axis.text.y = element_text(
        size = 6
      )
    )
}

# Create one household-level figure for each Anopheles taxon.

p_gambiae_household <- plot_household_species_counts(
  "An. gambiae s.l."
)

p_funestus_household <- plot_household_species_counts(
  "An. funestus gp"
)

p_paludis_household <- plot_household_species_counts(
  "An. paludis"
)

p_hancocki_household <- plot_household_species_counts(
  "An. hancocki"
)

p_moucheti_household <- plot_household_species_counts(
  "An. moucheti"
)

p_sp_household <- plot_household_species_counts(
  "An. sp."
)

p_ziemanni_household <- plot_household_species_counts(
  "An. ziemanni"
)

p_gambiae_household
p_funestus_household

# Create a simple table containing the mean household count
# for each Anopheles taxon, health-zone site, and collection month.
#
# Each mean is calculated from the 25 fixed household observations
# sampled within that site during that collection month.

mean_household_species_by_site_month <- household_species_check |>
  select(
    health_zone,
    collection_month_label,
    identification_taxon,
    total_species_count,
    mean_species_count_per_household
  ) |>
  arrange(
    identification_taxon,
    health_zone,
    collection_month_label
  )


# View the table in the RStudio Data Viewer.

View(
  mean_household_species_by_site_month
)


# 24. Summarise Anopheles species counts by health zone and collection month ---

# Summarise the total number of mosquitoes of each taxon collected
# in each health-zone site during each collection month.
#
# The household-level dataset created above contains all seven taxa
# for all 25 households, including zero counts.
#
# Therefore, every health-zone-by-month combination retains all
# seven Anopheles taxa in this summary.

kc_taxon_summary <- household_species_check |>
  transmute(
    health_zone,
    collection_month_label,
    identification_taxon,
    n = total_species_count
  )


# Calculate the proportion represented by each taxon within each
# health-zone site and collection month.
#
# prop represents the proportion of all Anopheles collected in that
# site and month that belonged to each taxon.
#
# If no Anopheles were collected in a site during a particular month,
# the proportion is recorded as NA.

kc_taxon_summary <- kc_taxon_summary |>
  group_by(
    health_zone,
    collection_month_label
  ) |>
  mutate(
    prop = if (sum(n) > 0) {
      n / sum(n)
    } else {
      NA_real_
    }
  ) |>
  ungroup()

# Check the resulting taxon summary.

kc_taxon_summary |>
  glimpse()

# Confirm the expected number of records.

nrow(
  kc_taxon_summary
)

# Identify health-zone-by-month combinations where no Anopheles
# mosquitoes were collected in the 25 households.

kc_taxon_summary |>
  group_by(
    health_zone,
    collection_month_label
  ) |>
  summarise(
    total_count = sum(n),
    .groups = "drop"
  ) |>
  filter(
    total_count == 0
  )


# 25. Define and apply the order of Anopheles taxa ----------------------------

# Calculate the total mosquito count for each taxon across all
# health zones and collection months.
#
# Taxa are ordered from the highest to the lowest total count.
# This order will be used consistently in subsequent tables and figures.

taxon_order <- kc_taxon_summary |>
  group_by(
    identification_taxon
  ) |>
  summarise(
    total_count = sum(
      n
    ),
    .groups = "drop"
  ) |>
  arrange(
    desc(total_count)
  ) |>
  pull(
    identification_taxon
  )


# Display the resulting taxon order.

taxon_order


# Apply the taxon order to the species summary dataset.

kc_taxon_summary <- kc_taxon_summary |>
  mutate(
    identification_taxon = factor(
      identification_taxon,
      levels = taxon_order
    )
  )


# Confirm the ordered taxon levels.

levels(
  kc_taxon_summary$identification_taxon
)


# 26. Summarise Anopheles species counts by health zone -----------------------

# Calculate the total number of mosquitoes of each taxon collected
# in each health zone across the 12 collection months.
#
# Because kc_taxon_summary includes zero counts, all seven taxa
# are retained for every health zone.

species_count_per_health_zone <- kc_taxon_summary |>
  group_by(
    health_zone,
    identification_taxon
  ) |>
  summarise(
    total_count = sum(
      n
    ),
    .groups = "drop"
  )


# Inspect the resulting health-zone species summary.

species_count_per_health_zone |>
  glimpse()


# Confirm the total number of Anopheles mosquitoes represented
# in the health-zone species summary.

sum(
  species_count_per_health_zone$total_count
)

# A total of 10,150 Anopheles mosquitoes were identified during the
# 12-month survey period.

# These data will be used to compare species composition among health
# zones and identify the dominant Anopheles taxa across Kasaï-Central.

# 27. Visualise Anopheles species counts by health zone -----------------------

# Visualise the total number of mosquitoes of each taxon collected
# in each health zone across the 12 collection months.
#
# Health zones are ordered according to their total mosquito count.
#
# Each coloured section of a bar represents one Anopheles taxon,
# while the complete bar represents the total Anopheles count
# recorded in that health zone.

plot_species_count_hz <- species_count_per_health_zone |>
  ggplot(
    aes(
      x = fct_reorder(
        health_zone,
        total_count,
        .fun = sum
      ),
      y = total_count,
      fill = identification_taxon
    )
  ) +
  geom_col(
    colour = "grey40",
    linewidth = 0.1
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "An. gambiae s.l." = "firebrick",
      "An. funestus gp" = "goldenrod",
      "An. paludis" = "mediumpurple",
      "An. hancocki" = "forestgreen",
      "An. sp." = "grey50",
      "An. moucheti" = "darkturquoise",
      "An. ziemanni" = "deeppink"
    ),
    labels = c(
      "An. gambiae s.l." = expression(
        italic("An. gambiae") ~ "s.l."
      ),
      "An. funestus gp" = expression(
        italic("An. funestus") ~ "gp"
      ),
      "An. paludis" = expression(
        italic("An. paludis")
      ),
      "An. hancocki" = expression(
        italic("An. hancocki")
      ),
      "An. sp." = expression(
        italic("An.") ~ "sp."
      ),
      "An. moucheti" = expression(
        italic("An. moucheti")
      ),
      "An. ziemanni" = expression(
        italic("An. ziemanni")
      )
    )
  ) +
  labs(
    title = expression(
      italic("Anopheles") *
        " counts by health zone and species"
    ),
    subtitle = expression(
      "Total " *
        italic("Anopheles") *
        " collected across 12 collection months"
    ),
    x = "Health zone",
    y = expression(
      "Total number of " *
        italic("Anopheles") *
        " collected"
    ),
    fill = "Species",
    caption = paste(
      "Counts are based on 25 fixed households sampled",
      "in one village per health zone"
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    plot.caption = element_text(
      hjust = 0.5,
      face = "italic"
    )
  )


# Display the health-zone species count figure.

plot_species_count_hz


# 28. Save the health-zone species count plot in high resolution ---------------

# Create the figure output directory if it does not already exist.
#
# recursive = TRUE creates any missing parent folders.
# showWarnings = FALSE avoids a warning when the folder already exists.

# dir.create(
#   "outputs/figures",
#   recursive = TRUE,
#   showWarnings = FALSE
# )

# Save the figure as a high-resolution PNG.
#
# A resolution of 600 dpi is suitable for posters, publications,
# presentations, and other high-quality outputs.
#
# The white background keeps the figure plain and ensures that it
# displays consistently when inserted into PowerPoint or other software.

# ggsave(
#   filename = "outputs/figures/anopheles_species_counts_by_health_zone.png",
#   plot = plot_species_count_hz,
#   width = 14,
#   height = 8.75,
#   units = "in",
#   dpi = 600,
#   bg = "white"
# )

# 29. Summarise monthly Anopheles counts by health zone ------------------------

# Calculate the total number of Anopheles mosquitoes collected
# in each health zone during each collection month.
#
# The counts of all seven taxa are summed to obtain the total
# Anopheles count for each health-zone-by-month combination.

health_zone_monthly_counts <- kc_taxon_summary |>
  group_by(
    health_zone,
    collection_month_label
  ) |>
  summarise(
    monthly_count = sum(
      n
    ),
    .groups = "drop"
  )


# Inspect the resulting monthly health-zone count dataset.

health_zone_monthly_counts |>
  glimpse()


# Confirm the expected number of health-zone-by-month records.
#
# Expected:
# 26 health zones × 12 collection months = 312 records.

nrow(
  health_zone_monthly_counts
)

# Confirm the total Anopheles count across all health zones
# and collection months.

sum(
  health_zone_monthly_counts$monthly_count
)

# Identify the health-zone-by-month combinations with the
# highest mosquito counts.

health_zone_monthly_counts |>
  arrange(
    desc(monthly_count)
  ) |>
  print(
    n = 20
  )

# Monthly mosquito counts varied  among health zones.

# The highest monthly count was recorded in Muetshi in Aug 2025,
# with 200 Anopheles mosquitoes collected.

# Other high monthly counts included:
#
# - Kananga: 187 mosquitoes in Jul 2025;
# - Muetshi: 156 mosquitoes in Sep 2025;
# - Kananga: 147 mosquitoes in Apr 2025;
# - Muetshi: 146 mosquitoes in Nov 2025; and
# - Katoka: 141 mosquitoes in Oct 2025.

# Several health zones, particularly Muetshi, Kananga, Mikalayi,
# and Lukonga, appeared repeatedly among the highest monthly counts.

# These results show spatial and temporal variation in mosquito counts
# across the study area.

# 30. Summarise Anopheles counts for each health zone -------------------------

# Summarise mosquito counts across the 12 collection months
# for each health zone.
#
# max_monthly_count represents the largest monthly count observed
# in the health zone.
#
# total_count represents the total number of Anopheles collected
# across all 12 collection months.

health_zone_count_summary <- health_zone_monthly_counts |>
  group_by(
    health_zone
  ) |>
  summarise(
    max_monthly_count = max(
      monthly_count
    ),
    total_count = sum(
      monthly_count
    ),
    .groups = "drop"
  ) |>
  arrange(
    desc(max_monthly_count)
  )


# Display all 26 health zones.

print(
  health_zone_count_summary,
  n = 26
)


# Distribution of maximum monthly mosquito counts
# across the 26 health zones.

summary(
  health_zone_count_summary$max_monthly_count
)

# Confirm that the health-zone totals still sum to
# the complete study count of 10,150 Anopheles mosquitoes.

sum(
  health_zone_count_summary$total_count
)


# 31. Group health zones by maximum monthly Anopheles count -------------------

# Use mutate() and case_when() to classify health zones according
# to the highest monthly Anopheles count observed during the study period.

# These groups are used only to organise health zones in subsequent
# visualisations and make differences in mosquito count easier to compare.

# The thresholds provide a simple descriptive separation between
# health zones with relatively high, medium, and low peak monthly counts.

# These categories are descriptive only and do not represent
# entomological or malaria-risk thresholds.

health_zone_count_summary <- health_zone_count_summary |>
  mutate(
    count_group = case_when(
      max_monthly_count >= 100 ~ "High count",
      max_monthly_count >= 30 ~ "Medium count",
      TRUE ~ "Low count"
    )
  )

# Use count() to check the number of health zones assigned
# to each count group.

health_zone_count_summary |>
  count(
    count_group
  )

# 32. Define the order of the health-zone count groups ------------------------

# Use factor() to define the order in which the count groups
# will be displayed in subsequent tables and figures.

# Health zones will be displayed in the following order:
#
# - High count;
# - Medium count; and
# - Low count.

health_zone_count_summary <- health_zone_count_summary |>
  mutate(
    count_group = factor(
      count_group,
      levels = c(
        "High count",
        "Medium count",
        "Low count"
      )
    )
  )

# Use arrange() to order health zones first by count group
# and then by their maximum monthly Anopheles count.

health_zone_count_summary |>
  arrange(
    count_group,
    desc(max_monthly_count)
  )


# 33. Add health-zone count groups to the taxon summary ------------------------

# The taxon summary dataset contains information on:
#
# - health zone;
# - collection month;
# - taxonomic identification;
# - mosquito count; and
# - species proportion.

# Use left_join() to add the health-zone count summaries
# created in the previous sections.

# left_join() retains all rows from the taxon summary dataset
# and adds the matching health-zone information using
# the health_zone variable.

# Each taxon-by-month record will therefore receive:
#
# - total_count;
# - max_monthly_count; and
# - count_group
#
# corresponding to its health zone.

kc_taxon_summary_grouped <- kc_taxon_summary |>
  left_join(
    health_zone_count_summary |>
      select(
        health_zone,
        total_count,
        max_monthly_count,
        count_group
      ),
    by = "health_zone"
  )

# Use glimpse() to confirm that the health-zone count information
# was added successfully.

kc_taxon_summary_grouped |>
  glimpse()

# Check that all three health-zone count groups are represented.

kc_taxon_summary_grouped |>
  count(
    count_group
  )

# All three health-zone count groups are represented.

# The complete taxon summary contains:
#
# - 672 records from 8 High-count health zones;
# - 756 records from 9 Medium-count health zones; and
# - 756 records from 9 Low-count health zones.
#
# Each health zone contributes 84 records:
# 12 collection months × 7 Anopheles taxa.

# 34. Order health zones within the count groups -------------------------------

# Use arrange() to order health zones first by count group
# and then from the highest to the lowest maximum monthly count.

# Use pull() to extract the ordered health-zone names
# as a character vector.

health_zone_order <- health_zone_count_summary |>
  arrange(
    count_group,
    desc(max_monthly_count)
  ) |>
  pull(
    health_zone
  )

# Display the resulting health-zone order.

health_zone_order


# Convert health_zone to a factor so that this order is preserved
# in subsequent figures.

kc_taxon_summary_grouped <- kc_taxon_summary_grouped |>
  mutate(
    health_zone = factor(
      health_zone,
      levels = health_zone_order
    )
  )

# Confirm the ordered health-zone levels.

levels(
  kc_taxon_summary_grouped$health_zone
)

# 35. Create shorter month labels for the health-zone figure -------------------

# Create shorter labels for the collection months
# to improve readability in the multi-panel figure.

# The original collection_month_label variable is not changed;
# these labels are used only when displaying the x-axis.

month_labels <- c(
  "Apr 2025" = "Apr 25",
  "May 2025" = "May 25",
  "Jun 2025" = "Jun 25",
  "Jul 2025" = "Jul 25",
  "Aug 2025" = "Aug 25",
  "Sep 2025" = "Sep 25",
  "Oct 2025" = "Oct 25",
  "Nov 2025" = "Nov 25",
  "Dec 2025" = "Dec 25",
  "Jan 2026" = "Jan 26",
  "Feb 2026" = "Feb 26",
  "Mar 2026" = "Mar 26"
)

# Define positions between consecutive collection months.

# Use seq() to create vertical separator positions
# between the 12 collection months.

separator_positions <- seq(
  1.5,
  11.5,
  by = 1
)

# Check the month labels.

month_labels

# Check the separator positions.

separator_positions


# 36. Define colours for Anopheles species -------------------------------------

# Create a named vector containing the colour assigned
# to each Anopheles taxon.

# The same colours will be reused in subsequent figures
# to keep species representation consistent throughout the analysis.

species_colors <- c(
  "An. gambiae s.l." = "firebrick",
  "An. funestus gp" = "goldenrod",
  "An. paludis" = "mediumpurple",
  "An. hancocki" = "forestgreen",
  "An. sp." = "grey50",
  "An. moucheti" = "darkturquoise",
  "An. ziemanni" = "deeppink"
)

# Check the species colour vector.

species_colors

# Prepare a complete species legend

# 37. Plot monthly Anopheles species counts for High-count health zones --------

# Retain only health zones classified in the High-count group.

# Display monthly mosquito counts as stacked bars,
# with each colour representing one Anopheles taxon.

# Vertical lines separate consecutive collection months.

# Each High-count health zone is displayed in a separate panel.
#
# The eight High-count health zones are shown in one horizontal row.

p_high_count <- kc_taxon_summary_grouped |>
  filter(
    count_group == "High count"
  ) |>
  ggplot(
    aes(
      x = collection_month_label,
      y = n,
      fill = identification_taxon
    )
  ) +
  geom_vline(
    xintercept = separator_positions,
    colour = "grey70",
    linewidth = 0.35
  ) +
  geom_col(
    position = "stack",
    width = 0.75
  ) +
  facet_grid(
    rows = vars(count_group),
    cols = vars(health_zone),
    scales = "free_y",
    space = "free_x",
    switch = "y"
  ) +
  scale_y_continuous(
    limits = c(
      0,
      200
    ),
    breaks = seq(
      0,
      200,
      by = 40
    )
  ) +
  scale_x_discrete(
    labels = month_labels,
    drop = FALSE
  ) +
  scale_fill_manual(
    limits = taxon_order,
    breaks = taxon_order,
    drop = FALSE,
    values = species_colors,
    labels = c(
      "An. gambiae s.l." = expression(
        italic("An. gambiae") ~ "s.l."
      ),
      "An. funestus gp" = expression(
        italic("An. funestus") ~ "gp"
      ),
      "An. paludis" = expression(
        italic("An. paludis")
      ),
      "An. hancocki" = expression(
        italic("An. hancocki")
      ),
      "An. sp." = expression(
        italic("An.") ~ "sp."
      ),
      "An. moucheti" = expression(
        italic("An. moucheti")
      ),
      "An. ziemanni" = expression(
        italic("An. ziemanni")
      )
    )
  ) +
  labs(
    x = NULL,
    y = "Mosquito count",
    fill = expression(
      italic("Anopheles") ~ "species"
    )
  ) +
  theme_bw() +
  theme(
    # Format the health-zone and count-group labels.

    strip.background = element_rect(
      fill = "grey90",
      colour = "black"
    ),

    strip.text.x = element_text(
      face = "bold",
      size = 8
    ),

    strip.text.y.left = element_text(
      face = "bold",
      size = 11,
      angle = 90
    ),

    # Hide collection-month labels in the High-count row.
    #
    # Month labels will be displayed only in the bottom row
    # of the final combined figure.

    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),

    # Simplify the panel grid.

    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),

    panel.grid.major.y = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),

    panel.grid.minor.y = element_blank(),

    axis.title.x = element_blank(),

    axis.text.y = element_text(
      size = 8
    ),

    axis.title.y = element_text(
      size = 11
    ),

    # The species legend will be displayed beside
    # the Medium-count row in the final combined figure.

    legend.position = "none",

    # Keep health-zone panels close together.

    panel.spacing.x = grid::unit(
      0.05,
      "lines"
    )
  )


# Display the High-count health-zone figure.

p_high_count


# 38. Plot monthly Anopheles species counts for Medium-count health zones ------

# Retain only health zones classified in the Medium-count group.

# Display monthly mosquito counts as stacked bars,
# with each colour representing one Anopheles taxon.

# Vertical lines separate consecutive collection months.

# Each Medium-count health zone is displayed in a separate panel.
#
# The nine Medium-count health zones are shown in one horizontal row.

p_medium_count <- kc_taxon_summary_grouped |>
  filter(
    count_group == "Medium count"
  ) |>
  ggplot(
    aes(
      x = collection_month_label,
      y = n,
      fill = identification_taxon
    )
  ) +
  geom_vline(
    xintercept = separator_positions,
    colour = "grey70",
    linewidth = 0.35
  ) +
  geom_col(
    position = "stack",
    width = 0.75
  ) +
  facet_grid(
    rows = vars(count_group),
    cols = vars(health_zone),
    scales = "free_y",
    space = "free_x",
    switch = "y"
  ) +
  scale_y_continuous(
    limits = c(
      0,
      100
    ),
    breaks = seq(
      0,
      100,
      by = 20
    )
  ) +
  scale_x_discrete(
    labels = month_labels,
    drop = FALSE
  ) +
  scale_fill_manual(
    limits = taxon_order,
    breaks = taxon_order,
    drop = FALSE,
    values = species_colors,
    labels = c(
      "An. gambiae s.l." = expression(
        italic("An. gambiae") ~ "s.l."
      ),
      "An. funestus gp" = expression(
        italic("An. funestus") ~ "gp"
      ),
      "An. paludis" = expression(
        italic("An. paludis")
      ),
      "An. hancocki" = expression(
        italic("An. hancocki")
      ),
      "An. sp." = expression(
        italic("An.") ~ "sp."
      ),
      "An. moucheti" = expression(
        italic("An. moucheti")
      ),
      "An. ziemanni" = expression(
        italic("An. ziemanni")
      )
    )
  ) +
  labs(
    x = NULL,
    y = "Mosquito count",
    fill = expression(
      italic("Anopheles") ~ "species"
    )
  ) +
  theme_bw() +
  theme(
    # Format the health-zone and count-group labels.

    strip.background = element_rect(
      fill = "grey90",
      colour = "black"
    ),

    strip.text.x = element_text(
      face = "bold",
      size = 8
    ),

    strip.text.y.left = element_text(
      face = "bold",
      size = 11,
      angle = 90
    ),

    # Hide collection-month labels in the Medium-count row.
    #
    # Month labels will be displayed only in the bottom row
    # of the final combined figure.

    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),

    # Simplify the panel grid.

    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),

    panel.grid.major.y = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),

    panel.grid.minor.y = element_blank(),

    axis.title.x = element_blank(),

    axis.text.y = element_text(
      size = 8
    ),

    axis.title.y = element_text(
      size = 11
    ),

    # Display the species legend beside
    # the Medium-count row.

    legend.position = "right",
    legend.justification = "center",
    legend.background = element_blank(),

    legend.title = element_text(
      size = 13
    ),

    legend.text = element_text(
      size = 10
    ),

    # Keep health-zone panels close together.

    panel.spacing.x = grid::unit(
      0.05,
      "lines"
    )
  )


# Display the Medium-count health-zone figure.

p_medium_count


# 39. Plot monthly Anopheles species counts for Low-count health zones ---------

# Retain only health zones classified in the Low-count group.

# Display monthly mosquito counts as stacked bars,
# with each colour representing one Anopheles taxon.

# Vertical lines separate consecutive collection months.

# Each Low-count health zone is displayed in a separate panel.
#
# The nine Low-count health zones are shown in one horizontal row.

p_low_count <- kc_taxon_summary_grouped |>
  filter(
    count_group == "Low count"
  ) |>
  ggplot(
    aes(
      x = collection_month_label,
      y = n,
      fill = identification_taxon
    )
  ) +
  geom_vline(
    xintercept = separator_positions,
    colour = "grey70",
    linewidth = 0.35
  ) +
  geom_col(
    position = "stack",
    width = 0.75
  ) +
  facet_grid(
    rows = vars(count_group),
    cols = vars(health_zone),
    scales = "free_y",
    space = "free_x",
    switch = "y"
  ) +
  scale_y_continuous(
    limits = c(
      0,
      30
    ),
    breaks = seq(
      0,
      30,
      by = 10
    )
  ) +
  scale_x_discrete(
    labels = month_labels,
    drop = FALSE
  ) +
  scale_fill_manual(
    limits = taxon_order,
    breaks = taxon_order,
    drop = FALSE,
    values = species_colors,
    labels = c(
      "An. gambiae s.l." = expression(
        italic("An. gambiae") ~ "s.l."
      ),
      "An. funestus gp" = expression(
        italic("An. funestus") ~ "gp"
      ),
      "An. paludis" = expression(
        italic("An. paludis")
      ),
      "An. hancocki" = expression(
        italic("An. hancocki")
      ),
      "An. sp." = expression(
        italic("An.") ~ "sp."
      ),
      "An. moucheti" = expression(
        italic("An. moucheti")
      ),
      "An. ziemanni" = expression(
        italic("An. ziemanni")
      )
    )
  ) +
  labs(
    x = NULL,
    y = "Mosquito count"
  ) +
  theme_bw() +
  theme(
    # Format the health-zone and count-group labels.

    strip.background = element_rect(
      fill = "grey90",
      colour = "black"
    ),

    strip.text.x = element_text(
      face = "bold",
      size = 8
    ),

    strip.text.y.left = element_text(
      face = "bold",
      size = 11,
      angle = 90
    ),

    # Display collection-month labels in the bottom row
    # of the final combined figure.

    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 6
    ),

    axis.ticks.x = element_blank(),

    # Simplify the panel grid.

    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),

    panel.grid.major.y = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),

    panel.grid.minor.y = element_blank(),

    axis.title.x = element_blank(),

    axis.text.y = element_text(
      size = 8
    ),

    axis.title.y = element_text(
      size = 11
    ),

    # The species legend is displayed only
    # beside the Medium-count row.

    legend.position = "none",

    # Keep health-zone panels close together.

    panel.spacing.x = grid::unit(
      0.05,
      "lines"
    )
  )


# Display the Low-count health-zone figure.

p_low_count

# 40. Combine the High-, Medium-, and Low-count plots --------------------------

# Use the patchwork package to arrange the three count-group plots
# vertically into one combined figure.

# Use plot_annotation() to add one common title, subtitle,
# and sampling-design note to the final figure.

final_count_plot <-
  (p_high_count /
    p_medium_count /
    p_low_count) +
  plot_annotation(
    title = expression(
      "Spatial and temporal variation in " *
        italic("Anopheles") *
        " species counts across health zones"
    ),
    subtitle = paste(
      "Health zones grouped by maximum monthly count among sampled households:",
      "High ≥100; Medium 30–99; Low <30"
    ),
    caption = paste0(
      "Counts are based on 25 fixed households sampled in one village per health zone\n",
      "during each collection month; they do not represent complete health-zone coverage."
    ),
    theme = theme(
      plot.title = element_text(
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        hjust = 0.5
      ),
      plot.caption.position = "plot",
      plot.caption = element_text(
        hjust = 0.5,
        size = 10,
        face = "italic",
        margin = margin(
          t = 15
        )
      )
    )
  )


# Display the final combined figure.

final_count_plot

# 41. Save the final spatial-temporal Anopheles count figure -------------------

# Create the figure output directory if it does not already exist.

dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)


# Save the combined figure as a high-resolution PNG.
#
# The white background keeps the figure clean and suitable
# for posters, presentations, and reports.

ggsave(
  filename = "outputs/figures/spatial_temporal_anopheles_species_counts.png",
  plot = final_count_plot,
  width = 20,
  height = 11,
  units = "in",
  dpi = 600,
  bg = "white"
)


# Save a vector PDF version of the same figure.
#
# The PDF remains sharp when enlarged and is useful
# for publication-quality printing.

ggsave(
  filename = "outputs/figures/spatial_temporal_anopheles_species_counts.pdf",
  plot = final_count_plot,
  width = 20,
  height = 11,
  units = "in",
  bg = "white"
)


# Confirm that both figure files were created successfully.

file.exists(
  "outputs/figures/spatial_temporal_anopheles_species_counts.png"
)

file.exists(
  "outputs/figures/spatial_temporal_anopheles_species_counts.pdf"
)


# 42. Visualise monthly Anopheles species counts in selected health zones ------

# Select six health zones representing contrasting patterns
# in mosquito count and species composition across the study period.

selected_health_zones <- c(
  "Muetshi",
  "Mutoto",
  "Kananga",
  "Mikalayi",
  "Bobozo",
  "Tshikula"
)


# Retain only the six selected health zones.

# Convert health_zone to a factor to preserve the intended
# order of the health zones in the figure.

# Display monthly mosquito counts as stacked bars,
# with each colour representing one Anopheles taxon.

# Each selected health zone is displayed in a separate panel.

p_selected_health_zones <- kc_taxon_summary |>
  filter(
    health_zone %in% selected_health_zones
  ) |>
  mutate(
    health_zone = factor(
      health_zone,
      levels = selected_health_zones
    )
  ) |>
  ggplot(
    aes(
      x = collection_month_label,
      y = n,
      fill = identification_taxon
    )
  ) +
  geom_vline(
    xintercept = separator_positions,
    colour = "grey70",
    linewidth = 0.35
  ) +
  geom_col(
    position = "stack",
    width = 0.75
  ) +
  facet_wrap(
    ~health_zone,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_x_discrete(
    labels = month_labels,
    drop = FALSE
  ) +
  scale_fill_manual(
    limits = taxon_order,
    breaks = taxon_order,
    drop = FALSE,
    values = species_colors,
    labels = c(
      "An. gambiae s.l." = expression(
        italic("An. gambiae") ~ "s.l."
      ),
      "An. funestus gp" = expression(
        italic("An. funestus") ~ "gp"
      ),
      "An. paludis" = expression(
        italic("An. paludis")
      ),
      "An. hancocki" = expression(
        italic("An. hancocki")
      ),
      "An. sp." = expression(
        italic("An.") ~ "sp."
      ),
      "An. moucheti" = expression(
        italic("An. moucheti")
      ),
      "An. ziemanni" = expression(
        italic("An. ziemanni")
      )
    )
  ) +
  labs(
    title = expression(
      "Monthly variation in " *
        italic("Anopheles") *
        " species counts across selected health zones"
    ),
    subtitle = "Selected health zones illustrate contrasting patterns in mosquito count and species composition",
    x = "Collection month",
    y = "Mosquito count",
    fill = expression(
      italic("Anopheles") ~ "species"
    ),
    caption = paste0(
      "Counts are based on 25 fixed households sampled in one village per health zone\n",
      "during each collection month; they do not represent complete health-zone coverage."
    )
  ) +
  theme_bw() +
  theme(
    # Centre the title and subtitle.

    plot.title = element_text(
      hjust = 0.5
    ),

    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10
    ),

    # Format the health-zone panel labels.

    strip.background = element_rect(
      fill = "grey90",
      colour = "black"
    ),

    strip.text = element_text(
      face = "bold",
      size = 10
    ),

    # Display collection-month labels vertically
    # to improve readability.

    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 7
    ),

    # Simplify the panel grid.

    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),

    panel.grid.major.y = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),

    panel.grid.minor.y = element_blank(),

    # Format the species legend.

    legend.position = "right",

    legend.title = element_text(
      size = 12
    ),

    legend.text = element_text(
      size = 10
    ),

    # Centre the sampling-design note below the figure.

    plot.caption.position = "plot",

    plot.caption = element_text(
      hjust = 0.5,
      size = 9,
      face = "italic",
      margin = margin(
        t = 12
      )
    )
  )


# Display the selected health-zone figure.

p_selected_health_zones


# 43. Save the selected health-zone species count figure -----------------------

# Save the selected health-zone figure as a high-resolution PNG.
#
# The white background keeps the figure clean and suitable
# for posters, presentations, reports, and manuscripts.

ggsave(
  filename = "outputs/figures/selected_health_zones_anopheles_species_counts.png",
  plot = p_selected_health_zones,
  width = 14,
  height = 10,
  units = "in",
  dpi = 600,
  bg = "white"
)


# 44. Visualise monthly Anopheles species counts in selected health zones -----

# Select one representative health zone for each contrasting
# vector-composition pattern highlighted in the PAMCA poster:
#
# - Mutoto: An. gambiae s.l. dominance;
# - Kananga: An. funestus gp dominance; and
# - Bobozo: secondary vector species dominance.

selected_health_zones <- c(
  "Mutoto",
  "Kananga",
  "Bobozo"
)


# Define the taxon order used specifically for this PAMCA poster figure.
#
# An. sp. is excluded from the poster visualisation because these
# mosquitoes were not identified to a named taxon.
#
# This does not modify the original taxonomic dataset or the
# other analyses conducted in this script.

poster_taxon_order <- taxon_order[
  taxon_order != "An. sp."
]


# Keep the same species colours used throughout the analysis,
# excluding the colour assigned to An. sp.

poster_species_colors <- species_colors[
  names(species_colors) != "An. sp."
]


# Retain the three representative health zones and exclude An. sp.
#
# Convert health_zone to a factor to preserve the intended
# left-to-right order:
#
# Mutoto | Kananga | Bobozo
#
# Monthly species counts are displayed as stacked bars.

p_selected_health_zones <- kc_taxon_summary |>
  filter(
    health_zone %in% selected_health_zones,
    identification_taxon != "An. sp."
  ) |>
  mutate(
    health_zone = factor(
      health_zone,
      levels = selected_health_zones
    )
  ) |>
  ggplot(
    aes(
      x = collection_month_label,
      y = n,
      fill = identification_taxon
    )
  ) +

  # Add vertical separators between consecutive collection months.

  geom_vline(
    xintercept = separator_positions,
    colour = "grey70",
    linewidth = 0.35
  ) +

  # Display monthly species counts as stacked bars.

  geom_col(
    position = "stack",
    width = 0.75
  ) +

  # Display the three representative health zones horizontally.
  #
  # Free y-axis scales allow the species-composition patterns
  # to remain visible despite large differences in mosquito count.

  facet_wrap(
    ~health_zone,
    ncol = 3,
    scales = "free_y"
  ) +

  # Use shorter collection-month labels.

  scale_x_discrete(
    labels = month_labels,
    drop = FALSE
  ) +

  # Apply consistent species colours and italicised taxon names.

  scale_fill_manual(
    limits = poster_taxon_order,
    breaks = poster_taxon_order,
    drop = FALSE,
    values = poster_species_colors,
    labels = c(
      "An. gambiae s.l." = expression(
        italic("An. gambiae") ~ "s.l."
      ),
      "An. funestus gp" = expression(
        italic("An. funestus") ~ "gp"
      ),
      "An. paludis" = expression(
        italic("An. paludis")
      ),
      "An. hancocki" = expression(
        italic("An. hancocki")
      ),
      "An. moucheti" = expression(
        italic("An. moucheti")
      ),
      "An. ziemanni" = expression(
        italic("An. ziemanni")
      )
    )
  ) +

  labs(
    title = expression(
      "Contrasting monthly " *
        italic("Anopheles") *
        " species patterns"
    ),

    subtitle = expression(
      "Mutoto: " *
        italic("An. gambiae") *
        " s.l. dominance;   Kananga: " *
        italic("An. funestus") *
        " gp dominance;   Bobozo: secondary vector species dominance"
    ),

    x = "Collection month",
    y = "Mosquito count",

    fill = expression(
      italic("Anopheles") ~ "species"
    ),

    caption = paste0(
      "Counts are based on 25 fixed households sampled in one village per health zone ",
      "during each collection month."
    )
  ) +

  theme_bw() +

  theme(
    # Centre the title and subtitle.

    plot.title = element_text(
      hjust = 0.5,
      size = 15
    ),

    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10
    ),

    # Format the health-zone panel labels.

    strip.background = element_rect(
      fill = "grey90",
      colour = "black"
    ),

    strip.text = element_text(
      face = "bold",
      size = 11
    ),

    # Display collection-month labels vertically
    # so that all 12 months remain readable.

    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 7
    ),

    # Simplify the panel grid.

    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),

    panel.grid.major.y = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),

    panel.grid.minor.y = element_blank(),

    # Place the species legend below the three panels
    # to preserve horizontal space for the bar charts.

    legend.position = "bottom",

    legend.title = element_text(
      size = 11
    ),

    legend.text = element_text(
      size = 9
    ),

    # Format the sampling-design note.

    plot.caption.position = "plot",

    plot.caption = element_text(
      hjust = 0.5,
      size = 8,
      face = "italic",
      margin = margin(
        t = 8
      )
    )
  ) +

  # Arrange the six identified taxa over two rows
  # to keep the poster legend compact.

  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )


# Display the selected health-zone figure.

p_selected_health_zones


# 44. Save the selected health-zone species count figure -----------------------

# Save the three-site PAMCA poster figure as a high-resolution PNG.
#
# The wide format is suitable for displaying Mutoto,
# Kananga, and Bobozo horizontally.

ggsave(
  filename = "outputs/figures/pamca_selected_health_zones_species_counts.png",
  plot = p_selected_health_zones,
  width = 16,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)


# 45. Inspect the household location dataset -----------------------------------

# The kc_location dataset contains the geographic coordinates
# of the 650 fixed households sampled across Kasaï-Central.

# These coordinates will be used to:
#
# - visualise the spatial distribution of sampled households;
# - link household locations to mosquito count data;
# - validate household locations against health-zone boundaries; and
# - prepare spatial inputs for subsequent environmental analyses.

# Use glimpse() to inspect the structure, variables,
# and data types of the household location dataset.

kc_location |>
  glimpse()


# Use names() to inspect the available variable names
# before linking household locations to mosquito count data.

names(
  kc_location
)

# The household location dataset contains information on:
#
# - health zone;
# - health area;
# - village;
# - house number;
# - latitude;
# - longitude; and
# - reported GPS precision.

# The combination of health_zone, health_area, village,
# and house_number uniquely identifies each sampled household
# and can be used to link the location and mosquito datasets.

# The latitude and longitude coordinates will later be used for:
#
# - spatial visualisation;
# - validation against health-zone boundaries;
# - environmental covariate extraction; and
# - species distribution modelling.

# 46. Summarise Anopheles counts for each sampled household --------------------

# Calculate the total number of Anopheles mosquitoes collected
# in each fixed household across the 12 collection months.
#
# This creates one row per sampled household and will be used
# to link household mosquito counts with geographic coordinates.

kc_mosq_clean <- kc_mosq |>
  group_by(
    health_zone,
    health_area,
    village,
    house_number
  ) |>
  summarise(
    total_count = sum(
      n_anopheles_collected
    ),
    .groups = "drop"
  )


# Confirm that the expected 650 fixed households are present.

nrow(
  kc_mosq_clean
)


# Confirm that the household totals retain the complete
# study count of 10,150 Anopheles mosquitoes.

sum(
  kc_mosq_clean$total_count
)


# Inspect the household-level mosquito count dataset.

kc_mosq_clean |>
  glimpse()


# 47. Join household mosquito counts with GPS coordinates ----------------------

# Add the geographic coordinates of each sampled household
# to the household-level mosquito count dataset.
#
# Households are matched using:
#
# - health zone;
# - health area;
# - village; and
# - house number.

kc_df <- kc_mosq_clean |>
  left_join(
    kc_location,
    by = c(
      "health_zone",
      "health_area",
      "village",
      "house_number"
    )
  )


# Inspect the resulting household spatial dataset.

kc_df |>
  glimpse()


# Confirm that the join preserved the expected
# 650 fixed households.

nrow(
  kc_df
)


# Check the completeness of household GPS coordinates.

coord_check <- kc_df |>
  summarise(
    n_households = n(),
    missing_lat = sum(
      is.na(lat_dd)
    ),
    missing_long = sum(
      is.na(long_dd)
    )
  )


# Display the coordinate completeness check.

coord_check

# 48. Check the range of household GPS coordinates

# Check whether the household coordinates fall within plausible
# geographic ranges before converting them to spatial points.

coordinate_range <- kc_df |>
  summarise(
    min_longitude = min(long_dd),
    max_longitude = max(long_dd),
    min_latitude = min(lat_dd),
    max_latitude = max(lat_dd)
  )

coordinate_range


# 49. Convert household coordinates to an sf spatial object

# Convert the household dataset from a regular data frame
# to a spatial point object using the recorded longitude
# and latitude coordinates.
#
# long_dd is used as the x-coordinate (longitude),
# and lat_dd as the y-coordinate (latitude).
#
# CRS 4326 corresponds to the WGS 84 geographic coordinate system,
# which is appropriate for the recorded household GPS coordinates.
#
# remove = FALSE keeps the original longitude and latitude columns
# in the dataset after creating the geometry column.

kc_sf <- st_as_sf(
  kc_df,
  coords = c(
    "long_dd",
    "lat_dd"
  ),
  crs = 4326,
  remove = FALSE
)


# Inspect the resulting spatial household dataset.

kc_sf |>
  glimpse()


# Confirm the coordinate reference system.

st_crs(
  kc_sf
)


# Confirm that all 650 sampled households were preserved.

nrow(
  kc_sf
)


# 50. Visualise the spatial distribution of sampled households

# Create an initial spatial quality-control map of the
# 650 sampled households across Kasaï-Central.
#
# This allows us to confirm that the household locations
# show the expected spatial distribution before adding
# health-zone boundaries.

p_household_locations <- ggplot() +

  geom_sf(
    data = kc_sf,
    size = 1
  ) +

  labs(
    title = "Spatial distribution of sampled households in Kasaï-Central",
    subtitle = "650 fixed households sampled across 26 health zones"
  ) +

  theme_bw() +

  theme(
    plot.title = element_text(
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )


# Display the household location map.

p_household_locations


# 51. Load the Kasaï-Central health-zone boundaries

# Read the GRID3 health-zone boundary file stored locally
# for this project.
#
# clean_names() standardises the variable names to make
# subsequent filtering and joins easier.

health_zones <- st_read(
  "data/downloads/grid3/grid3_cod_health_zones_v8_0.gpkg",
  quiet = TRUE
) |>
  clean_names()


# Inspect the structure of the health-zone spatial dataset.

health_zones |>
  glimpse()


# Check the province names recorded in the GRID3 dataset
# before selecting Kasaï-Central.

unique(
  health_zones$province
)


# 52. Select Kasaï-Central health-zone boundaries

# Retain only health zones located in Kasaï-Central Province.

kc_hz <- health_zones |>
  filter(
    province == "Kasaï-Central"
  )


# Confirm the number of health-zone polygons
# retained for Kasaï-Central.

nrow(
  kc_hz
)


# Inspect the available variable names to identify
# the health-zone name field.

names(
  kc_hz
)


# Check the coordinate reference system
# of the Kasaï-Central health-zone boundaries.

st_crs(
  kc_hz
)


# Compare it with the coordinate reference system
# of the household locations.

st_crs(
  kc_sf
)


# 53. Harmonise health-zone names between GRID3 and the entomological database

# Two health-zone names differ slightly between the GRID3 boundary dataset
# and the entomological database:
#
# GRID3        Entomological database
# Bena Leka    Benaleka
# Bena Tshadi  Benatshiadi
#
# These are naming differences only.
# Recode the GRID3 names so that they match the entomological database.

kc_hz <- kc_hz |>
  mutate(
    zonesante = recode(
      zonesante,
      "Bena Leka" = "Benaleka",
      "Bena Tshadi" = "Benatshiadi"
    )
  )


# Rename the GRID3 health-zone variable so that both datasets
# use the same variable name.

kc_hz <- kc_hz |>
  rename(
    health_zone = zonesante
  )


# Confirm that the health-zone names now match exactly
# between the entomological and spatial datasets.

setequal(
  unique(kc_df$health_zone),
  unique(kc_hz$health_zone)
)


# Confirm that the Kasaï-Central boundary dataset still contains
# 26 unique health zones.

n_distinct(
  kc_hz$health_zone
)

# 54. Plot sampled households within Kasaï-Central health zones

# Display the Kasaï-Central health-zone boundaries together
# with the 650 sampled household locations.
#
# This spatial quality-control map allows us to check whether
# household locations follow the expected geographic distribution
# across the 26 health zones before spatial validation.

p_households_health_zones <- ggplot() +

  # Display the 26 Kasaï-Central health-zone boundaries.

  geom_sf(
    data = kc_hz,
    fill = "grey98",
    colour = "grey70",
    linewidth = 0.4
  ) +

  # Add the 650 sampled household locations.

  geom_sf(
    data = kc_sf,
    size = 1
  ) +

  labs(
    title = "Spatial distribution of sampled households across Kasaï-Central health zones",
    subtitle = "650 fixed households sampled across 26 health zones"
  ) +

  theme_bw() +

  theme(
    plot.title = element_text(
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )


# Display the household and health-zone map.

p_households_health_zones


# 55. Validate household locations against their recorded health zones

# Spatially join each sampled household to the health-zone polygon
# in which its GPS coordinates fall.
#
# The recorded health-zone name is then compared with the
# health-zone assignment obtained from the GRID3 boundaries.

household_zone_check <- kc_sf |>
  st_join(
    kc_hz |>
      select(
        spatial_health_zone = health_zone
      ),
    join = st_within
  ) |>
  mutate(
    health_zone_match = health_zone == spatial_health_zone
  )


# Summarise the spatial validation.
#
# Ideally:
#
# - all 650 households fall within a health-zone polygon;
# - all 650 spatial health-zone assignments match the
#   health-zone names recorded in the entomological database.

spatial_validation <- household_zone_check |>
  st_drop_geometry() |>
  summarise(
    n_households = n(),
    outside_health_zone = sum(
      is.na(spatial_health_zone)
    ),
    matching_health_zone = sum(
      health_zone_match,
      na.rm = TRUE
    ),
    mismatching_health_zone = sum(
      health_zone_match == FALSE,
      na.rm = TRUE
    )
  )


# Display the spatial validation results.

spatial_validation


# 56. Create the PAMCA key map of representative vector-composition patterns

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
    "Secondary taxa combined"
  )
)


# Define colours consistent with the species-composition figures.
#
# Grey is used for Bobozo because the pattern represents
# several secondary vector species rather than one species.

pamca_pattern_colors <- c(
  "An. gambiae s.l. dominance" = "firebrick",
  "An. funestus gp dominance" = "goldenrod",
  "Secondary taxa combined" = "grey40"
)


# Add the representative vector-composition patterns
# to the Kasaï-Central health-zone boundaries.

pamca_hz_map <- kc_hz |>
  left_join(
    pamca_map_key,
    by = "health_zone"
  )


# Create an external boundary for Kasaï-Central.

kc_outline <- st_union(
  kc_hz
)


# Get the geographic extent of Kasaï-Central.
#
# Additional space is added to the right of the province
# to accommodate the legend without covering the map.

map_bbox <- st_bbox(
  kc_hz
)

map_width <- as.numeric(
  map_bbox["xmax"] - map_bbox["xmin"]
)


# Create the PAMCA key map.

p_pamca_key_map <- ggplot() +

  # Display all Kasaï-Central health zones.

  geom_sf(
    data = pamca_hz_map,
    fill = "grey97",
    colour = "grey75",
    linewidth = 0.25
  ) +

  # Highlight Mutoto, Kananga, and Bobozo.

  geom_sf(
    data = pamca_hz_map |>
      filter(
        !is.na(selected_pattern)
      ),
    aes(
      fill = selected_pattern
    ),
    colour = "black",
    linewidth = 0.7
  ) +

  # Draw the external boundary of Kasaï-Central.

  geom_sf(
    data = kc_outline,
    fill = NA,
    colour = "grey35",
    linewidth = 0.45
  ) +

  # Label the three representative health zones.

  geom_sf_text(
    data = pamca_hz_map |>
      filter(
        !is.na(selected_pattern)
      ),
    aes(
      label = health_zone
    ),
    fontface = "bold",
    size = 3.2,
    colour = "black"
  ) +

  # Apply colours corresponding to the representative patterns.

  scale_fill_manual(
    values = pamca_pattern_colors,
    breaks = c(
      "An. gambiae s.l. dominance",
      "An. funestus gp dominance",
      "Secondary taxa combined"
    ),
    labels = expression(
      italic("An. gambiae") ~ "s.l. dominance",
      italic("An. funestus") ~ "gp dominance",
      "Secondary vector species dominance (" *
        italic("An. paludis") *
        ", " *
        italic("An. hancocki") *
        ", " *
        italic("An. moucheti") *
        ")"
    )
  ) +

  # Keep the map closely fitted to Kasaï-Central.

  coord_sf(
    expand = FALSE,
    datum = NA
  ) +

  labs(
    title = "Representative vector-composition patterns",
    fill = NULL
  ) +

  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  ) +

  theme_void() +

  theme(
    # Format the map title.

    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 15,
      margin = margin(
        b = 5
      )
    ),

    # Position the legend below the map.

    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",

    # Keep the legend compact and readable.

    legend.background = element_blank(),

    legend.text = element_text(
      size = 8
    ),

    legend.key.width = grid::unit(
      0.42,
      "cm"
    ),

    legend.key.height = grid::unit(
      0.42,
      "cm"
    ),

    legend.spacing.x = grid::unit(
      0.15,
      "cm"
    ),

    legend.margin = margin(
      t = 4
    ),

    plot.margin = margin(
      5,
      5,
      5,
      5
    )
  )

# Display the PAMCA key map.

p_pamca_key_map


# 57. Save the PAMCA key map in high resolution

# Save the PAMCA key map as a high-resolution PNG.

ggsave(
  filename = "outputs/figures/pamca_representative_vector_patterns_map.png",
  plot = p_pamca_key_map,
  width = 8,
  height = 10.5,
  units = "in",
  dpi = 600,
  bg = "white"
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

# Create the Mikalayi health-zone layer.

mikalayi_hz <- kc_hz |>
  filter(
    health_zone == "Mikalayi"
  )


# Calculate the mean GPS location of the households surveyed
# in Mikalayi.

mikalayi_sentinel_site <- kc_location |>
  filter(
    health_zone == "Mikalayi"
  ) |>
  summarise(
    long_dd = mean(long_dd),
    lat_dd = mean(lat_dd)
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

  # Display all DRC provinces in white.

  geom_sf(
    data = drc_provinces,
    fill = "white",
    colour = "grey65",
    linewidth = 0.25
  ) +

  # Highlight Kasaï-Central Province in grey.

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

  # Highlight Mikalayi health zone in blue.
  #
  # A thinner boundary is used so that the sentinel-site point
  # remains clearly visible even when it is close to the edge.

  geom_sf(
    data = mikalayi_hz,
    aes(
      fill = "Mikalayi health zone"
    ),
    colour = "black",
    linewidth = 0.30
  ) +

  # Add the Mikalayi sentinel site using the mean GPS
  # coordinates of surveyed households in Mikalayi.

  geom_sf(
    data = mikalayi_sentinel_site,
    aes(
      shape = "Mikalayi sentinel site"
    ),
    size = 1.8,
    colour = "black"
  ) +

  # Define colours for Kasaï-Central and Mikalayi.

  scale_fill_manual(
    values = c(
      "Kasaï-Central Province" = "grey65",
      "Mikalayi health zone" = "deepskyblue3"
    ),
    breaks = c(
      "Kasaï-Central Province",
      "Mikalayi health zone"
    )
  ) +

  # Define the symbol for the Mikalayi sentinel site.

  scale_shape_manual(
    values = c(
      "Mikalayi sentinel site" = 19
    )
  ) +

  coord_sf(
    expand = FALSE,
    datum = NA
  ) +

  labs(
    fill = NULL,
    shape = NULL
  ) +

  guides(
    fill = guide_legend(
      order = 1
    ),
    shape = guide_legend(
      order = 2
    )
  ) +

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

# 61. Save the DRC study-location map in high resolution

# Save a high-resolution PNG version for the poster.

ggsave(
  filename = "outputs/figures/drc_kasai_central_mikalayi_location_map.png",
  plot = p_drc_kc_key_map,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)
