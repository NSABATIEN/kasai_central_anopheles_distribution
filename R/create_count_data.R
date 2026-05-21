library(readxl)
library(dplyr)
library(readr)
library(janitor)

vic_data_path <- "V:/1. PhD_Journey 2025_2026/PhD_Workspace/Thesis_Databases/kc_entomo_database"

data_folder_path <- vic_data_path

files <- list.files(
  data_folder_path,
  pattern = "^[0-9]{8}_drc_entomo_database_kc.xlsx?$",
  full.names = TRUE
)

latest_file <- files[which.max(file.info(files)$mtime)]

excel_sheets(latest_file)

species_data <- read_excel(
  latest_file,
  sheet = "species"
) |>
  clean_names()

names(species_data)
glimpse(species_data)


counts <- species_data |>
  rename(
    species = identification_taxon
  ) |>
  group_by(
    health_zone,
    health_area,
    village,
    house_number,
    species
  ) |>
  summarise(
    count = n(),
    .groups = "drop"
  )

write_csv(
  counts,
  "data/clean/kc_household_counts.csv"
)

list.files("data/clean")
