library(readxl)
library(dplyr)

vic_data_path <- "V:/1. PhD_Journey 2025_2026/PhD_Workspace/Thesis_Databases/kc_entomo_database"
data_folder_path <- vic_data_path

files <- list.files(
  data_folder_path,
  pattern = "^[0-9]{8}_drc_entomo_database_kc.xlsx?$",
  full.names = TRUE
)

latest_file <- files[which.max(file.info(files)$mtime)]

data <- read_excel(latest_file, sheet = "species")

total_records <- nrow(data)
View(data)
View(data.frame(identification_taxon = data$identification_taxon))

species_summary <- data |>
  group_by(identification_taxon) |> 
  summarise(
    count = n(),
    percentage = round( (n() / total_records) * 100 , 2 ) 
  ) |>
  arrange(desc(count))

View(species_summary)

abdominal_stage_summary <- data |>
  group_by(abdominal_stage) |>
  summarise(
    count = n(),
    percentage = round( (n() / total_records) * 100 , 2 ) 
  ) |>
  arrange(desc(count)) 

View(abdominal_stage_summary)


# Write results to CSV
write.csv(species_summary, "species_summary.csv", row.names = FALSE)
write.csv(abdominal_summary, "abdominal_summary.csv", row.names = FALSE)



# load packages
library(tidyverse)
library(readr)

# load raw entomological database
raw_data <- read_csv(
  "data/raw/kc_entomo_database.csv"
)

# create household species counts
counts <- raw_data %>%
  
  # rename species column
  rename(
    species = identification_taxon
  ) %>%
  
  # count mosquitoes per household and species
  group_by(
    health_area,
    village,
    house_number,
    species
  ) %>%
  
  summarise(
    count = n(),
    .groups = "drop"
  )

# view results
View(counts)

# save clean counts dataset
write_csv(
  counts,
  "data/clean/kc_household_counts.csv"
)