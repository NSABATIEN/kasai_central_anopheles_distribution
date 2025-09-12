# Read in field data from MS Excel

## Step 1: I need to load R packages that I will use in my code

### Let load the packages

library(tidyverse)
library(readxl)
library(ggplot2)
library(janitor)
library(naniar)

## Step 2 : I need to tell R where to my MS Excel

# Make a path for David's computer
dhd_data_path <- "~/pCloud Drive/R/data/va/vic/kc/raw"

# Make a path for Vic's computer

get_data_folder_path <- function(user = c("dhd", "vic", "ger", "nick")) {
  if (user == "dhd") {
    path <- dhd_data_path
  } else {
    message("where is the data on your computer? Let's talk about this.")
  }
  return(path)
}

data_folder_path <- get_data_folder_path("dhd")

# Get all Excel files
files <- list.files(
  data_folder_path,
  pattern = "^[0-9]{8}_drc_entomo_database_kc.xlsx?$",
  full.names = TRUE
)

# Find the most recent by modification time
latest_file <- files[which.max(file.info(files)$mtime)]

# Is it a multisheet file?

excel_sheets(latest_file)

# Sheet 1 is the collection data, as the name suggests
read_excel(latest_file, sheet = 1) |> glimpse()

# Sheet 4 looks like it has the location info
read_excel(latest_file, sheet = "location") |> glimpse()

## Step 3 : Now let read in collection data from MS Excel. # Vic had already fixed all the column names, but if not we could use a call to janitor::clean_names(kc_mosq). Let's show how anyway

kc_mosq <- read_excel(latest_file, sheet = "collection_data") |>
  clean_names()

## Visualize the data

kc_mosq |> glimpse()

# the date is not recognised as a date, let's fix that

kc_mosq <- kc_mosq |>
  mutate(
    date = convert_to_date(
      date, # the variable name
      character_fun = lubridate::dmy # give it the clue to how to interpret content
    )
  ) |> #
  glimpse()

# We probably don't want repeat month data, Vic, so what do we want?  Also, we don't need all of these fields, which ones do we need

# Step 4: Make a summary of the data that makes sense for the Anopheles distribution and composition model, and drop the fields that won't be useful for the modelling steps

message("Let's talk about this!")

# Let's see how to join the location information

kc_location <- read_excel(latest_file, sheet = "location") |>
  clean_names() |>
  glimpse()

# A couple of field names are different here compared to the collection data, let's change them so that the join command is nice and simple
kc_location <- kc_location |>
  rename(
    house_number = code_house,
    health_zone = zs,
    health_area = as
  )

kc_df <- left_join(
  kc_mosq,
  kc_location,
  by = c("health_zone", "health_area", "house_number")
) |>
  select(
    -c(comment, precision_m)
  )

# Step 5: might be nice to add a summary data output of plots in space, time, and a table of NAs?
library(patchwork)
library(terra)
library(tidyterra)


drc <- vect("~/pCloud Drive/R/data/va/vic/gadm/gadm41_COD_0_pk_low.rds")

kc <- vect("~/pCloud Drive/R/data/va/vic/gadm/gadm41_COD_1_pk.rds") |>
  filter(
    NAME_1 == "Kasaï-Central"
  )

missing_data <- naniar::gg_miss_var(kc_df, show_pct = TRUE)

plot_kc_df <- filter(kc_df, !is.na(long_dd)) |>
  sf::st_as_sf(
    coords = c("long_dd", "lat_dd"),
    crs = 4326
  )

location <- ggplot() +
  geom_spatvector(data = kc) +
  geom_sf(data = plot_kc_df)
theme_minimal()

missing_data + location + plot_layout(ncol = 1)
