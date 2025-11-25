# Read in field data from MS Excel

## Step 1: I need to load R packages that I will use in my code

### Let load the packages

library(tidyverse)
library(readxl)
library(ggplot2)
library(stats)
library(janitor)
library(naniar)
library(patchwork)
library(terra)
library(tidyterra)
library(geodata)
library(dplyr)
library(forcats)

## Step 2 : I need to tell R where to my MS Excel

# Make a path for David's computer
# dhd_data_path <- "~/pCloud Drive/R/data/va/vic/kc/raw"
vic_data_path <- "V:/1. Vic's PhD Journey 2025/1. Vic's PhD Journey 2025/1. LSTM PhD work project/1.kc_entomo_database"

# Make a path for Vic's computer

# get_data_folder_path <- function(user = c("dhd", "vic", "ger", "nick")) {
#   if (user == "dhd") {
#     path <- dhd_data_path
#   } else {
#     message("where is the data on your computer? Let's talk about this.")
#   }
#   return(path)
# }

# data_folder_path <- get_data_folder_path("dhd")

data_folder_path <- vic_data_path

# Get all Excel files


files <- list.files(
  data_folder_path,
  pattern = "^[0-9]{8}_drc_entomo_database_kc.xlsx?$",
  full.names = TRUE
)

# Find the most recent by modification time

latest_file <- files[which.max(file.info(files)$mtime)]

# need to understand the concept of index (vic)
# files[which.max(file.info(files)$size)] #(this ligne of code is not concern my own code but it's can help me to understand the concept about file info wich is can be the size, mtime, ctime or actime :keep it in mind) 

# Is it a multisheet file?

excel_sheets(latest_file)

# Sheet 1 is the collection data, as the name suggests

read_excel(latest_file, sheet = 1) |> glimpse()

# need to learn what glimpse doing  #(glimpse is the function)

# Sheet 4 looks like it has the location info

read_excel(latest_file, sheet = "location") |> glimpse()


## Step 3 : Now let read in collection data from MS Excel. # Vic had already fixed all the column names, but if not we could use a call to janitor::clean_names(kc_mosq). Let's show how anyway

kc_mosq <- read_excel(latest_file, sheet = "data") |>
  clean_names()

## Visualize the data

kc_mosq |> glimpse()

# the date is not recognised as a date, let's fix that # (need to keep it in your mind : POSIXct wich is mean date)

kc_mosq <- kc_mosq |>
  mutate(
    date = convert_to_date(
      date, # the variable name
      character_fun = lubridate::dmy # give it the clue to how to interpret content
    )
  ) |> #
  glimpse()

# We probably don't want repeat month data, Vic, so what do we want?  Also, we don't need all of these fields, which ones do we need
# Vic comments : we need to keep the variables which make sense for my  objective 1 and composition model 

# Step 4: Make a summary of the data that makes sense for the Anopheles distribution and composition model, and drop the fields that won't be useful for the modelling steps
# To do that, I can use dplyr package

# library(dplyr) # already done above

kc_mosq_clean <- kc_mosq |> 
  dplyr::select(-collection_month,
                -date,
                -unique_initials_of_health_area_and_village,
                -unique_collection,
                -sample_transferred_pool_to_kasai_central,
                -sample_transferred_kasai_central_to_kinshasa,
                -comment) |> 
  group_by(village,house_number) |> 
  summarise(total_count = sum(n_anopheles_collected),.groups = "drop") 

# p_month <- kc_plot |>
#   ggplot(aes(x = collection_month, y = n_anopheles_collected)) +
#   geom_boxplot()  +
#   labs(title = "Mosquito count per collection month in Kasaï-Central",
#     x = "collection_month", y = "n_anopheles_collected") +
#   theme()
# plot(p_month)
# 
# p_zone_month <- kc_plot_ord |>
#   ggplot(aes(y = health_zone, x = n_anopheles_collected)) +
#   geom_boxplot() +
#   facet_wrap(~ collection_month, ncol = 2, scales = "free_y") +
#   labs(title = "kc_data by health zone and collection_month",
#        x = "n_anopheles_collected", y = "health_zone") +
#   theme() 
# plot(p_zone_month)
 
# message("Let's talk about this!")

# Let's see how to join the location information (Sheet 4)

# 23.10.2025 : let have a look on species
kc_species <- read_excel(latest_file, sheet = "species") |>
  clean_names()

kc_taxon_plot <- kc_species |>
  dplyr::filter(!is.na(identification_taxon)) |>
  dplyr::mutate(
    collection_month = factor(collection_month,
                              levels = c("month_1","month_2","month_3","month_4")),
    health_zone = as.factor(health_zone)
  ) |>
  dplyr::select(health_zone, collection_month, identification_taxon)

p_taxon_counts <- kc_taxon_plot |>
  ggplot(aes(x = health_zone, fill = identification_taxon)) +
  geom_bar() +
  coord_flip() +
  facet_wrap(~ collection_month, ncol = 2, scales = "free_y") +
  labs(
    title = "Identified mosquito species by health zone and collection month",
    x = "health_zone",
    y = "Number of individuals per species per village per month",
    fill = "Taxon"
  ) +
  theme_grey(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
p_taxon_counts <- p_taxon_counts +
  theme(
    axis.text.y = element_text(size = 6),    # smaller labels
    strip.text = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(face = "bold")
  )

plot(p_taxon_counts)

kc_location <- read_excel(latest_file, sheet = "location") |>
  clean_names() |>
  glimpse()

# Sheet 4 contains "NA" values that's should remove 
# After completing all collection gps data I should avoid to use theses two lines of code (111 and 112)

#First way to filter
# kc_location <- read_excel(latest_file, sheet = "location") |> 
#filter(!(is.na(lat_dd))) 

#second way to filter 

kc_location_clean <- read_excel(latest_file, sheet = "location") |> 
  glimpse()

# A couple of field names are different here compared to the collection data, let's change them so that the join command is nice and simple
#kc_location <- kc_location |>
  # rename(
  #   zs = health_zone,
  #   as = health_area,
  #   code_house = house_number
  # )

# kc_df <- left_join(
#   kc_mosq_clean,
#   kc_location_clean,
#   by = c("village", "house_number")
# ) |>
#   select(
#     -c(precision)
#   ) |> 
#   filter(!(is.na(lat_dd) & is.na(long_dd)))

kc_df <- left_join(
  kc_mosq_clean,
  kc_location_clean,
  by = c("village", "house_number")
) |>
  select(-precision) |>
  mutate(
    status = ifelse(is.na(lat_dd) | is.na(long_dd),
                    "To be georeferenced",
                    "Georeferenced")
  )


# Step 5: might be nice to add a summary data output of plots in space, time, and a table of NAs?
# library(patchwork)
# library(terra)
# library(tidyterra)

# this shp looks incomplete
#drc <- vect("V:/1. Vector Atlas and my PhD at LSTM/1. My PhD with LSTM/1. LSTM PhD work project/10. kc_shapfiles/rdc_aires-de-sante/RDC_Aires de santé.shp")

# get administrative area for a country


drc_shp <-gadm(
  country = "COD",
  level= 0,
  path= "data/downloads"
)

drc_shp_prv <-gadm(
  country = "COD",
  level= 1,
  path= "data/downloads"
)

# If I work with multiples provinces in DRC # use in function 
# drc_shp_prv |> 
#   filter(
#     NAME_1 %in% c("Kasaï-Central","Haut-Uele","Kongo-Central"),
#   ) 
# exemple 

# For one province 

kc<- drc_shp_prv |> 
  filter(
    NAME_1 == "Kasaï-Central"
      )
  

missing_data <- naniar::gg_miss_var(kc_df, show_pct = TRUE)

# before the : : is a package, # after the : : is a function
# read : spatial raster and vector data
# sf is the package for vector data
# vector data can be any thing is represent by the point, line, or polygon 
# spatial raster it's a grid 
# what kind of object is location? is ggplot object
# rm function
# ls function
# use this function rm(list=ls()) in case you want to see what's, 
# the restard R session with this cmd+shift +f10


plot_kc_df <- filter(kc_df, !is.na(long_dd)) |>
  sf::st_as_sf(
    coords = c("long_dd", "lat_dd"),
    crs = 4326
  )

location <- ggplot() +
  geom_spatvector(data = kc) +
  geom_sf(data = plot_kc_df) +
theme_minimal()

missing_data + location + plot_layout(ncol = 1)
