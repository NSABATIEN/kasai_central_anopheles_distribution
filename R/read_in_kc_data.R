# Read in field data from MS Excel

# 1. load all required packages
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
library(lubridate)
library(ggh4x)
#

# 2. Tell R where to find the Excel file
## Make a path for David's computer
## dhd_data_path <- "~/pCloud Drive/R/data/va/vic/kc/raw"


vic_data_path <- "V:/1. PhD_Journey 2025_2026/PhD_Workspace/Thesis_Databases/kc_entomo_database"

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

# 3. Get all Excel files in a folder
files <- list.files(
  data_folder_path,
  pattern = "^[0-9]{8}_drc_entomo_database_kc.xlsx?$",
  full.names = TRUE
)

# 4. Select the latest Excel file by modification time
latest_file <- files[which.max(file.info(files)$mtime)]

#print(latest_file)

# need to understand the concept of index (vic)
# Note: files[which.max(file.info(files)$size)] #(this line of code is not concern my own code but I keep it as an example to help the understanding of indexing + `file.info about the`size, mtime, ctime, atime)
# files[which.max(file.info(files)$size)]  # select the largest file
# Is it a multisheet file?

excel_sheets(latest_file)

# Sheet 1 is the collection data, as the name suggests

read_excel(latest_file, sheet = 1) |> glimpse()

# Sheet 4 looks like it has the location info
read_excel(latest_file, sheet = "location") |> glimpse()


## 5. Now let read in collection data from MS Excel. # Vic had already fixed all the column names, but if not we could use a call to janitor::clean_names(kc_mosq). Let's show how anyway
kc_mosq <- read_excel(latest_file, sheet = "data") |>
  clean_names()

#read in species sheet from MS Excel
kc_species <- read_excel(latest_file, sheet = "species") |>
  clean_names()

#read in location sheet from MS Excel
kc_location <- read_excel(latest_file, sheet = "location") |>
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

# Note : We probably don't want repeat month data, Vic, so what do we want?  Also, we don't need all of these fields, which ones do we need
# Note : we need to keep the variables which make sense for my  objective 1 and composition model 

# 6. Make a summary of the data that makes sense for the Anopheles distribution and composition model, and drop the fields that won't be useful for the modelling steps
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

## Create relabelled variable ploting the month collection
kc_mosq_plot <- kc_mosq |>
  mutate(
    collection_month_label = case_when(
      collection_month == "month_1"  ~ "Apr 2025",
      collection_month == "month_2"  ~ "May 2025",
      collection_month == "month_3"  ~ "Jun 2025",
      collection_month == "month_4"  ~ "Jul 2025",
      collection_month == "month_5"  ~ "Aug 2025",
      collection_month == "month_6"  ~ "Sep 2025",
      collection_month == "month_7"  ~ "Oct 2025",
      collection_month == "month_8"  ~ "Nov 2025",
      collection_month == "month_9"  ~ "Dec 2025",
      collection_month == "month_10" ~ "Jan 2026",
      collection_month == "month_11" ~ "Feb 2026",
      collection_month == "month_12" ~ "Mar 2026",
    ),
    collection_month_label = factor(
      collection_month_label,
      levels = c(
        "Apr 2025", "May 2025", "Jun 2025", "Jul 2025",
        "Aug 2025", "Sep 2025", "Oct 2025", "Nov 2025",
        "Dec 2025", "Jan 2026", "Feb 2026", "Mar 2026"
      )
    )
  )
# Check what I get now
kc_mosq_plot |>
  count(collection_month, collection_month_label)

# Plot mosquito counts by collection month
# p_month <- kc_mosq_plot |>
#   ggplot(aes(x = collection_month_label, y = n_anopheles_collected)) +
#   geom_boxplot()  +
#   labs(title = "Mosquito count per collection month in Kasaï-Central",
#     x = "Collection_month", y = "Number of Anopheles collected") +
#   theme()
# plot(p_month)
p_month <- kc_mosq_plot |>
  ggplot(aes(x = collection_month_label, y = n_anopheles_collected)) +
  geom_boxplot() +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3
  ) +
  labs(
    title = "Monthly distribution of Anopheles abundance per household in Kasaï-Central",
    x = "Collection month",
    y = "Number of Anopheles collected per household"
  ) +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(p_month)
## Monthly evolution of mosquito count

p_month <- kc_mosq_plot |>
  group_by(collection_month_label) |>
  summarise(total_count = sum(n_anopheles_collected, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = collection_month_label, y = total_count, group = 1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(
    breaks = seq(0, 1200, by = 200),
    limits = c(0, 1200)
  ) +
  labs(
    title = "Monthly total Anopheles collections in Kasaï-Central",
    x = "collection_month",
    y = "Total number of Anopheles collected"
  ) +
  theme()

p_month


# Plot per zone per month

p_zone_month <-kc_mosq_plot |>
  ggplot(aes(y = health_zone, x = n_anopheles_collected)) +
  geom_boxplot() +
  facet_wrap(~ collection_month_label, ncol = 5, scales = "free_y") +
  labs(title = "Monthly distribution of household Anopheles collections
                across health zones in Kasaï-Central",
       x = "Number of Anopheles collected per household", y = "health_zone") +
  theme(axis.text.y = element_text(size = 6))
plot(p_zone_month)


# Species visualization
kc_species <- read_excel(latest_file, sheet = "species") |>
  clean_names()

kc_species_plot <- kc_species |>
  mutate(
    collection_month_label = case_when(
      collection_month == "month_1"  ~ "Apr 2025",
      collection_month == "month_2"  ~ "May 2025",
      collection_month == "month_3"  ~ "Jun 2025",
      collection_month == "month_4"  ~ "Jul 2025",
      collection_month == "month_5"  ~ "Aug 2025",
      collection_month == "month_6"  ~ "Sep 2025",
      collection_month == "month_7"  ~ "Oct 2025",
      collection_month == "month_8"  ~ "Nov 2025",
      collection_month == "month_9"  ~ "Dec 2025",
      collection_month == "month_10" ~ "Jan 2026",
      collection_month == "month_11" ~ "Feb 2026",
      collection_month == "month_12" ~ "Mar 2026"
    ),
    collection_month_label = factor(
      collection_month_label,
      levels = c(
        "Apr 2025", "May 2025", "Jun 2025", "Jul 2025",
        "Aug 2025", "Sep 2025", "Oct 2025", "Nov 2025",
        "Dec 2025", "Jan 2026", "Feb 2026", "Mar 2026"
      )
    )
  )


kc_taxon_summary <- kc_species_plot |>
  filter(!is.na(identification_taxon)) |>
  group_by(health_zone, collection_month_label, identification_taxon) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(health_zone, collection_month_label) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

taxon_order <- kc_taxon_summary |>
  group_by(identification_taxon) |>
  summarise(total_n = sum(n), .groups = "drop") |>
  arrange(desc(total_n)) |>
  pull(identification_taxon)

kc_taxon_summary <- kc_taxon_summary |>
  mutate(
    identification_taxon = factor(
      identification_taxon,
      levels = taxon_order
    )
  )
# 
# p_taxon <- ggplot(kc_taxon_summary,
#                   aes(x = collection_month_label,
#                       y = prop,
#                       fill = identification_taxon)) +
#   geom_col(position = "fill") +
#   scale_fill_manual(
#     values = c(
#       "An. gambiae s.l." = "firebrick",
#       "An. funestus gp" = "goldenrod",
#       "An. hancocki" = "forestgreen",
#       "An. moucheti" = "darkturquoise",
#       "An. paludis" = "mediumpurple",
#       "An. sp" = "grey50",
#       "An. ziemanni" = "deeppink"
#     ),
#     labels = c(
#       "An. gambiae s.l." = expression(italic("An. gambiae") ~ "s.l."),
#       "An. funestus gp" = expression(italic("An. funestus") ~ "gp"),
#       "An. hancocki" = expression(italic("An. hancocki")),
#       "An. moucheti" = expression(italic("An. moucheti")),
#       "An. paludis" = expression(italic("An. paludis")),
#       "An. sp" = expression(italic("An.") ~ "sp"),
#       "An. ziemanni" = expression(italic("An. ziemanni"))
#     )
#   ) +
#   facet_wrap(~ health_zone, ncol = 4) +
#   scale_y_continuous(labels = scales::percent) +
#   scale_x_discrete(
#     labels = function(x) stringr::str_wrap(x, width = 8)
#   ) +
#   labs(
#     title = "Spatial and temporal variation in Anopheles species composition across health zones",
#     x = "Collection month",
#     y = "Proportion",
#     fill = "Anopheles species"
#     ) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
#     strip.text = element_text(size = 8)
#   )
# p_taxon

#kc graph with count data 
month_levels <- c(
  "Apr 2025", "May 2025", "Jun 2025", "Jul 2025",
  "Aug 2025", "Sep 2025", "Oct 2025", "Nov 2025",
  "Dec 2025", "Jan 2026", "Feb 2026", "Mar 2026"
)

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

separator_positions <- seq(1.5, length(month_levels) - 0.5, by = 1)

kc_taxon_summary <- kc_taxon_summary |>
  mutate(
    collection_month_label = factor(collection_month_label, levels = month_levels)
  )

species_order <- kc_taxon_summary |>
  group_by(identification_taxon) |>
  summarise(total_count = sum(n, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total_count)) |>
  pull(identification_taxon)

kc_taxon_summary <- kc_taxon_summary |>
  mutate(
    identification_taxon = factor(identification_taxon, levels = species_order)
  )

hz_total_counts <- kc_taxon_summary |>
  group_by(health_zone, collection_month_label) |>
  summarise(monthly_total = sum(n, na.rm = TRUE), .groups = "drop") |>
  group_by(health_zone) |>
  summarise(
    max_monthly_mosquitoes = max(monthly_total, na.rm = TRUE),
    total_mosquitoes = sum(monthly_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    abundance_group = case_when(
      max_monthly_mosquitoes >= 100 ~ "High abundance",
      max_monthly_mosquitoes >= 30  ~ "Medium abundance",
      TRUE ~ "Low abundance"
    ),
    abundance_group = factor(
      abundance_group,
      levels = c("High abundance", "Medium abundance", "Low abundance")
    )
  )

kc_taxon_summary_grouped <- kc_taxon_summary |>
  left_join(
    hz_total_counts |>
      select(health_zone, total_mosquitoes, max_monthly_mosquitoes, abundance_group),
    by = "health_zone"
  ) |>
  arrange(abundance_group, desc(max_monthly_mosquitoes)) |>
  mutate(
    health_zone = factor(health_zone, levels = unique(health_zone))
  )

# Dummy data to force all species to appear in the legend
legend_dummy <- tibble(
  collection_month_label = factor("Apr 2025", levels = month_levels),
  n = 0,
  identification_taxon = factor(species_order, levels = species_order)
)

make_abundance_plot <- function(group_name) {
  
  y_limits <- case_when(
    group_name == "High abundance" ~ list(c(0, 200)),
    group_name == "Medium abundance" ~ list(c(0, 100)),
    group_name == "Low abundance" ~ list(c(0, 30))
  )[[1]]
  
  y_breaks <- case_when(
    group_name == "High abundance" ~ list(seq(0, 200, by = 40)),
    group_name == "Medium abundance" ~ list(seq(0, 100, by = 20)),
    group_name == "Low abundance" ~ list(seq(0, 30, by = 10))
  )[[1]]
  
  kc_taxon_summary_grouped |>
    filter(abundance_group == group_name) |>
    ggplot(aes(x = collection_month_label, y = n, fill = identification_taxon)) +
    geom_vline(
      xintercept = separator_positions,
      colour = "grey70",
      linewidth = 0.35
    ) +
    geom_col(position = "stack", width = 0.75) +
    geom_col(
      data = legend_dummy,
      aes(
        x = collection_month_label,
        y = n,
        fill = identification_taxon
      ),
      inherit.aes = FALSE,
      show.legend = TRUE
    ) +
    facet_grid(
      rows = vars(abundance_group),
      cols = vars(health_zone),
      scales = "free_y",
      space = "free_x",
      switch = "y"
    ) +
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    scale_x_discrete(labels = month_labels, drop = FALSE) +
    scale_fill_manual(
      limits = species_order,
      breaks = species_order,
      drop = FALSE,
      values = c(
        "An. gambiae s.l." = "firebrick",
        "An. funestus gp" = "goldenrod",
        "An. paludis" = "mediumpurple",
        "An. hancocki" = "forestgreen",
        "An. sp" = "grey50",
        "An. moucheti" = "darkturquoise",
        "An. ziemanni" = "deeppink"
      ),
      labels = c(
        "An. gambiae s.l." = expression(italic("An. gambiae") ~ "s.l."),
        "An. funestus gp" = expression(italic("An. funestus") ~ "gp"),
        "An. paludis" = expression(italic("An. paludis")),
        "An. hancocki" = expression(italic("An. hancocki")),
        "An. sp" = expression(italic("An.") ~ "sp"),
        "An. moucheti" = expression(italic("An. moucheti")),
        "An. ziemanni" = expression(italic("An. ziemanni"))
      )
    ) +
    labs(
      x = NULL,
      y = "Mosquito count",
      fill = "Anopheles species"
    ) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "grey90", colour = "black"),
      strip.text.x = element_text(face = "bold", size = 8),
      strip.text.y.left = element_text(face = "bold", size = 11, angle = 90),
      
      axis.text.x = if (group_name == "Low abundance") {
        element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)
      } else {
        element_blank()
      },
      
      axis.ticks.x = element_blank(),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3),
      panel.grid.minor.y = element_blank(),
      
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 11),
      
      legend.position = if (group_name == "Medium abundance") "right" else "none",
      legend.justification = "center",
      legend.background = element_blank(),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 10),
      
      panel.spacing.x = unit(0.05, "lines"),
      panel.spacing.y = unit(0.2, "lines")
    )
}

p_high <- make_abundance_plot("High abundance")
p_medium <- make_abundance_plot("Medium abundance")
p_low <- make_abundance_plot("Low abundance")

final_plot <- 
  (p_high / p_medium / p_low) +
  plot_annotation(
    title = "Spatial and temporal variation in Anopheles species abundance across Health zones",
    subtitle = "Health zones grouped by maximum monthly abundance: High ≥100; Medium 30–99; Low <30",
    caption = "Collection month"
  ) &
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 13, hjust = 0.5)
  )

final_plot

########### Join location information
kc_location <- read_excel(latest_file, sheet = "location") |>
  clean_names() |>
  glimpse()


# Sheet 4 contains "NA" values that's should remove 
# After completing all collection gps data I should avoid to use theses two lines of code (111 and 112)

#First way to filter
# kc_location <- read_excel(latest_file, sheet = "location") |> 
#filter(!(is.na(lat_dd))) 

#second way to filter 
# 
# kc_location_clean <- read_excel(latest_file, sheet = "location") |> 
#   glimpse()

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
  kc_location,
  by = c("village", "house_number")
) |>
  select(-precision)

#check
coord_check <- kc_df |>
  summarise(
    n_households = n(),
    missing_lat = sum(is.na(lat_dd)),
    missing_long = sum(is.na(long_dd))
  )

print(coord_check)

#write_csv(kc_location, "data/clean/kc_household_coords.csv")

# Step 5: might be nice to add a summary data output of plots in space, time, and a table of NAs?
# library(patchwork)
# library(terra)
# library(tidyterra)

# this shp looks incomplete
#drc <- vect("V:/1. Vector Atlas and my PhD at LSTM/1. My PhD with LSTM/1. LSTM PhD work project/10. kc_shapfiles/rdc_aires-de-sante/RDC_Aires de santé.shp")

# get administrative area for a country


# drc_shp <-gadm(
#   country = "COD",
#   level= 0,
#   path= "data/downloads"
# )
# 
# drc_shp_prv <-gadm(
#   country = "COD",
#   level= 1,
#   path= "data/downloads"
# )

# Get GRID3 administrative health-zone boundaries for the country
health_zones <- sf::st_read(
  "data/downloads/grid3/grid3_cod_health_zones_v8_0.gpkg"
) |>
  janitor::clean_names()

# Filter Kasaï-Central health zones
kc_hz <- health_zones |>
  filter(
    province == "Kasaï-Central"
  )

# Save Kasaï-Central health-zone boundaries
sf::st_write(
  kc_hz,
  "data/clean/kc_health_zones.gpkg",
  delete_dsn = TRUE
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


# plot_kc_df <- filter(kc_df, !is.na(long_dd)) |>
#   sf::st_as_sf(
#     coords = c("long_dd", "lat_dd"),
#     crs = 4326
#   )

plot_kc_df <- kc_df |>
  filter(!is.na(long_dd), !is.na(lat_dd)) |>
  sf::st_as_sf(
    coords = c("long_dd", "lat_dd"),
    crs = 4326
  )

location <- ggplot() +
  geom_sf(data = kc_hz, fill = "white", colour = "black") +
  geom_sf(data = plot_kc_df) +
  theme_minimal() +
  labs(
    title = "Household collection points in Kasaï-Central",
  )


location

# missing_data + location + patchwork::plot_layout(ncol = 1)


# kc_df |>
#   filter(lat_dd > 0)
# 
# points_sf |>
#   filter(outside_kc) |>
#   st_drop_geometry() |>
#   View()

