# load all required packages

library(tidyverse) # a collection of packages (including dplyr, ggplot2, readr, etc.) that work together for data wrangling and visualization
library(readxl) # reads Excel files and different sheets into R.
library(ggplot2) # for Data visualisation to clean, organize, and combine data from different sheets for analysis.
library(janitor) # cleans messy column names and tables (makes them consistent).e.g. clean_names() turns "Health.Zone" into "health_zone eg. filter(), select(), group_by(), summarise(), left_join()
library(naniar) # Checks and visualizes missing data to clean datasets before combining e.g. gg_miss_var()
library(dplyr) # organizes and manipulates data — filtering, arranging, summarizing, and joining tables

# set the data path for geodata
options( geodata_default_path = "data/raw")