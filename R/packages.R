# Packages used in the Kasaï-Central Anopheles distribution analysis
# load the packages required for data preparation, spatial analysis,
# environmental covariate analysis, species distribution modelling,
# and visualisation.

<<<<<<< HEAD
library(tidyverse) # a collection of packages (including dplyr, ggplot2, readr, etc.) that work together for data wrangling and visualization
library(readxl) # reads Excel files and different sheets into R.
library(ggplot2) # for Data visualisation to clean, organize, and combine data from different sheets for analysis.
library(janitor) # cleans messy column names and tables (makes them consistent).e.g. clean_names() turns "Health.Zone" into "health_zone eg. filter(), select(), group_by(), summarise(), left_join()
library(naniar) # Checks and visualizes missing data to clean datasets before combining e.g. gg_miss_var()
library(dplyr) # organizes and manipulates data — filtering, arranging, summarizing, and joining tables
library(raster)
library(dismo)
library(stats)
library(patchwork)
library(terra)
library(tidyterra)
library(geodata)
library(dplyr)
library(forcats)
library(lubridate)
library(ggh4x)
library(sf) # invoked in read_in_kc_data.R

# set the data path for geodata
options(geodata_default_path = "data/raw")
=======
# Data wrangling and quality control
library(tidyverse) # Data manipulation, visualisation, and workflow tools
library(readxl) # Import Excel spreadsheets
library(janitor) # Clean and standardise column names
library(naniar) # Explore and visualise missing data

# Spatial data handling and environmental covariates
library(sf) # Work with vector spatial data (points, polygons, lines)
library(terra) # Handle raster data and spatial analysis
library(raster) # Raster processing
library(dismo) # Species distribution modelling
library(geodata) # Download environmental data
library(tidyterra) # Use terra objects with ggplot2 and tidyverse

# Species distribution modelling
library(mgcv) # Fit Generalised Additive Models (GAM)

# Visualisation and plot 
library(patchwork) # Combine multiple ggplots into a single figure
library(ggh4x) # Advanced extensions for ggplot2
library(idpalette) # TheKids colour palettes for better quality plots

# Spatial ecology and SDM utilities
library(sdmtools) # Spatial and SDM evaluation tools (e.g., distances, metrics)
>>>>>>> 8aa12da9630e746179e5a9d0fbb071e4ab7a2198
