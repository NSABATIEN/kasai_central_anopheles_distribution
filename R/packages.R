# Packages used in the Kasaï-Central Anopheles distribution analysis
# load the packages required for data preparation, spatial analysis,
# environmental covariate analysis, species distribution modelling,
# and visualisation.

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