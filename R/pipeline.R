# ============================================================
# Main analysis pipeline
# Project: Kasaï-Central Anopheles distribution
# ============================================================

# 1. Load packages
source("R/packages.R") #Loads all R packages I need for my project

# 2. Prepare entomological data
source("R/prep_ento_data.R") #Clean and prepare my entomo data

# 3. Prepare covariates
source("R/prep_covariates.R") #Download/prepare my environmental raster covariates

# 4. Fit model
source("R/fit_model.R") 
#Fits the model using the prepared entomological data + covariates

# 5. Environmental similarity / MESS
source("R/env_similarity.R")

# 6. Model diagnostics
source("R/dharma.R") #Check model diagnostics after fitting the model