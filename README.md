# **PhD Objective 1 Roadmap : Preliminary Indoor *Anopheles* Distribution Mapping in Kasaï-Central, Democratic Republic of the Congo**

This roadmap outlines the key steps for Objective 1, which focuses on developing a fine-scale geospatial model of malaria vector species distribution using indoor household entomological collections from Kasaï-Central.

## **Step 1 : Load packages (*done*)**

- Load all packages required

## **Step 2 : Read in KC data (*done*)**

<<<<<<< HEAD
- Define a path and find the latest Excel file ✓
- Read in Excel sheets and clean the data ✓
- Clean and summarize collection data ✓
- Clean and summarize location data ✓
- Join collection data and location data ✓
- Preliminary Spatial visualization and quality check ✓

## **Step 3 : Prepare environmental covariates (*done*)**

- Download environmental covariates :
  - WorldClim bioclimatic variables downloaded using the geodata package ✓
  - Landcover variables downloaded in geodata package using global_landcover function ✓
- Prepare environmental rasters :
  - Crop climate and landcover rasters to the Kasaï-Central extent ✓
  - Visualise raw bioclimatic and landcover variables ✓
- Link both covariates (climate and landcover) to location data :
  - Plot the climate and landcover ✓
=======
- Define a path and find the latest Excel file (*done*)
- Read in Excel sheets and clean the data (*done*)
- Clean and summarize collection data (*done*)
- Clean and summarize location data (*done*)
- Join collection data and location data (*done*)
- Preliminary Spatial visualization and quality check (*done*)

## **Step 3 : Prepare environmental covariates (*done*)**

- Download environmental covariates (*done*) :
  - WorldClim bioclimatic variables downloaded using the geodata package ✓\
  - Landcover variables downloaded in geodata package using global_landcover function ✓
- Prepare environmental rasters (*done*):
  - Crop climate and landcover rasters to the Kasaï-Central extent ✓
  - Visualise raw bioclimatic and landcover variables
- Link both covariates (climate and landcover) to location data ✓
  - Plot the landcover \_crop ✓
>>>>>>> 5be829ec488ecc95d391dcf29afa086843fae442
  - Prepare covariates for the whole country ✓

## **Step 4 : Conduct PCA of environmental covariates (*done*)**

- Conduct PCA on bioclimatic variables ✓
- Conduct PCA on landcover variables ✓
- Reduce correlated environmental variables into major environmental gradients ✓
- Retain PCA components explaining \>90% of environmental variation ✓
- Interpret PCA loadings and environmental gradients ✓
- Produce PCA maps for climatic and landscape components ✓

## **Step 5 : Conduct Multivariate Environmental Similarity Analysis (MESS) (*done*)**

- Prepare PCA covariates for MESS analysis ✓
- Conduct MESS analysis across Kasaï-Central ✓
- Identify environmentally similar and dissimilar areas relative to surveyed households ✓
- Generate environmental dissimilarity layers ✓
- Produce preliminary environmental similarity maps prior to species distribution modelling ✓

## **Step 6 : Fit the model (*in progress*)**

Binomial model for presence/absence data, or poisson model for count data will be using (*in progress*)

## **Step 7 : Model Diagnostics Using DHARMa (*not yet*)**
