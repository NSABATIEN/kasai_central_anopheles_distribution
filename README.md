# **Vic’s Roadmap objective 1**

This roadmap outlines the key steps to be completed for Objective 1,
which focuses on geospatial modeling of malaria vector species
distribution across Kasaï-Central in Democratic Republic of the Congo

## **Step 1 : Load packages (*done*)**

-   Load all packages required

## **Step 2 : Read in KC data (*done*)**

-   Define a path and find the latest Excel file (*done*)
-   Read in Excel sheets and clean the data (*done*)
-   Clean and summarize collection data (*done*)
-   Clean and summarize location data (*done*)
-   Join collection data and location data (*done*)
-   Preliminary Spatial visualization and quality check (*done*)

## **Step 3 : Prepare environmental covariates (*in progress*)**

-   Download environmental covariates (*done*) :
    -   Bioclimatic variables downloaded in geodata package using
        WorldClim function ✓  
    -   Landcover layers downloaded in geodata package using
        global\_landcover function ✓
-   Crop both covariates (climate and landcover) to the extend around KC
    (*not yet*)
-   Link both covariates (climate and landcover) to location data (*not
    yet*)
-   Plot the landcover \_crop (*not yet*)

## **Step 4 : Fit the model**

Binomial model for presence/absence data, or poisson model for count
data will be using (*not yet*)

## **Step 5 : Model Diagnostics Using DHARMa (*not yet*)**

## **Step 6 : Loading model and conducting env\_ (MESS) (*not yet*)**
