# fit a negative binomial random effects model of the abundance of the different
# taxa, regressed against gridded environmental covariates. Use hierarchical terms to
# capture shared responses (shared environmental drivers)

source("R/packages.R")

dir.create("output", showWarnings = FALSE)
dir.create("output/spatial", recursive = TRUE, showWarnings = FALSE)

# load the ento data, make species a factor
counts <- read_csv("data/clean/kc_household_counts.csv",
                   col_types = cols(
                     health_zone = col_character(),
                     health_area = col_character(),
                     village = col_character(),
                     house_number = col_character(),
                     species = col_character(),
                     count = col_double()
                   ))

all_species <- counts %>%
  distinct(species) %>%
  pull(species)

# counts <- counts %>%
#   tidyr::complete(
#     health_zone,
#     health_area,
#     village,
#     house_number,
#     species = all_species,
#     fill = list(count = 0)
#   ) %>%
#   mutate(
#     species = factor(species)
#   )

counts <- read_csv("data/clean/kc_household_counts_from_species_sheet.csv",
                   col_types = cols(
                     health_zone = col_character(),
                     health_area = col_character(),
                     village = col_character(),
                     house_number = col_character(),
                     species = col_character(),
                     count = col_double()
                   )) |>
  mutate(
    species = factor(species)
  )

write_csv(
  counts,
  "data/clean/kc_household_counts_complete.csv"
)

coords <- read_csv("data/clean/kc_households_coords.csv",
                   col_types = cols(
                     health_zone = col_character(),
                     health_area = col_character(),
                     village = col_character(),
                     house_number = col_character(),
                     lat_dd = col_double(),
                     long_dd = col_double()
                   )) 

# load the spatial covariate data (already scaled to data locations)
covs <- rast("data/clean/covariates.tif")

# extract scaled covariate data at observation locations, for modelling
coord_covs <- coords %>%
  bind_cols(
    terra::extract(covs,
                   dplyr::select(coords,
                          long_dd,
                          lat_dd),
                   ID = FALSE
    )
  ) %>%
  dplyr::select(-lat_dd, -long_dd)

# combine with the count data to get dataframe for modelling
df <- counts %>%
  left_join(
    coord_covs,
    by = c("health_zone",
           "health_area",
           "village",
           "house_number")
  )

# model using a GAM
m <- gam(
  count ~
    # separate intercept (average abundance) for each species
    1 + species + 
    # shared bioclim terms
    s(bioclim_pc1) +
    s(bioclim_pc2) +
    s(bioclim_pc3) +
    s(bioclim_pc4) +
    # shared landcover terms
    s(landcover_pc1) +
    s(landcover_pc2) +
    s(landcover_pc3) +
    s(landcover_pc4) +
    s(landcover_pc5) +
    # species-specific bioclim terms
    s(bioclim_pc1, species, bs = "re") +
    s(bioclim_pc2, species, bs = "re") +
    s(bioclim_pc3, species, bs = "re") +
    s(bioclim_pc4, species, bs = "re") +
    # species-specific landcover terms
    s(landcover_pc1, species, bs = "re") +
    s(landcover_pc2, species, bs = "re") +
    s(landcover_pc3, species, bs = "re") +
    s(landcover_pc4, species, bs = "re") +
    s(landcover_pc5, species, bs = "re"),
  # negative binomial observation model, with estimation of the theta parameter
  family = nb,
  # fit by REML (more trustworthy than GCV)
  method = "REML",
  data = df
)
summary(m)
plot(m)

# make prediction maps
# unique(df$species)
cov_species_dummy <- covs[[1]] * 0
names(cov_species_dummy) <- "species"

cov_gambiae <-
  cov_funestus <- 
  cov_paludis <-
  cov_hancocki <-
  cov_sp <-
  cov_moucheti <-
  cov_ziemanni <- cov_species_dummy

cov_gambiae[] <- "An. gambiae s.l."
cov_funestus[] <- "An. funestus gp"
cov_paludis[] <- "An. paludis"
cov_hancocki[] <- "An. hancocki"
cov_sp[] <- "An. sp"
cov_moucheti[] <- "An. moucheti"
cov_ziemanni[] <- "An. ziemanni"

pred_gambiae <- predict(c(covs, cov_gambiae),
                        m,
                        type = "response")
pred_funestus <- predict(c(covs, cov_funestus),
                         m,
                         type = "response")
pred_paludis <- predict(c(covs, cov_paludis),
                        m,
                        type = "response")
pred_hancocki <- predict(c(covs, cov_hancocki),
                         m,
                         type = "response")
pred_sp <- predict(c(covs, cov_sp),
                   m,
                   type = "response")
pred_moucheti <- predict(c(covs, cov_moucheti),
                         m,
                         type = "response")
pred_ziemanni <- predict(c(covs, cov_ziemanni),
                         m,
                         type = "response")

names(pred_gambiae) <- "An. gambiae s.l."
names(pred_funestus) <- "An. funestus gp"
names(pred_paludis) <- "An. paludis"
names(pred_hancocki) <- "An. hancocki"
names(pred_sp) <- "An. sp"
names(pred_moucheti) <- "An. moucheti"
names(pred_ziemanni) <- "An. ziemanni"

preds <- c(pred_gambiae,
           pred_funestus,
           pred_paludis,
           pred_hancocki,
           pred_sp,
           pred_moucheti,
           pred_ziemanni)

# compute the probability of detecting each species in a given household
expected_pre_household <- preds/25
prob_household_detection <- 1 - exp(-expected_pre_household)

drc <- geodata::gadm("COD", level = 0)
drc_l2 <- geodata::gadm("COD", level = 2)

par(mfrow = c(3, 3))
for (i in 1:nlyr(prob_household_detection)) {
  plot(prob_household_detection[[i]],
       range = c(0, 1),
       main = names(prob_household_detection)[i])
  plot(drc_l2, add = TRUE, bg = grey(0.5))
  plot(drc, lwd = 2, add = TRUE)
  points(coords$lat_dd ~ coords$long_dd,
         pch = 21,
         bg = "blue",
         cex = 1)
}

write_csv(
  coord_covs,
  file = "data/clean/coords_covariates.csv"
)

writeRaster(
  prob_household_detection,
  "output/spatial/prob_household_detection.tif",
  overwrite = TRUE
)

saveRDS(
  m,
  file = "output/model.Rds"
)
