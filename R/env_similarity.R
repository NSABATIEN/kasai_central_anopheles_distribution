  # checks and analyses
  
  # load in results and data
  
  source("R/packages.R")
  
  covs <- rast("data/clean/covariates.tif")
  
  cod_covs <- rast("data/clean/cod_covariates.tif")
  
  coord_covs <- read_csv(file = "data/clean/coords_covariates.csv") #seems that I don't have it, 
  
  #so I need to create it 
  prob_household_detection <- rast("output/spatial/prob_household_detection.tif")
  
  m <- readRDS(file = "output/model.Rds")
  
  coords <- read_csv("data/clean/kc_household_coords.csv",
                     col_types = cols(
                       health_zone = col_character(),
                       health_area = col_character(),
                       village = col_character(),
                       house_number = col_character(),
                       lat_dd = col_double(),
                       long_dd = col_double()
                     ))
  
  drc <- geodata::gadm("COD", level = 0)
  drc_l2 <- geodata::gadm("COD", level = 2)
  
  ## MESS
  # generate multivariate environmental similarity surface using
  # dismo, which works only with raster :eyeroll: 
  # per Elith et al. 2010
  # https://doi.org/10.1111/j.2041-210X.2010.00036.x
  
  #vic comment:  # Are the environments in my raster map across Kasaï-Central 
  #similar to the environments where I sampled mosquitoes?
  #the raster map across KC is my stack of environmental PCA layers
  #so MESS need to compare covraster (the environmental conditions everywhere across KC raster)
  #and coord_covs (environmental conditions at sampled households)
  #let do it
  # for my better understanding I found this paper 
  # http://www.malariajournal.com/content/13/1/213 
  #https://link.springer.com/article/10.1186/s13071-023-05912-z and 
  # I still continue to read it 
  covraster <- brick(covs)
  mess_drc <- mess(
    x = covraster,
    v = coord_covs |>
      dplyr::select(-health_zone, 
                    -health_area, 
                    -village, 
                    -house_number,
                    -lat_dd,
                    -long_dd,
                    -precision) |>
      as.data.frame()
  ) |>
    rast()
  
  plot(mess_drc)
  #this mess plot is checks if the environmental conditions in each pixel of my map 
  #are similar to the environmental conditions where my actually sampled mosquitoes
  #It loads my environmental covariate raster (PCA layers) for Kasaï-Central, 
  
  
  
  # plot with palette that diverges around zero
  mess_drc_plot <- mess_drc
  
  mess_drc_plot[is.infinite(values(mess_drc_plot))] <- NA
  
  mess_limit <- max(
    abs(values(mess_drc_plot)),
    na.rm = TRUE
  )
  
  plot_mess_local <- ggplot() +
    geom_spatraster(
      data = mess_drc_plot
    ) +
    scale_fill_distiller(
      type = "div",
      palette = "RdBu",
      direction = 1,
      limits = c(-mess_limit, mess_limit),
      na.value = "grey"
    ) +
    theme_void() +
    labs(fill = "Multivariate\nEnvironmental\nSimilarity")
  
  plot_mess_local
  
  # Save plot
  # ggsave(
  #   filename = "output/mess_local.png",
  #   plot = plot_mess_local,
  #   width = 7,
  #   height = 5,
  #   dpi = 300
  # )
  
  # plot again with village coords
  mess_drc_plot <- mess_drc
  mess_drc_plot[is.infinite(values(mess_drc_plot))] <- NA
  
  mess_limit <- max(
    abs(values(mess_drc_plot)),
    na.rm = TRUE
  )
  
  plot_mess_local_coords <- ggplot() +
    geom_spatraster(
      data = mess_drc_plot
    ) +
    scale_fill_distiller(
      type = "div",
      palette = "RdBu",
      direction = 1,
      limits = c(-mess_limit, mess_limit),
      na.value = "grey"
    ) +
    theme_void() +
    labs(fill = "Multivariate\nEnvironmental\nSimilarity")  +
    geom_point(
      data = coords,
      aes(
        x = long_dd,
        y = lat_dd
      ),
      col = "grey30"
    )
  
  plot_mess_local_coords
  
  # Save plot
  # ggsave(
  #   filename = "output/mess_local_coords.png",
  #   plot = plot_mess_local_coords,
  #   width = 7,
  #   height = 5,
  #   dpi = 300
  # )
  
  
  # make mask of this, such that anything < 0 is NA,
  # i.e. dissimilar, and >= 0 is 1, i.e., similar.
  #vic understanding 
  #only keep predictions in environments
  #similar to the mosquito sampling environments
  #Remove environmentally different areas
  
  mess_mask <- mess_drc
  
  mvals <- values(mess_drc)
  
  mess_mask[which(mvals < 0)] <- NA
  mess_mask[which(mvals >= 0)] <- 1
  plot(mess_mask)
  
  mess_mask <- terra::resample(
    mess_mask,
    prob_household_detection[[1]],
    method = "near"
  )
  plot(c(mess_mask, prob_household_detection[[1]]))
  
  
  ## plot probability of household detections under MESS mask
  #I need to check extends
  # Check extents
  ext(prob_household_detection)
  ext(mess_mask)
  
  mess_mask <- terra::resample(
    mess_mask,
    prob_household_detection,
    method = "near"
  )
  
  #now let take household detection probability map
  #crop it to the same extent as the MESS mask
  #remove areas where MESS < 0
  #So the result keeps predictions only where environments are similar to sampled households
  
  prob_household_detection_mess <- prob_household_detection |>
    mask(mess_mask)
  
  ##then Create a map of household mosquito detection probability
  #only in environmentally reliable areas
  plot_hhdet_mess_local <- ggplot() +
    geom_spatraster(data = prob_household_detection_mess) +
    facet_wrap(~lyr) +
    scale_fill_viridis_c(
      option = "G",
      na.value = "white"
    ) +
    theme_void()  +
    labs(
      title = "Probability of detection in locations environmentally similar to sample sites",
      fill = "Household\nprobability\nof detection"
    )
  
  plot_hhdet_mess_local
  
  # ggsave(
  #   filename = "output/plot_hhdet_mess_local.png",
  #   plot = plot_hhdet_mess_local
  # )
  
  
  plot_hhdet_mess_local_coords <- ggplot() +
    geom_spatraster(data = prob_household_detection_mess) +
    facet_wrap(~lyr) +
    scale_fill_distiller(
      palette = "Blues",
      direction = 1,
      na.value = "grey90"
    ) +
    theme_void() +
    geom_point(
      data = coords,
      aes(
        x = long_dd,
        y = lat_dd
      ),
      col = "hotpink"
    ) +
    labs(
      title = "Probability of detection in locations environmentally similar to sample sites",
      fill = "Household\nprobability\nof detection",
      caption = "Pink points are sample villages"
    )
  
  plot_hhdet_mess_local_coords
  
  # ggsave(
  #   filename = "output/plot_hhdet_mess_local_coords.png",
  #   plot = plot_hhdet_mess_local_coords
  # )
  
  # predict over whole DRC
  cod_cov_species_dummy <- cod_covs[[1]] * 0
  names(cod_cov_species_dummy) <- "species"
  
  cod_cov_gambiae  <- 
  cod_cov_funestus <- 
  cod_cov_paludis  <- 
  cod_cov_hancocki <- 
  cod_cov_sp       <- 
  cod_cov_moucheti <- 
  cod_cov_ziemanni <- cod_cov_species_dummy
  
  cod_cov_gambiae[]  <- "An. gambiae s.l."
  cod_cov_funestus[] <- "An. funestus gp"
  cod_cov_paludis[]  <- "An. paludis"
  cod_cov_hancocki[] <- "An. hancocki"
  cod_cov_sp[]       <- "An. sp"
  cod_cov_moucheti[] <- "An. moucheti"
  cod_cov_ziemanni[] <- "An. ziemanni"
  
  cod_pred_gambiae <- predict(c(cod_covs, cod_cov_gambiae),
                              m,
                              type = "response")
  
  cod_pred_funestus <- predict(c(cod_covs, cod_cov_funestus),
                               m,
                               type = "response")
  
  cod_pred_paludis <- predict(c(cod_covs, cod_cov_paludis),
                              m,
                              type = "response")
  
  cod_pred_hancocki <- predict(c(cod_covs, cod_cov_hancocki),
                               m,
                               type = "response")
  
  cod_pred_sp <- predict(c(cod_covs, cod_cov_sp),
                         m,
                         type = "response")
  
  cod_pred_moucheti <- predict(c(cod_covs, cod_cov_moucheti),
                               m,
                               type = "response")
  
  cod_pred_ziemanni <- predict(c(cod_covs, cod_cov_ziemanni),
                               m,
                               type = "response")
  
  names(cod_pred_gambiae)  <- "An. gambiae s.l."
  names(cod_pred_funestus) <- "An. funestus gp"
  names(cod_pred_paludis)  <- "An. paludis"
  names(cod_pred_hancocki) <- "An. hancocki"
  names(cod_pred_sp)       <- "An. sp"
  names(cod_pred_moucheti) <- "An. moucheti"
  names(cod_pred_ziemanni) <- "An. ziemanni"
  
  
  cod_preds <- c(cod_pred_gambiae,
             cod_pred_funestus,
             cod_pred_paludis,
             cod_pred_hancocki,
             cod_pred_sp,
             cod_pred_moucheti,
             cod_pred_ziemanni)
  
  # compute the probability of detecting each species in a given household
  # 25 households were sampled per health zone/site
  #So the output will be 0 = very unlikely to detect the interest mosquito specie
  # 1 = very likely to detect the interest mosquito specie
  
  cod_prob_household_detection <- 1 - exp(-cod_preds/25)
  
  plot_hhdet_cod <- ggplot() +
    geom_spatraster(data = cod_prob_household_detection) +
    facet_wrap(~lyr, ncol = 2) +
    scale_fill_viridis_c(
      option = "G",
      na.value = "white"
    ) +
    theme_void() +
    labs(
      title = "Probability of detection in environnement similar locations",
      fill = "Household\nprobability\nof detection",
    )
  plot_hhdet_cod
  
  # ggsave(
  #   filename = "output/plot_hhdet_cod.png",
  #   plot = plot_hhdet_cod,
  #   width = 8,
  #   height = 6,
  #   dpi = 300
  # )
  
  
  
  # compute the probability of detecting each species in a given household
  # 25 households were sampled per health zone/site
  ## plot probability of household detections under MESS mask
  # 
  # prob_household_detection_kc <- prob_household_detection |>
  #   crop(mess_mask) |>
  #   mask(mess_mask)
  # 
  # prob_household_detection_mess <- prob_household_detection_kc |>
  #   mask(mess_mask)
  # 
  # expected_pre_household <- preds / 25
  # 
  # prob_household_detection <- 1 - exp(-expected_pre_household)
  # 
  # 
  # plot_hhdet_local <- ggplot() +
  #   geom_spatraster(data = prob_household_detection) +
  #   facet_wrap(~lyr, ncol = 3) +
  #   scale_fill_viridis_c(
  #     option = "G",
  #     limits = c(0, 1),
  #     na.value = "white"
  #   ) +
  #   theme_void() +
  #   geom_point(
  #     data = coords,
  #     aes(
  #       x = long_dd,
  #       y = lat_dd
  #     ),
  #     col = "hotpink",
  #     size = 0.8
  #   ) +
  #   labs(
  #     title = "Probability of detection in Kasaï-Central",
  #     fill = "Household\nprobability\nof detection",
  #     caption = "Pink points are sampled households"
  #   )
  # 
  # plot_hhdet_local
  
  # drc <- geodata::gadm("COD", level = 0)
  # drc_l2 <- geodata::gadm("COD", level = 2)
  # 
  # par(mfrow = c(3, 3))
  # 
  # for (i in 1:nlyr(prob_household_detection)) {
  #   plot(prob_household_detection[[i]],
  #        range = c(0, 1),
  #        main = names(prob_household_detection)[i])
  #   
  #   plot(drc_l2, add = TRUE, bg = grey(0.5))
  #   plot(drc, lwd = 2, add = TRUE)
  #   
  #   points(coords$lat_dd ~ coords$long_dd,
  #          pch = 21,
  #          bg = "blue",
  #          cex = 1)
  # }
  
  ## National scale MESS
  cod_covraster <- brick(cod_covs)
  
  mess_cod <- mess(
    x = cod_covraster,
    v = coord_covs |>
      dplyr::select(
        -health_zone,
        -health_area,
        -village,
        -house_number,
        -lat_dd,
        -long_dd,
        -precision
      ) |>
      as.data.frame()
  ) |>
    rast() |>
    mask(cod_covs[[1]])
  
  plot(mess_cod)
  
  
  # plot with palette that diverges around zero
  
  # generate plot limits for palette divergence
  # something creating inf max value, but min real value is greatest absolute
  # so using that instead
  cod_mess_limit <- abs(min(values(mess_cod), na.rm = TRUE)) * c(-1, 1)
  
  plot_mess_cod <- ggplot() +
    geom_spatraster(
      data = mess_cod
    ) +
    scale_fill_distiller(
      type = "div",
      palette = "RdBu",
      direction = 1,
      limit = cod_mess_limit,
      na.value = "transparent"
    ) +
    theme_void() +
    labs(fill = "Multivariate\nEnvironmental\nSimilarity")
  
  plot_mess_cod
  
  # ggsave(
  #   filename = "output/mess_cod.png",
  #   plot = plot_mess_cod,
  #   width = 8,
  #   height = 8,
  #   dpi = 300
  # )
  
  # plot again with village coords
  plot_mess_cod_coords <- ggplot() +
    geom_spatraster(
      data = mess_cod
    ) +
    scale_fill_distiller(
      type = "div",
      palette = "RdBu",
      direction = 1,
      limit = cod_mess_limit,
      na.value = "transparent"
    ) +
    theme_void() +
    labs(fill = "Multivariate\nEnvironmental\nSimilarity")  +
    geom_point(
      data = coords,
      aes(
        x = long_dd,
        y = lat_dd
      ),
      col = "grey30"
    )
  
  plot_mess_cod_coords
  
  # ggsave(
  #   filename = "output/mess_cod_coords.png",
  #   plot = plot_mess_cod_coords,
  #   width = 8,
  #     height = 8,
  #     dpi = 300
  # )
  
  # make mask of this, such that anything < 0 is NA,
  # i.e. dissimilar, and >= 0 is 1, i.e., similar.
  mess_mask_cod <- mess_cod
  
  mvals_cod <- values(mess_cod)
  
  naidx_cod <- which(is.na(mvals_cod))
  
  naidx_cod_mess <- c(naidx_cod, which(mvals_cod < 0))
  mess_mask_cod[naidx_cod_mess] <- NA
  mess_mask_cod[!naidx_cod_mess] <- 1
  
  plot(mess_mask_cod)
  
  plot(c(mess_mask_cod, cod_prob_household_detection[[1]]))
  
  cod_prob_household_detection_mess <- cod_prob_household_detection |>
    mask(mess_mask_cod)
  
  cod_prob_household_detection <- writeRaster(
    cod_prob_household_detection,
    "cod_prob_household_detection.tif"
  )
  
  
  mess_mask_cod <- writeRaster(
    mess_mask_cod,
    "mess_mask_cod.tif"
  )
  
  plot_hhdet_mess_cod <- ggplot() +
    geom_spatraster(data = cod_prob_household_detection_mess) +
    facet_wrap(~lyr) +
    scale_fill_viridis_c(
      option = "G",
      na.value = "white"
    ) +
    theme_void()  +
    labs(
      title = "Probability of detection in locations environmentally similar to sample sites",
      fill = "Household\nprobability\nof detection"
    ) +
    geom_spatvector(
      data = drc,
      fill = NA,
      colour = "grey80"
    )
  plot_hhdet_mess_cod
  
  # ggsave(
  #   filename = "output/plot_hhdet_mess_cod.png",
  #   plot = plot_hhdet_mess_cod,
  #   width = 8,
  #   height = 8,
  #   dpi = 300
  # )
  
  plot_hhdet_mess_cod_coords <- ggplot() +
    geom_spatraster(data = cod_prob_household_detection_mess) +
    facet_wrap(~lyr) +
    scale_fill_viridis_c(
      option = "G",
      na.value = "white"
    ) +
    theme_void() +
    geom_point(
      data = coords,
      aes(
        x = long_dd,
        y = lat_dd
      ),
      col = "hotpink",
      size = 0.5
    ) +
    labs(
      title = "Probability of detection in locations environmentally similar to sample sites",
      fill = "Household\nprobability\nof detection",
      caption = "Pink points are sample villages"
    ) +
    geom_spatvector(
      data = drc,
      fill = NA,
      colour = "grey80"
    )
  
  plot_hhdet_mess_cod_coords
  
  
  # ggsave(
  #   filename = "output/plot_hhdet_mess_cod_coords.png",
  #   plot = plot_hhdet_mess_cod_coords,
  #   width = 8,
  #   height = 8,
  #   dpi = 300
  # )
  
  
  ##Plots much better 
  ggplot() +
    geom_spatraster(data = cod_prob_household_detection) +
    facet_wrap(~lyr, ncol = 2) +
    scale_fill_gradient(
      low = "hotpink",
      high = "firebrick",
      na.value = "white"
    ) +
    theme_void() +
    labs(
      title = "Probability of detection",
      fill = "Household\nprobability\nof detection",
    )
  
  
  mcvals <- values(mess_cod)
  
  cod_log_dissimilarity <- mess_cod
  cod_log_dissimilarity <- -cod_log_dissimilarity
  cod_log_dissimilarity <- cod_log_dissimilarity - global(cod_log_dissimilarity, min, na.rm = TRUE)[1,1]
  cod_log_dissimilarity <- mask(cod_log_dissimilarity, mess_cod)
  cod_log_dissimilarity <- log10(cod_log_dissimilarity)
  
  cod_log_dissimilarity <- writeRaster(
    cod_log_dissimilarity,
    "cod_log_dissimilarity.tif",
    overwrite = TRUE
  )
  
  
  ggplot() +
    geom_spatraster(
      data = cod_log_dissimilarity
    ) +
    scale_fill_gradient(
      low = "grey99",
      high = "grey20",
      na.value = "white"
    ) +
    theme_void() +
    geom_point(
      data = coords,
      aes(
        x = long_dd,
        y = lat_dd
      ),
      col = "hotpink",
      size = 0.5
    ) +
    labs(fill = "Multivariate\nEnvironmental\nDissimilarity")  #+
  # geom_spatvector(
  #   data = drc,
  #   fill = NA,
  #   colour = "black"
  # )
  
  
  # mess_drc_clean <- mess_drc
  # 
  # mess_drc_clean[
  #   is.infinite(values(mess_drc_clean))
  # ] <- NA
  # 
  # dissimilarity_local <- -mess_drc_clean
  # 
  # dissimilarity_local <- dissimilarity_local -
  #   global(dissimilarity_local, min, na.rm = TRUE)[1, 1]
  # 
  # dissimilarity_local <- log10(dissimilarity_local + 1)

  ggplot() +
    geom_spatraster(
      data = dissimilarity_local
    ) +
    scale_fill_gradient(
      low = "grey90",
      high = "grey30",
      na.value = "white",
      limits = c(0, 3)
    ) +
    theme_void() +
    geom_point(
      data = coords,
      aes(
        x = long_dd,
        y = lat_dd
      ),
      col = "hotpink",
      size = 1
    ) +
    labs(
      fill = "Multivariate\nEnvironmental\nDissimilarity"
    )

  
  
  ggplot() +
  geom_spatraster(data = cod_prob_household_detection_mess) +
  facet_wrap(~lyr) +
  scale_fill_gradient(
    low = "pink",
    high = "firebrick",
    na.value = "grey90"
  ) +
  theme_void()  +
  labs(
    title = "Probability of detection in locations environmentally similar to sample sites",
    fill = "Household\nprobability\nof detection"
  ) +
  geom_spatvector(
    data = drc,
    fill = NA,
    colour = "black"
  )



ggplot() +
  geom_spatraster(data = prob_household_detection_mess) +
  facet_wrap(~lyr) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    na.value = "grey90"
  ) +
  theme_void()  +
  labs(
    title = "Probability of detection in locations environmentally similar to sample sites",
    fill = "Household\nprobability\nof detection"
  ) +
  geom_spatvector(
    data = crop(drc_l2, prob_household_detection_mess),
    fill = NA,
    colour = "black"
  ) +
  geom_spatvector(
    data = crop(drc, prob_household_detection_mess),
    fill = NA,
    colour = "black"
  )  +
  geom_point(
    data = coords,
    aes(
      x = long_dd,
      y = lat_dd
    ),
    fill = "hotpink",
    col = "hotpink",
    size = 1
  )







# par(mfrow = c(2, 2))
# for (i in 1:4) {
#   plot(drc, lwd = 2)
#   plot(prob_household_detection[[i]],
#        range = c(0, 1),
#        main = names(prob_household_detection)[i], add = TRUE)
#   plot(drc_l2, add = TRUE, bg = grey(0.5))
#   plot(drc, lwd = 2, add = TRUE)
#   points(coords$lat_dd ~ coords$long_dd,
#          pch = 21,
#          bg = "blue",
#          cex = 1)
# }
# 
# 
# write_csv(
#   coord_covs,
#   file = "data/clean/coords_covariates.csv"
# )
# 
# writeRaster(
#   prob_household_detection,
#   "output/spatial/prob_household_detection.tif",
#   overwrite = TRUE
# )
# 
# saveRDS(
#   m,
#   file = "output/model.Rds"
# )
