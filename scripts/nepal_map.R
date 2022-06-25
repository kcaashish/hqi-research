library(sf)
library(gridpattern)
library(ggpattern)
# install.packages("magick")

# source in required scripts ----
source("./scripts/dependent_stats.R")

get_map <- function(df, year) {
  # Read shapefile into a dataframe, then merge provincial hqi data with the df
  p_data <- df
  nepal_map <-
    st_read("./data/hermes_NPL_new_wgs/hermes_NPL_new_wgs_1.shp")
  nepal_map2 <-
    merge(
      nepal_map,
      p_data[, c("province", "HQI Range")],
      by.x = "PROVINCE",
      by.y = "province",
      all.x = TRUE,
    )
  
  # Creating province wise distribution map of HQI
  # requires packages ggpattern and magick
  map <- ggplot(nepal_map2, aes(label = stringr::str_wrap(PR_NAME, 13))) +
    geom_sf() +
    geom_sf_pattern(
      aes(pattern_type = `HQI Range`),
      pattern = "magick",
      pattern_fill = "black",
      fill = "white",
      color = "black",
      pattern_scale = 1,
      pattern_key_scale_factor = 1.5
    ) +
    geom_sf_label(
      aes(fontface = "bold"),
      fun.geometry = sf::st_centroid,
      label.size = NA,
      label.padding = unit(0.05, "lines")
    ) +
    labs(title = paste("Housing quality distribution by province, Nepal, ", year), x = "", y = "") +
    scale_pattern_type_discrete(choices = c("horizontalsaw", "hs_cross", "hs_horizontal", "hs_bdiagonal", "crosshatch45"), drop = FALSE) +
    theme_bw() +
    theme(
      legend.key.size = unit(1.5, "cm"),
      legend.text = element_text(size = 12),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  
  return(map)
}

