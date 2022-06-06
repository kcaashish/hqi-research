library(sf)
library(gridpattern)
library(ggpattern)
# install.packages("magick")

# source in required scripts ----
source("./scripts/hqi_stats.R")

# Read shapefile into a dataframe, then merge provincial hqi data with the df
nepal_map <- st_read("./data/hermes_NPL_new_wgs/hermes_NPL_new_wgs_1.shp")
nepal_map2 <- merge(nepal_map, province_data[, c("province", "HQI Range")], 
                    by.x = "PROVINCE", by.y = "province", all.x = TRUE, )

# Creating province wise distribution map of HQI
# requires packages ggpattern and magick
ggplot(nepal_map2, aes(label = stringr::str_wrap(PR_NAME, 13))) +
  geom_sf() +
  geom_sf_pattern(
    aes(pattern_type = `HQI Range`),
    pattern = "magick",
    pattern_fill = "black",
    fill = "white",
    color = "black",
    pattern_density = 0.4
  ) +
  geom_sf_label(fun.geometry = sf::st_centroid, label.size = NA, label.padding = unit(0.05, "lines")) +
  labs(title = "Housing quality distribution by province, Nepal, 2018", x = "", y = "") +
  scale_pattern_type_discrete(choices = gridpattern::names_magick) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )