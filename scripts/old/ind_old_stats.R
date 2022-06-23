library(dplyr)
library(tidyr)

options(scipen = 999)

# source the required scripts / RDS files
source("./scripts/stats_helpers.R")
independent <- readRDS("./data/old/processed/independent.RDS")

# separate marital status into 5 different variables ----
ind_fac <- independent %>% 
  mutate(
    never_married = if_else(marital == 1, 1, 0),
    single_married = if_else(marital == 2, 1, 0), 
    poly_married = if_else(marital == 3, 1, 0),
    re_married = if_else(marital == 4, 1, 0),
    widow_widower = if_else(marital == 5, 1, 0),
    divorced = if_else(marital == 6, 1, 0),
    separated = if_else(marital == 7, 1, 0)
  ) %>% 
  select(!marital)

# get stats ----
region_separted_total <- get_region_separated_stats(ind_fac)
region_included_total <- get_region_included_stats(ind_fac)

# save into csvs ----
write.csv(region_separted_total, file = "./output/region_separated_old.csv", quote = FALSE)
write.csv(region_included_total, file = "./output/region_included_old.csv", quote = FALSE)

