library(dplyr)
library(tidyr)

# sourcing the required scripts ----
source("./functions/stats_helpers.R")
independent_var <- readRDS("./data/processed/independent.RDS")

# seperate marital status into 5 different variables ----
independent_var_separate_mar <- independent_var %>% 
  mutate(
    never_married = if_else(marital == "Never married", 1, 0),
    married = if_else(marital == "Married", 1, 0),
    widow = if_else(marital == "Widow/widower", 1, 0),
    separated = if_else(marital == "Separated", 1, 0),
    divorced = if_else(marital == "Divorced", 1, 0)
  ) %>% 
  select(!marital)

# get stats ----
region_separated_total <- get_region_separated_stats(independent_var_separate_mar)
region_included_total <- get_region_included_stats(independent_var_separate_mar)

# save into csvs ----
write.csv(region_separated_total, file = "./output/regional_stats.csv", quote = FALSE)
write.csv(region_included_total, file = "./output/national_stats.csv", quote = FALSE)
