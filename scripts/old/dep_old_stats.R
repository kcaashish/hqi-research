library(dplyr)
library(plyr)
library(data.table)

# source the required scripts ----
source("./functions/helpers.R")

# import the required data for statistics ----
dependent <- readRDS("./data/old/processed/dep_old.RDS")
dependent_vars <- readRDS("./data/old/processed/dep_old_vars.RDS")

# get HQI distribution ----
dependent <- dependent %>% 
  mutate(quintile = ntile(HQI, 5))
old_hqiDT <- as.data.table(dependent)
old_hqiDT[, as.list(get_summary(HQI))]
old_hqiDT[, as.list(get_summary(HQI)), by = region]
old_hqiDT[, as.list(get_summary(HQI)), by = quintile]

# get quintile based percentage summary of HQI
dependent %>%
  mutate(
    approx_quintile = case_when(
      15 <= HQI & HQI <= 26 ~ "15-26",
      27 <= HQI & HQI <= 30 ~ "27-30",
      31 <= HQI & HQI <= 33 ~ "31-33",
      34 <= HQI & HQI <= 38 ~ "34-38",
      39 <= HQI & HQI <= 44 ~ "39-44"
    )
  ) %>%
  dplyr::group_by(approx_quintile) %>%
  dplyr::summarize(Percent = paste0(round(n() / nrow(dependent) * 100, 1), " %"))

# get descriptive stats for dependent variables ----
out <- lapply(names(dependent_vars)[-c(9,10)], function(x) get_freq(dependent_vars, x))
