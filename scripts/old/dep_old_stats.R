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
old_hqiDT[, as.list(get_summary(HQI)), by = province]

# get quintile based percentage summary of HQI
dependent %>%
  mutate(
    approx_quintile = case_when(
      16 <= HQI & HQI <= 27 ~ "16-27",
      28 <= HQI & HQI <= 30 ~ "28-30",
      31 <= HQI & HQI <= 34 ~ "31-34",
      35 <= HQI & HQI <= 38 ~ "35-38",
      39 <= HQI & HQI <= 44 ~ "39-44"
    )
  ) %>%
  dplyr::group_by(approx_quintile) %>%
  dplyr::summarize(Percent = paste0(round(n() / nrow(dependent) * 100, 1), " %"))

# prepare provincial level hqi range data for ggplot ----
old_province_data <- old_hqiDT[, as.list(get_summary(HQI)), by = province] %>%
  mutate(
    "HQI Range" = case_when(
      16 <= Mean & Mean < 30 ~ "16-30",
      30 <= Mean & Mean < 33 ~ "30-33",
      33 <= Mean & Mean < 35 ~ "33-35",
      35 <= Mean & Mean < 39 ~ "35-39",
      39 <= Mean & Mean <= 45 ~ "39-45"
    )
  ) %>%
  mutate("HQI Range" = factor(
    `HQI Range`,
    levels = c("16-30", "30-33", "33-35", "35-39", "39-45")
  ))

# get descriptive stats for dependent variables ----
out <- lapply(names(dependent_vars)[-c(9:13)], function(x) get_freq(dependent_vars, x))

# save province data ----
saveRDS(old_province_data, "./data/old/processed/old_province_data.RDS")