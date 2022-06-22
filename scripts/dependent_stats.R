library(dplyr)
library(data.table)

# source in the required scripts ----
source("./scripts/dependent.R")
source("./scripts/helpers.R")

# national level stats ----
print("The national level stats:")
dep_num_rev %>%
  mutate(HQI = rowSums(.[3:10])) %>%
  arrange(desc(HQI)) %>%
  summarise(
    min = min(HQI),
    max = max(HQI),
    mean = mean(HQI),
    sd = sd(HQI)
  )

# get HQI as sum of vars & add region data ----
hqi_for_sum <- dep_num_rev %>%
  mutate(HQI = rowSums(.[3:10])) %>%
  left_join(
    dplyr::select(s00, psu, hhld, region = location_region_HIMALAYAS_HILLS_TERAI, province = location_province),
    by = c("psu", "hhld")
  ) %>%
  mutate(region = factor(region, labels = c("Himalaya", "Hill", "Terai"))) %>%
  mutate(quintile = ntile(HQI, 5))

hqiDT <- as.data.table(hqi_for_sum)
hqiDT[, as.list(get_summary(HQI))]
hqiDT[, as.list(get_summary(HQI)), by = region]
province_data <- hqiDT[, as.list(get_summary(HQI)), by = province]
hqiDT[, as.list(get_summary(HQI)), by = quintile]

# get quintile based percentage summary of HQI
hqi_for_sum %>%
  mutate(
    approx_quintile = case_when(
      17 <= HQI & HQI <= 29 ~ "17-29",
      30 <= HQI & HQI <= 32 ~ "30-32",
      33 <= HQI & HQI <= 34 ~ "33-34",
      35 <= HQI & HQI <= 38 ~ "35-38",
      39 <= HQI & HQI <= 45 ~ "39-45"
    )
  ) %>%
  dplyr::group_by(approx_quintile) %>%
  dplyr::summarize(Percent = paste0(round(n() / nrow(hqi_for_sum) * 100, 1), " %"))

# prepare provincial level hqi range data for ggplot
province_data <- province_data %>%
  mutate(
    "HQI Range" = case_when(
      17 <= Mean & Mean < 30 ~ "17-30",
      30 <= Mean & Mean < 33 ~ "30-33",
      33 <= Mean & Mean < 35 ~ "33-35",
      35 <= Mean & Mean < 39 ~ "35-39",
      39 <= Mean & Mean <= 45 ~ "39-45"
    )
  ) %>%
  mutate("HQI Range" = factor(
    `HQI Range`,
    levels = c("17-30", "30-33", "33-35", "35-39", "39-45")
  ))

# save hqi_for_sum dataset ----
saveRDS(hqi_for_sum, file = "./data/processed/hqi_data.RDS")
