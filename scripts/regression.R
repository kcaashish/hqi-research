library(dplyr)
library(tidyr)

dependent <- readRDS("./data/processed/dependent.RDS")
independent <- readRDS("./data/processed/independent.RDS")
hqi_data <- readRDS("./data/processed/hqi_data.RDS")

# national level equation ----
all_variables <- left_join(independent,
                           hqi_data %>% select(psu, hhld, HQI),
                           by = c("psu", "hhld"))
national_model <- lm(HQI ~ . - psu - hhld, data = all_variables)
summary(national_model)

# himalaya region equation ----
him_variables <- left_join(
  independent %>% filter(himalaya == 1) %>% select(-c(himalaya, hill, terai)),
  hqi_data %>% select(psu, hhld, HQI),
  by = c("psu", "hhld")
)
himalaya_model <- lm(HQI ~ . - psu - hhld, data = him_variables)
summary(himalaya_model)

# hill region equation ----
hill_variables <- left_join(
  independent %>% filter(hill == 1) %>% select(-c(himalaya, hill, terai)),
  hqi_data %>% select(psu, hhld, HQI),
  by = c("psu", "hhld")
)
hill_model <- lm(HQI ~ . - psu - hhld, data = hill_variables)
summary(hill_model)

# terai region equation ----
ter_variables <- left_join(
  independent %>% filter(terai == 1) %>% select(-c(himalaya, hill, terai)),
  hqi_data %>% select(psu, hhld, HQI),
  by = c("psu", "hhld")
)
terai_model <- lm(HQI ~ . - psu - hhld, data = ter_variables)
summary(terai_model)
