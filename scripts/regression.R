library(dplyr)
library(tidyr)

dependent <- readRDS("./data/processed/dependent.RDS")
independent <- readRDS("./data/processed/independent.RDS")
independent_no <- readRDS("./data/processed/independent_no_region.RDS")
hqi_data <- readRDS("./data/processed/hqi_data.RDS")

# national level equation ----
all_variables <- left_join(independent_no,
                           hqi_data %>% select(psu, hhld, HQI),
                           by = c("psu", "hhld"))

### fit full model ----
national_model <- lm(HQI ~ sex + age + caste + marital + can_read + can_write + ever_school + 
                       grade_comp + tec_voc_training + fam_size +
                       own_land_own + other_land_own + own_land_other + hhg_tot30 +
                       region, data = all_variables)
summary(national_model)

### stepwise regression model ----
national_step <- MASS::stepAIC(national_model, direction = "both", trace = FALSE)
national_step$anova
summary(national_step)


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
