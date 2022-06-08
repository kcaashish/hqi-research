library(dplyr)
library(tidyr)

dependent <- readRDS("./data/processed/dependent.RDS")
independent_sep_region <- readRDS("./data/processed/independent_sep_region.RDS")
independent <- readRDS("./data/processed/independent_with_region.RDS")
hqi_data <- readRDS("./data/processed/hqi_data.RDS")

# national level equation ----
all_variables <- left_join(independent,
                           hqi_data %>% select(psu, hhld, HQI),
                           by = c("psu", "hhld"))

### fit full model ----
national_model <- lm(HQI ~ sex + age + caste + marital + can_read + can_write + ever_school + 
                       grade_comp + tec_voc_training + fam_size +
                       own_land_own + other_land_own + own_land_other + hhg_tot30 +
                       region, data = all_variables)
summary(national_model)

### stepwise regression model ----
national_model_f <- MASS::stepAIC(national_model, direction = "both", trace = FALSE)
national_model_f$anova
summary(national_model_f)


# himalaya region equation ----
him_variables <- left_join(
  independent %>% filter(region == "Himalaya") %>% select(-region),
  hqi_data %>% select(psu, hhld, HQI),
  by = c("psu", "hhld")
)

### fit full model ----
himalaya_model <- lm(HQI ~ sex + age + caste + marital + can_read + can_write + ever_school + 
                       grade_comp + tec_voc_training + fam_size +
                       own_land_own + other_land_own + own_land_other + hhg_tot30
                       , data = him_variables)
summary(himalaya_model)

### stepwise regression model ----
himalaya_model_f <- MASS::stepAIC(himalaya_model, direction = "both", trace = FALSE)
himalaya_model_f$anova
summary(himalaya_model_f)

# hill region equation ----
hill_variables <- left_join(
  independent %>% filter(region == "Hill") %>% select(-region),
  hqi_data %>% select(psu, hhld, HQI),
  by = c("psu", "hhld")
)

### fit full model ----
hill_model <- lm(HQI ~ sex + age + caste + marital + can_read + can_write + ever_school + 
                       grade_comp + tec_voc_training + fam_size +
                       own_land_own + other_land_own + own_land_other + hhg_tot30, data = hill_variables)
summary(hill_model)

### stepwise regression model ----
hill_model_f <- MASS::stepAIC(hill_model, direction = "both", trace = FALSE)
hill_model_f$anova
summary(hill_model_f)

# terai region equation ----
ter_variables <- left_join(
  independent %>% filter(region == "Terai") %>% select(-region),
  hqi_data %>% select(psu, hhld, HQI),
  by = c("psu", "hhld")
)

### fit full model ----
terai_model <- lm(HQI ~ sex + age + caste + marital + can_read + can_write + ever_school + 
                   grade_comp + tec_voc_training + fam_size +
                   own_land_own + other_land_own + own_land_other + hhg_tot30, data = ter_variables)
summary(terai_model)

### stepwise regression model ----
terai_model_f <- MASS::stepAIC(terai_model, direction = "both", trace = FALSE)
terai_model_f$anova
summary(terai_model_f)
