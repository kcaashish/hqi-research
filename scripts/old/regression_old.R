library(dplyr)
library(tidyr)
library(labelled)
# WARNING: loading MASS library conflicts with select from dplyr

independent <- readRDS("./data/old/processed/independent.RDS")
hqi_data <- readRDS("./data/old/processed/dep_old.RDS")

# convert some numeric data to factors as needed ----
independent <- to_factor(independent)

# national level equation ----
all_variables <- left_join(independent,
                           hqi_data %>% select(psu, hhld, HQI),
                           by = c("psu", "hhld"))

### fit full model ----
national_model <-
  lm(
    HQI ~ sex + age + caste + marital + can_read + can_write + ever_school +
      grade_comp + fam_size + agriculture_land + region,
    data = all_variables
  )
summary(national_model)

### stepwise regression model ----
national_model_f <-
  MASS::stepAIC(national_model, direction = "both", trace = FALSE)
national_model_f$anova
summary(national_model_f)


# himalaya region equation ----
him_variables <- left_join(
  independent %>% filter(region == "Himalaya") %>% select(-region),
  hqi_data %>% select(psu, hhld, HQI),
  by = c("psu", "hhld")
)

### fit full model ----
himalaya_model <-
  lm(
    HQI ~ sex + age + caste + marital + can_read + can_write + ever_school +
      grade_comp + fam_size + agriculture_land,
    data = him_variables
  )
summary(himalaya_model)

### stepwise regression model ----
himalaya_model_f <-
  MASS::stepAIC(himalaya_model, direction = "both", trace = FALSE)
himalaya_model_f$anova
summary(himalaya_model_f)

# hill region equation ----
hill_variables <- left_join(
  independent %>% filter(region == "Hill") %>% select(-region),
  hqi_data %>% select(psu, hhld, HQI),
  by = c("psu", "hhld")
)

### fit full model ----
hill_model <-
  lm(
    HQI ~ sex + age + caste + marital + can_read + can_write + ever_school +
      grade_comp + fam_size + agriculture_land,
    data = hill_variables
  )
summary(hill_model)

### stepwise regression model ----
hill_model_f <-
  MASS::stepAIC(hill_model, direction = "both", trace = FALSE)
hill_model_f$anova
summary(hill_model_f)

# terai region equation ----
ter_variables <- left_join(
  independent %>% filter(region == "Terai") %>% select(-region),
  hqi_data %>% select(psu, hhld, HQI),
  by = c("psu", "hhld")
)

### fit full model ----
terai_model <-
  lm(
    HQI ~ sex + age + caste + marital + can_read + can_write + ever_school +
      grade_comp + fam_size + agriculture_land,
    data = ter_variables
  )
summary(terai_model)

### stepwise regression model ----
terai_model_f <-
  MASS::stepAIC(terai_model, direction = "both", trace = FALSE)
terai_model_f$anova
summary(terai_model_f)
