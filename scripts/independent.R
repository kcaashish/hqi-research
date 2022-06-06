library(dplyr)
library(tidyr)

# sourcing all the required scripts ----
source("./scripts/imports.R")

# Collecting all the independent variables ----
income_s06 <- s06 %>% 
  filter(id == 1) %>% 
  mutate(
    income_mth = case_when(
      prd_remu == "Monthly" ~ as.numeric(amt_cashrs),
      prd_remu == "Weekyly" ~ amt_cashrs * 4,
      prd_remu == "Daily" ~ amt_cashrs * 30
    )
  )

raw_independent <- s02 %>% 
  filter(id == 1) %>% 
  dplyr::select(psu, hhld, sex, age, caste, marital, can_read, can_write, ever_school, grade_comp, tec_voc_training) %>%
  left_join(
    dplyr::summarise(group_by(s02, psu, hhld), fam_size = n()),
    by = c("psu", "hhld")
  ) %>% 
  # left_join(
  #   dplyr::summarise(group_by(s12, psu, hhld), absent_num = n()),
  #   by = c("psu", "hhld")
  # ) %>% 
  # replace_na(list(absent_num = 0)) %>% 
  left_join(
    dplyr::select(s00, psu, hhld, region = location_region_HIMALAYAS_HILLS_TERAI),
    by = c("psu", "hhld")
  ) %>% 
  mutate(region = factor(region, labels = c("Himalaya", "Hill", "Terai")),) %>%
  left_join(
    dplyr::select(s01, psu, hhld, own_land_own, other_land_own, own_land_other),
    by = c("psu", "hhld")
  ) %>%
  left_join(
    dplyr::select(filter(s09, id == 1), psu, hhld, hhg_tot30),
    by = c("psu", "hhld")
  ) %>%
  left_join(
    dplyr::select(filter(s04, id == 1), psu, hhld, occup = mwrk_nsco4, emp_status = mwrk_status, job_sector = mwrk_orgtype),
    by = c("psu", "hhld")
  ) %>%
  left_join(
    dplyr::select(filter(income_s06, id == 1), psu, hhld, income_mth),
    by = c("psu", "hhld")
  )

# checking for number of NA values in each variable ----
raw_independent %>% 
  # View()
  dplyr::summarise(across(everything(), ~sum(is.na(.))))

# dropping the last 4 columns with large number of NAs & creating dummy variables ----
independent_var <- raw_independent %>% 
  select(!c(tail(names(raw_independent), 4))) %>% 
  mutate(
    sex = if_else( sex == 1, 1, 0),
    caste = if_else(caste %in% c(2, 27), 1, 0),
    marital = if_else(marital == 2, 1, 0),
    can_read = if_else(can_read == 1, 1, 0),
    can_write = if_else(can_write == 1, 1, 0),
    ever_school = if_else(ever_school == 1, 1, 0),
    grade_comp = if_else(as.numeric(grade_comp) >= 10, 1, 0),
    tec_voc_training = if_else(tec_voc_training == 1, 1, 0),
    own_land_own = if_else(own_land_own == 1, 1, 0),
    other_land_own = if_else(other_land_own == 1, 1, 0),
    own_land_other = if_else(own_land_other == 1, 1, 0)
  )

# checking for number of NA values in new dataset ----
independent_var %>% 
  dplyr::summarise(across(everything(), ~sum(is.na(.))))

# remove rows with NA terms ----
independent_var <- independent_var %>% 
  mutate(
    can_write = if_else(is.na(can_write) & can_read == 0, 0, can_write),
    grade_comp = if_else(is.na(grade_comp) & can_read == 0, 0, grade_comp)
  ) %>%
  na.omit()

# getting region wise stats of independent variables ----
vars <- names(independent_var)[-c(1, 2)]

regional <- independent_var %>% 
  group_by(region) %>% 
  dplyr::summarise(across(all_of(vars[vars != "region"]), list(
    "Mean" = mean,
    "Sd." = sd,
    "Min." = min,
    "Max." = max
  ), .names = "{.col}-{.fn}")
  ) %>% 
  group_by(region) %>%
  pivot_longer(!region, names_to = "Variables", values_to = "Val") %>% 
  separate(Variables, sep = "-", into = c("Variable", "Stat")) %>% 
  group_by(Variable) %>% 
  pivot_wider(names_from = c("region", "Stat"), names_glue = "{region}_{Stat}", values_from = "Val")

# getting count of the households considered ----
count <- independent_var %>% 
  group_by(region) %>% 
  dplyr::summarise(count = n()) %>% 
  bind_rows(., tibble(region = "Total", count = nrow(independent_var))) %>% 
  pivot_wider(names_from = region, names_glue = "{region}_Mean", values_from = count) %>% 
  mutate(Variable = "Total Count")

# country-wide stats of independent variables ----
total <- independent_var %>% 
  dplyr::summarise(across(all_of(vars[vars != "region"]), list(
    "Mean" = mean,
    "Sd." = sd,
    "Min." = min,
    "Max." = max
  ), .names = "{.col}-{.fn}")
  ) %>% 
  pivot_longer(everything(), names_to = "Variables", values_to = "Val") %>% 
  separate(Variables, sep = "-", into = c("Variable", "Stat")) %>% 
  group_by(Variable) %>% 
  pivot_wider(names_from = c("Stat"), names_glue = "Total_{Stat}", values_from = "Val")
