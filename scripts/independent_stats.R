library(dplyr)
library(tidyr)

# sourcing the required scripts ----
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

# getting region wise stats of independent variables ----
vars <- names(independent_var_separate_mar)[-c(1, 2)]

regional <- independent_var_separate_mar %>%
  group_by(region) %>%
  dplyr::summarise(across(
    all_of(vars[vars != "region"]),
    list(
      "Mean" = mean,
      "Sd." = sd,
      "Min." = min,
      "Max." = max
    ),
    .names = "{.col}-{.fn}"
  )) %>%
  group_by(region) %>%
  pivot_longer(!region, names_to = "Variables", values_to = "Val") %>%
  separate(Variables,
           sep = "-",
           into = c("Variable", "Stat")) %>%
  group_by(Variable) %>%
  pivot_wider(
    names_from = c("region", "Stat"),
    names_glue = "{region}_{Stat}",
    values_from = "Val"
  )

# getting count of the households considered ----
count <- independent_var_separate_mar %>%
  group_by(region) %>%
  dplyr::summarise(count = n()) %>%
  bind_rows(., tibble(region = "Total", count = nrow(independent_var_separate_mar))) %>%
  pivot_wider(names_from = region,
              names_glue = "{region}_Mean",
              values_from = count) %>%
  mutate(Variable = "Total Count")

# country-wide stats of independent variables ----
total_no_region <- independent_var_separate_mar %>%
  dplyr::summarise(across(
    all_of(vars[vars != "region"]),
    list(
      "Mean" = mean,
      "Sd." = sd,
      "Min." = min,
      "Max." = max
    ),
    .names = "{.col}-{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = "Variables", values_to = "Val") %>%
  separate(Variables,
           sep = "-",
           into = c("Variable", "Stat")) %>%
  group_by(Variable) %>%
  pivot_wider(
    names_from = c("Stat"),
    names_glue = "Total_{Stat}",
    values_from = "Val"
  )

# bind regional stats with country-wide stats and count values ----
grand_total_no_region <-
  left_join(regional, total_no_region, by = "Variable")
options(scipen = 999)
grand_total_no_region_count <-
  bind_rows(grand_total_no_region, count)

# giving 3 different columns for regions ----
independent_var_separate_mar_reg <- independent_var_separate_mar %>%
  mutate(
    himalaya = if_else(region == "Himalaya", 1, 0),
    hill = if_else(region == "Hill", 1, 0),
    terai = if_else(region == "Terai", 1, 0)
  ) %>%
  select(!c("region"))

# country-wide stats of independent variables including regions ----
total_observation <- independent_var_separate_mar_reg %>%
  dplyr::summarise(across(
    all_of(names(independent_var_separate_mar_reg)[-c(1:2)]),
    list(
      "Mean" = mean,
      "Sd." = sd,
      "Min." = min,
      "Max." = max
    ),
    .names = "{.col}-{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = "Variables", values_to = "Val") %>%
  separate(Variables,
           sep = "-",
           into = c("Variable", "Stat")) %>%
  group_by(Variable) %>%
  pivot_wider(
    names_from = c("Stat"),
    names_glue = "Total_{Stat}",
    values_from = "Val"
  )

# total observation with count ----
total_observation_count <- total_observation %>%
  bind_rows(tibble(
    Variable = "Total Observations",
    Total_Mean = nrow(independent_var_separate_mar_reg)
  ))

# save into csvs ----
write.csv(grand_total_no_region_count, file = "./output/regional_stats.csv", quote = FALSE)
write.csv(total_observation_count, file = "./output/national_stats.csv", quote = FALSE)
