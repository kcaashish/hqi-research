library(dplyr)
library(tidyr)

get_region_separated_stats <- function(df){
  # getting region wise stats of independent variables ----
  vars <- names(df)[-c(1, 2)]
  
  regional <- df %>%
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
  count <- df %>%
    group_by(region) %>%
    dplyr::summarise(count = n()) %>%
    bind_rows(., tibble(region = "Total", count = nrow(df))) %>%
    pivot_wider(names_from = region,
                names_glue = "{region}_Mean",
                values_from = count) %>%
    mutate(Variable = "Total Count")
  
  # country-wide stats of independent variables ----
  total_no_region <- df %>%
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
  grand_total_no_region_count <-
    bind_rows(grand_total_no_region, count)
  
  return(grand_total_no_region_count)
}


get_region_included_stats <- function(df) {
  # giving 3 different columns for regions ----
  df_reg <- df %>%
    mutate(
      himalaya = if_else(region == "Himalaya", 1, 0),
      hill = if_else(region == "Hill", 1, 0),
      terai = if_else(region == "Terai", 1, 0)
    ) %>%
    select(!c("region"))
  
  # country-wide stats of independent variables including regions ----
  total_observation <- df_reg %>%
    dplyr::summarise(across(
      all_of(names(df_reg)[-c(1:2)]),
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
      Total_Mean = nrow(df_reg)
    ))
  
  return(total_observation_count)
}