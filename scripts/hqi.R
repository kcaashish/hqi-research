library(dplyr)
library(psych)

# importing the required csv files
s00 <- read.csv("./data/S00_rc_dta-csv copy.csv")
s01 <- read.csv("./data/S01_rc_dta-csv copy.csv")
s02 <- read.csv("./data/S02_rc_dta-csv copy.csv")
s04 <- read.csv("./data/S04_rc_dta-csv copy.csv")
s06 <- read.csv("./data/S06_rc_dta-csv copy.csv")
s09 <- read.csv("./data/S09_rc_dta-csv copy.csv")
s12 <- read.csv("./data/S12_rc_dta-csv copy.csv")

dependent_vars <- s01 %>%
  select(psu, hhld, house_own:type_toilet) %>%
  mutate(
    house_own = factor(
      house_own,
      levels = c("Others", "Institutional", "Rented", "Owned")
    ),
    mat_foundation = factor(
      mat_foundation,
      levels = c(
        "Others",
        "Wooden pillar",
        "Mud bonded brick/stone",
        "Cement bonded brick/stone",
        "RCC with pilar"
      )
    ),
    mat_outerwall = factor(
      mat_outerwall,
      levels = c(
        "Others",
        "Unbacked brick",
        "Bamboo",
        "Wooden plate",
        "Mud bonded brick/stone",
        "Cement bonded brick/stone"
      )
    ),
    mat_roof = factor(
      mat_roof,
      levels = c(
        "Others",
        "Mud",
        "Thatch",
        "Wooden plate",
        "Galvanize sheet",
        "Tile / slate",
        "RCC"
      )
    ),
    source_water = factor(
      source_water,
      levels = c(
        "Others",
        "River / stream",
        "Spring water",
        "Uncovered inar / well",
        "Covered inar / well",
        "Handpump / Tubewell",
        "Piped water"
      )
    ),
    source_fuel = factor(
      source_fuel,
      levels = c(
        "Others",
        "Kerosene",
        "GUITHA",
        "Firewood",
        "Bio gas",
        "LP Gas",
        "Electricity"
      )
    ),
    source_light = factor(
      source_light,
      levels = c("Others", "Kerosene", "Bio Gas", "Solar", "Electricity")
    ),
    type_toilet = factor(
      type_toilet,
      levels = c(
        "No toilet",
        "Flush toilet (Public sewarage)",
        "Ordinary",
        "Community toilet",
        "Flush toilet (Septik tank)"
      )
    )
  )

# For Cronbach's alpha
dependent_num <- as_tibble(lapply(dependent_vars, as.numeric))
alpha_hqi <- dependent_num[-c(1, 2)]
psych::alpha(alpha_hqi, check.keys = T)

# The function suggested reversing the order of house_own
# So, reversing the order
dependent_vars_rev <- dependent_vars %>%
  mutate(house_own = factor(house_own, levels = rev(levels(house_own))))

# ----- hqi_num_rev is the final dataset -------
dep_num_rev <- as_tibble(lapply(dependent_vars_rev, as.numeric))

# Cronbach's alpha calculation for final dataset
alpha_hqi_rev <- dep_num_rev[-c(1, 2)]
out <- psych::alpha(alpha_hqi_rev, check.keys = T)
capture.output(out, file = "./output/alpha.txt")
# Raw alpha: 0.66;  Std. alpha: 0.69
