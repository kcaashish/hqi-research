library(dplyr)
library(tidyr)
library(labelled)

# source the required scripts ----
source("./scripts/old/import_old.R")

# get the dependent vars ----
dep_old <- os02 %>% 
  select(psu = xhpsu, hhld = xhnum, outerwall = v02_04, foundation = v02_05, roofing = v02_06, 
         ownership = v02_11, drinking_source = v02_19, toilet = v02_26, electric_source = v02_27, 
         fuel_source = v02_33)

str(dep_old)

# convert haven_labelled data to factor ----
dep_old_f <- to_factor(dep_old, levels = "labels")
str(dep_old_f)

# Cronbach's alpha initial ----
alpha_hqi <- dep_old[-c(1, 2)]
psych::alpha(alpha_hqi, check.keys = T)

# reversing the levels of negatively related factors ----
dep_old_rev <- dep_old_f %>%
  mutate(
    roofing = factor(roofing, levels = rev(levels(roofing))),
    ownership = factor(ownership, levels = rev(levels(ownership))),
    fuel_source = factor(fuel_source, levels = rev(levels(fuel_source)))
  )

# converting to numberic value ----
dep_old_rev_num <- as_tibble(lapply(dep_old_rev, as.numeric))

# Cronbach's alpha final ----
alpha_hqi_rev <- dep_old_rev_num[-c(1, 2)]
psych::alpha(alpha_hqi_rev, check.keys = T)
