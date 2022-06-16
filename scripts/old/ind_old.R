library(dplyr)
library(tidyr)

# source required scripts ----
source("./scripts/old/import_old.R")

# select the variables ----
os01_n <- as_tibble(sapply(os01, function(x) {attributes(x) <- NULL; x}))
ind_old <- os01 %>% 
  filter(v01_04 == 1) %>% 
  select(age = v01_03, sex = v01_02, marital = v01_06, caste = v01_08)
