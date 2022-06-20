library(dplyr)
library(tidyr)

# source required scripts ----
source("./scripts/old/import_old.R")

# select the independent variables ----
os01_n <- as_tibble(sapply(os01, function(x) {attributes(x) <- NULL; x}))
ind_old <- os01 %>% 
  filter(v01_idc == 1) %>%
  select(psu = xhpsu, hhld = xhnum, age = v01_03, sex = v01_02, marital = v01_06, caste = v01_08) %>% 
  left_join(., os01 %>% 
              group_by(psu = xhpsu, hhld = xhnum) %>% 
              dplyr::mutate(fam_size = n()) %>% 
              filter(v01_idc == 1) %>% 
              select(psu, hhld, fam_size),
            by = c("psu", "hhld")) %>% 
  left_join(., 
            os07 %>%
              filter(v07_idc == 1) %>%  
              select(psu = xhpsu, hhld = xhnum, can_read = v07_02, can_write = v07_03, ever_school = v07_08,
                     grade_comp = v07_11, technical_vocational = v07_17), 
            by = c("psu", "hhld")) %>% 
  left_join(.,
            os00 %>% 
              dplyr::mutate(
                region = case_when(
                  xstra == 100 ~ 1,
                  xstra %in% c(218, 219, 221, 222, 223, 224, 225) ~ 2,
                  xstra %in% c(310, 321, 322, 323, 324, 325) ~ 3
                )) %>% 
              dplyr::mutate(region = factor(region, labels = c("Himalaya", "Hill", "Terai"))) %>% 
              select(psu = xhpsu, hhld = xhnum, agriculture_land = v13_02, region),
            by = c("psu", "hhld"))
