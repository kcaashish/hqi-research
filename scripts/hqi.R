library(dplyr)

# importing the required csv files
s00 <- read.csv("./data/S00_rc_dta-csv copy.csv")
s01 <- read.csv("./data/S01_rc_dta-csv copy.csv")
s02 <- read.csv("./data/S02_rc_dta-csv copy.csv")
s04 <- read.csv("./data/S04_rc_dta-csv copy.csv")
s06 <- read.csv("./data/S06_rc_dta-csv copy.csv")
s09 <- read.csv("./data/S09_rc_dta-csv copy.csv")
s12 <- read.csv("./data/S12_rc_dta-csv copy.csv")

dependent_vars <- s01 %>% 
  select(psu, hhld, house_own:type_toilet)
