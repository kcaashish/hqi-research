library(haven)

old <- "./data/old"
os00 <- read_sav(file.path(old, "xh00_s00.sav")) 
os01 <- read_sav(file.path(old, "xh01_s01.sav")) 
os02 <- read_sav(file.path(old, "xh02_s02.sav")) 
os07 <- read_sav(file.path(old, "xh10_s07.sav")) 
