library(dplyr)
library(tidyr)
library(labelled)
library(ggbiplot)
library(psych)

# source the required scripts ----
source("./scripts/old/import_old.R")

# get the dependent vars ----
dep_old <- os02 %>% 
  dplyr::select(psu = xhpsu, hhld = xhnum, outerwall = v02_04, foundation = v02_05, roofing = v02_06, 
         ownership = v02_11, drinking_source = v02_19, toilet = v02_26, electric_source = v02_27, 
         fuel_source = v02_33)

str(dep_old)

# convert haven_labelled data to factor and add region ----
dep_old_f <- to_factor(dep_old, levels = "labels") %>% 
  left_join(.,
            os00 %>% 
              dplyr::mutate(
                region = case_when(
                  xstra == 100 ~ 1,
                  xstra %in% c(218, 219, 221, 222, 223, 224, 225) ~ 2,
                  xstra %in% c(310, 321, 322, 323, 324, 325) ~ 3
                )) %>% 
              dplyr::mutate(region = factor(region, labels = c("Himalaya", "Hill", "Terai"))) %>% 
              select(psu = xhpsu, hhld = xhnum, region),
            by = c("psu", "hhld"))
str(dep_old_f)

# arrange the factor levels, in higher = better order
arranged_dep_ini <-
  as_tibble(lapply(dep_old_f[-c(1, 2, ncol(dep_old_f))], function(x)
    factor(x, levels = rev(levels(
      x
    ))))) %>%
  dplyr::mutate(
    roofing = factor(
      roofing,
      levels = c(
        "Other",
        "Earth/mud",
        "Straw/thatch",
        "Wood/planks",
        "Galvanized iron",
        "Tiles/slate",
        "Concrete/cement"
      )
    ),
    fuel_source = factor(
      fuel_source,
      levels = c(
        "Other",
        "Kerosene",
        "Leaves/rubbish/straw/thatch",
        "Dung",
        "Firewood",
        "Bio-gas",
        "Cylinder gas"
      )
    )
  )
arranged_dep_ini_num <- as_tibble(lapply(arranged_dep_ini, as.numeric))

# Cronbach's alpha initial ----
alpha_hqi <- arranged_dep_ini_num
psych::alpha(alpha_hqi, check.keys = T)

# reversing the levels of negatively related factors ----
arranged_dep <- arranged_dep_ini %>%
  mutate(
    ownership = factor(ownership, levels = rev(levels(ownership))),
  )

# converting to numeric value ----
arranged_dep_num <- as_tibble(lapply(arranged_dep, as.numeric))

# Cronbach's alpha final ----
alpha_hqi_rev <- arranged_dep_num
psych::alpha(alpha_hqi_rev, check.keys = T)

# principal component analysis ----
pca <- prcomp(alpha_hqi_rev, center = TRUE, scale. = TRUE)
summary(pca)

pca2 <- pca(alpha_hqi_rev, nfactors = 2, rotate = "varimax")
summary(pca2)
pca2
print(loadings(pca2), cutoff = 0.5)

# biplot ----
bi <- ggbiplot(
  pca,
  obs.scale = 1,
  circle = TRUE,
  var.scale = 1,
  varname.size = 4
)

# screeplot ----
sc <- screeplot(pca, type = "line", main = "Scree Plot for PCA")

# factor analysis ----
fa <- fa(alpha_hqi_rev,
   nfactors = 2,
   rotate = "varimax",
   fm = "minres")

# calculate the HQI and add region before saving ----
hqi <- arranged_dep_num %>%
  bind_cols(., dep_old_f[ncol(dep_old_f)]) %>% 
  mutate(HQI = rowSums(.[1:8]))

hqi_vars <- arranged_dep %>% 
  bind_cols(., hqi[tail(names(hqi), 2)])

# save dependent vars data ----
saveRDS(hqi_vars, file = "./data/old/processed/dep_old_vars.RDS")
saveRDS(hqi, file = "./data/old/processed/dep_old.RDS")
