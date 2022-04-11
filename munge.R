## Libs ----
library("tidyverse")
library("rKolada")

## Init ----
## Kolada base metadata
kk <- get_kpi(cache = TRUE, cache_location = paste0(getwd(), "/kolada_cache/"))

kk <- kk %>% 
  mutate(
    is_development_kpi = str_detect(description, regex("utvecklingsnyckeltal", ignore_case = TRUE)),
    last_descriptor = str_extract(title, "(?<=,)[\\w\\s/]+$"),
    is_quantity = str_detect(last_descriptor, regex("antal", ignore_case = TRUE)),
    is_cost_per_cap = str_detect(last_descriptor, regex("(kr/inv)", ignore_case = TRUE)),
    is_probably_share = str_detect(title, regex("andel", ignore_case = TRUE)),
    is_probably_cost = str_detect(last_descriptor, regex("(kr|kostnad)$", ignore_case = TRUE))
  ) %>% 
  filter(!is_development_kpi)
