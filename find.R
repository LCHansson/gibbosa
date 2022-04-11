## Libs ----
library("tidyverse")
library("rKolada")

## Init ----
## Kolada base metadata
kk <- get_kpi(cache = TRUE, cache_location = paste0(getwd(), "/kolada_cache/"))

## Find ----
kpi_search(kk, "narkotika")
