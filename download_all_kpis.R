# Libs ----
library("tidyverse")
library("rKolada")

# Basics ----
kk <- get_kpi(cache = TRUE, cache_location = "~/dev/gibbosa/kolada_cache/")
kkg <- get_kpi_groups(cache = TRUE, cache_location = "~/dev/gibbosa/kolada_cache/")
km <- get_municipality(cache = TRUE, cache_location = "~/dev/gibbosa/kolada_cache/")
kmg <- get_municipality_groups(cache = TRUE, cache_location = "~/dev/gibbosa/kolada_cache/")
ko <- get_ou(cache = TRUE, cache_location = "~/dev/gibbosa/kolada_cache/")

# Download ----
# ALL data
ids <- kk$id
pp <- kk$publ_period
no_data <- character(0)

# res <- list()
# rm(vals)

downloaded_kpis <- dir("data/", pattern = "*.RData") %>% 
  str_subset("^cache_") %>% 
  str_extract("[A-Z][0-9]*")

kpis_to_download <- setdiff(ids, downloaded_kpis)
num_kpis <- length(kpis_to_download)

for (i in 1:length(kpis_to_download)) {
  
  if (kpis_to_download[i] %in% no_data) {
    cat("KPI:", kpis_to_download[i], "has alread been detected as empty; skipping.", paste0("(", i, "/", num_kpis, ")"), "\n")
    next()
  }
  
  period <- if(is.na(pp[i])) 2018:2021 else (as.numeric(pp[i])-3):pp[i]
  
  cat(paste0("Downloading ", kpis_to_download[i], "... "))
  t1 <- Sys.time()
  vals <- get_values(kpi = kpis_to_download[i], period = period)
  t2 <- Sys.time()
  cat(paste0("(", format(t2 - t1, digits = 2), ") "))
  
  if(is.null(vals)) {
    cat("...no data", paste0("(", i, "/", num_kpis, ")"), "\n")
    no_data <- no_data %>% append(kpis_to_download[i])
  } else {
    cat(nrow(vals), "rows", paste0("(", i, "/", num_kpis, ")"), "\n")
    
    vals$downloaded_on <- Sys.Date()
    filename <- paste0("data/cache_", kpis_to_download[i], ".RData")
    save(vals, file = filename)
  }

  Sys.sleep(1)
  
}


## Sort data ----

downloaded_kpis <- dir("data/", pattern = "*.RData") %>% 
  str_subset("^cache_") %>% 
  str_extract("[A-Z][0-9]*")

for (i in 1:length(downloaded_kpis)) {
  load(paste0("data/cache_", downloaded_kpis[i], ".RData"))
  
  vals <- vals %>% 
    filter(municipality_type == "K")
  
  if (!exists("kpi_values"))
    kpi_values <- vals
  else
    kpi_values <- kpi_values %>% bind_rows(vals)
  
  cat(downloaded_kpis[i], "loaded\n")
}

save(kpi_values, file = "data/kolada_all_kpis.RData")