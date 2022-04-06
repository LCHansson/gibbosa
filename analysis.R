## Libs ----
library("tidyverse")
library("rKolada")

## Data ----
kk <- get_kpi(cache = TRUE, cache_location = "~/dev/gibbosa/kolada_cache/")
kkg <- get_kpi_groups(cache = TRUE, cache_location = "~/dev/gibbosa/kolada_cache/")
km <- get_municipality(cache = TRUE, cache_location = "~/dev/gibbosa/kolada_cache/")
kmg <- get_municipality_groups(cache = TRUE, cache_location = "~/dev/gibbosa/kolada_cache/")
ko <- get_ou(cache = TRUE, cache_location = "~/dev/gibbosa/kolada_cache/")

load("data/kolada_all_kpis.RData")

kpi_latest <- kpi_values %>% 
  group_by(kpi) %>% 
  filter(year == max(year, na.rm = TRUE)) %>% 
  filter(!is.na(value))

kpis_with_little_data <- kpi_latest %>% 
  filter(gender == "T") %>% 
  group_by(kpi) %>% 
  filter(n() < 150) %>% 
  select(-c(downloaded_on, year, status, gender, count, municipality_type)) %>% 
  ungroup()

kpis_with_little_data %>% 
  count(municipality)

kpis_with_little_data %>%
  filter(!str_detect(kpi, "^U")) %>% 
  count(municipality) %>% arrange(desc(n))

kpis_with_little_data %>%
  filter(!str_detect(kpi, "^U")) %>% 
  summarise(kpis = n_distinct(kpi))

kpis_with_little_data %>% 
  filter(!str_detect(kpi, "^U")) %>% 
  # slice(1:1000) %>% 
  # pivot_wider(names_from = kpi, values_from = value) %>% 
  ggplot(aes(x = kpi, y = fct_infreq(municipality), fill = value)) +
  geom_bin2d() +
  scale_y_discrete(limits = rev) -> p1

ggsave("plot.png")
