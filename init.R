## Libs ----
library("tidyverse")
library("rKolada")

## Init ----
## Kolada base metadata
kk <- get_kpi(cache = TRUE, cache_location = paste0(getwd(), "/kolada_cache/"))
kkg <- get_kpi_groups(cache = TRUE, cache_location = paste0(getwd(), "/kolada_cache/"))
km <- get_municipality(cache = TRUE, cache_location = paste0(getwd(), "/kolada_cache/"))
kmg <- get_municipality_groups(cache = TRUE, cache_location = paste0(getwd(), "/kolada_cache/"))
ko <- get_ou(cache = TRUE, cache_location = paste0(getwd(), "/kolada_cache/"))


## Geographic codes
nuts_codes <- read_csv2("data/se_nuts.csv", locale = locale(encoding = "MacRoman"))
nuts_codes <- nuts_codes %>% 
  mutate(
    nuts_1 = str_sub(code, 1, 3),
    nuts_2 = str_sub(code, 1, 4),
    nuts_3 = str_sub(code, 1, 5)
  )
nuts_codes <- nuts_codes %>% filter(nuts_level == 3) %>% 
  select(nuts_1, nuts_2, nuts_3, nuts_3_title = title) %>% 
  left_join(nuts_codes %>% filter(nuts_level == 2) %>% select(code, nuts_2_title = title), by = c("nuts_2" = "code")) %>% 
  left_join(nuts_codes %>% filter(nuts_level == 1) %>% select(code, nuts_1_title = title), by = c("nuts_1" = "code"))

munic_codes <- read_csv2("data/kommunlankod_20211229.csv", locale = locale(encoding = "MacRoman"))
munic_codes <- munic_codes %>% 
  mutate(
    region_code = str_sub(Code, 1, 2)
  )
munic_codes <- munic_codes %>% 
  filter(region_code != Code) %>% 
  dplyr::select(municipality = Code, title = Name, region_code) %>% 
  left_join(munic_codes %>% filter(region_code == Code) %>% dplyr::select(region_title = Name, region_code), by = "region_code")

munic_codes <- munic_codes %>% 
  left_join(nuts_codes, by = c("region_title" = "nuts_3_title"))




## Background data ----

## Econ
grp_kpis <- kpi_search(kk, "BRP")
grp_data <- get_values(grp_kpis$id, period = 2000:2021, municipality = km %>% filter(type == "K") %>% pull(id), simplify = FALSE) %>% 
  mutate(kpi_category = "economy")

# Sanitation
grp_data %>% count(count, gender, status)

# Columns
grp_data_wide <- grp_data %>% pivot_wider(names_from = kpi, values_from = value)

# Correlation
grp_data_comp <- grp_data_wide %>% 
  select(municipality, period, N03700, N03701) %>% 
  filter(!is.na(N03701)) %>% 
  mutate(
    N03700_scaled = scale(N03700)[,1],
    N03701_scaled = scale(N03701)[,1]
  ) %>% 
  group_by(municipality) %>% 
  summarise(
    N03700 = mean(N03700),
    N03701 = mean(N03701),
    diff = sum(N03700_scaled) - sum(N03701_scaled),
    abs_diff = abs(diff)
  ) %>% 
  arrange(desc(abs_diff)) %>% 
  left_join(munic_codes, by = c("municipality"))

## Pop
pop_data <- get_values("N01951", period = 2000:2021, municipality = km %>% filter(type == "K") %>% pull(id), simplify = FALSE) %>% 
  mutate(kpi_category = "demographics")

## Poverty
## 

## Equality, income, gini
income_equality_data <- get_values(
  c("N00956", "N00997", "N00011", "N00952", "N00905", "N00906", "U01803"),
  period = 2000:2021,
  municipality = km %>% filter(type == "K") %>% pull(id),
  simplify = FALSE
) %>% 
  mutate(kpi_category = "economy")

## Education

## Politics
politics_variables <- c(
  "N00664", "N00665", "N05813", "N05812", "N05817", "N05820", "N05811", "N05810", "N05814",
  "N05825", "N05827", "N00658", "N05830", "N05403", "U17413", "N05401", "N05833", "N05831", "N05404",
  "U65869", "U65868", "U65867", "U65866", "U65865", "U65864", "U65863", "U65862", "U65861", "U65860",
  "N65841", "N65843", "N65842", "N65848", "N65846", "N65847", "N65845", "N65844", "N65849",
  "N05807", "N05808", "N05806", "N05803", "N05805", "N05801", "N05804", "N05802", "N05809")

politics_data <- politics_variables %>% map(~get_values(
  kpi = .x,
  period = 2000:2021,
  municipality = km %>% filter(type == "K") %>% pull(id),
  simplify = FALSE,
  verbose = TRUE
)) %>% bind_rows() %>% 
  mutate(kpi_category = "politics")

## Urbanization

## Age structure

## Medication

## Health care expenditure

## Schools and preschools
## - Quality
## - Access and supply
## - Demand (demographics)

## Transportation
## - Availability of public transportation
## - Pricing of public transportation
## - Commuting distances

## Industry

## Elderly care

## Public health
# Medellivslängd
longev_data <- get_values(c("N00925", "N00923", "N70402", "N70403"), period = 2000:2021, municipality = km %>% filter(type == "K") %>% pull(id), simplify = FALSE) %>% 
  mutate(kpi_category = "health")


## Environment
environment_vars <- c(
  "N85537", "N85077", "N85045", "N00521", "N85518",
  "N45911", "N45905", "N45916", "N45917", "N45915", "N45910", "N45912", "N45913", "N45914", "N45920",
  "N07702", "N85532", "N85533", "N85535", "N85536", "N85537", "N85538", "N00401"
)
environment_data <- environment_vars %>% map_df(~get_values(
  kpi = .x,
  period = 2000:2021,
  municipality = km %>% filter(type == "K") %>% pull(id),
  simplify = FALSE,
  verbose = TRUE
)) %>% bind_rows() %>% 
  mutate(kpi_category = "environment")

## Antibiotics
antibi_data <- get_values("N00404", period = 2000:2021, municipality = km %>% filter(type == "K") %>% pull(id), simplify = FALSE) %>% 
  mutate(kpi_category = "health")


## Munge ----
all_data <- bind_rows(grp_data, income_equality_data, longev_data, politics_data, pop_data)

latest_data_nogender <- all_data %>% 
  group_by(kpi, municipality) %>% 
  filter(period == max(period), gender == "T") %>% 
  ungroup()

kpi_counts <- latest_data_nogender %>% 
  count(kpi) %>% 
  filter(n != 290)

latest_data_nogender <- latest_data_nogender %>% 
  anti_join(kpi_counts %>% select(kpi), by = "kpi")

library(corrplot)
cor_prep <- latest_data_nogender %>% 
  left_join(select(kk, kpi = id, title), by = "kpi") %>% 
  select(municipality, title, value) %>% 
  pivot_wider(values_from = value, names_from = title) %>% 
  select(-municipality)
cors <- cor(cor_prep)

corrplot(cors, method = "color")

## Plots ----

# Sweden map
# library("raster")
# swe <- getData("GADM", country = "SWE", level = 1)

# remotes::install_github("reinholdsson/swemaps")
library("swemaps") # https://github.com/reinholdsson/swemaps
swemaps::map_kn

p1 <- map_kn %>% 
  left_join(grp_data_comp, by = c("knkod" = "municipality")) %>% 
  arrange(order) %>% 
  ggplot(aes(ggplot_long, ggplot_lat, group = knkod, fill = diff)) +
  geom_polygon() +
  coord_equal() +
  theme_minimal()

p2 <- map_kn %>% 
  left_join(grp_data_comp %>% arrange(desc(abs_diff)) %>% slice(1:30), by = c("knkod" = "municipality")) %>% 
  arrange(order) %>% 
  ggplot(aes(ggplot_long, ggplot_lat, group = knkod, fill = abs_diff)) +
  geom_polygon() +
  coord_equal() +
  theme_minimal()

library("patchwork")
p1 + p2


# Only Stockholm
map_kn %>% 
  left_join(grp_data_comp, by = c("knkod" = "municipality")) %>%
  # filter(region_code == "03") %>% 
  arrange(order) %>% 
  ggplot(aes(ggplot_long, ggplot_lat, group = knkod, fill = diff)) +
  geom_polygon() +
  # coord_equal() +
  facet_wrap(~ nuts_2_title, scales = "free") +
  theme_void()
