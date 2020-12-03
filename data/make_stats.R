library(magrittr)
library(readr)
library(dplyr)
library(tidyr)

classifications <- read_tsv("data/classifications.tsv", col_types = "ccccccc")
coords_countries <- read_tsv("data/coords_countries.tsv")

country_map_data <- 
  classifications %>% 
  separate_rows(area, sep = ",") %>% 
  count(area) %>% 
  inner_join(coords_countries,
             by = c(area = "name")) %>% 
  select(
    Lat = latitude,
    Long = longitude,
    Country = area,
    `Number of Maps` = n
  )

write_csv(country_map_data, "../30DayMapChallenge2020Metadata/stats/country_map_data.csv")
