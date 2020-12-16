library(magrittr)
library(readr)
library(glue)
library(dplyr)
library(tidyr)
library(stringr)

challenges <- read_tsv("data/challenges.tsv")

cartographers <- read_tsv("data/cartographers.tsv", col_types = "ccccc")
maps <- read_tsv("data/maps.tsv", col_types = "ccccccc")
classifications <- read_tsv("data/classifications.tsv", col_types = "ccccccc")
images <- read_tsv("data/images.tsv", col_types = "ccccc")

coords_countries <- read_tsv("data/coords_countries.tsv")
coords_cities <- read_csv("data/worldcities.csv")


# Maps - Countries --------------------------------------------------------------------

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



# Maps - Cartographer Countries -------------------------------------------



cartographer_map_data <- 
  cartographers %>% 
  inner_join(maps, by = "handle") %>% 
  group_by(country) %>% 
  summarise(`Number of Cartographers` = n_distinct(handle),
            `Number of Maps` = n()) %>% 
  ungroup() %>% 
  inner_join(coords_countries,
             by = c(country = "name")) %>% 
  select(
    Lat = latitude,
    Long = longitude,
    Country = country,
    `Number of Cartographers`,
    `Number of Maps`
  )

write_csv(cartographer_map_data, "../30DayMapChallenge2020Metadata/stats/cartographer_map_data.csv")


# Maps - Cities -----------------------------------------------------------

city_map_data <- 
  classifications %>% 
  filter(city != "_") %>% 
  inner_join(coords_cities,
             by = c(area = "country",
                    "city")) %>% 
  count(city, area, lat, lng) %>% 
  select(
    Lat = lat,
    Long = lng,
    City = city,
    Country = area,
    `Number of Maps` = n
  )
write_csv(city_map_data, "../30DayMapChallenge2020Metadata/stats/city_map_data.csv")


# Tables ------------------------------------------------------------------

map_list <- 
  maps %>% 
  sample_frac(1) %>% 
  inner_join(classifications, by = c("handle", "Day")) %>% 
  inner_join(images, by = c("handle", "Day")) %>% 
  inner_join(cartographers, by = "handle") %>% 
  inner_join(challenges, by = "Day") %>% 
  transmute(Challenge = glue("**{Day}** ^{Challenge}^"),
            Cartographer = glue("**@{handle}** ^{coalesce(realname,username)}^"),
            Map = glue("![Map](https://david.frigge.nz/30DayMapChallenge2020/thumbnails/{mapid}.{extension})"),
            Links = glue("[tweet](https://twitter.com/{handle}/status/{tweet_id}) {if_else(website=='_','',paste0('[web](',website,')'))}"),
            Area = area %>% str_replace('^_$', '') %>% str_replace_all(',', ', '),
            City = city %>% str_replace('^_$', '') %>% str_replace_all(',', ', '),
            Topics = topics %>% str_replace('^_$', '') %>% str_replace_all(',', ', '),
            Types = types %>% str_replace('^_$', '') %>% str_replace_all(',', ', '),
            Tools = tools %>% str_replace('^_$', '') %>% str_replace_all(',', ', '))

write_csv(map_list, "data/map_list_for_table.csv")



# Heatmap -----------------------------------------------------------------

day_matrix <- 
  maps %>% 
  transmute(date_utc = date_posted %>% str_sub(1, 10),
            Day) %>% 
  count(date_utc, Day) %>% 
  arrange(Day, date_utc) %>% 
  pivot_wider(names_from = "Day",
              values_from = "n") %>%
  arrange(date_utc) %>% 
  filter(date_utc > "2020-10-15",
         date_utc < "2020-12-08")

write_csv(day_matrix, "data/day_matrix_for_heatmap.csv")

