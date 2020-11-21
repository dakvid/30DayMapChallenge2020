# build_site.R
# create the site

library(magrittr)
library(readr)
library(glue)
library(forcats)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(htmltools)
library(ggplot2)
library(cowplot)
library(showtext)
showtext_auto()

# font_add_google("Nunito Sans", "nunitosans")
# CHART_FONT <- "nunitosans"
CHART_FONT <- "Nunito Sans"

BS_THEME <- "lux"
source("_template.R", encoding = "UTF-8")



# Load Metadata --------------------------------------------------------

num_tweeters <- 
  read_tsv("data/tweeters.txt",
           col_names = FALSE) %>% 
  pull()


cartographers <-
  read_tsv("data/cartographers.tsv", col_types = "ccccc") %>% 
  rename(location = country)
# shuffle to give an interesting start
maps <- read_tsv("data/maps.tsv", col_types = "ccccccc") %>% sample_frac(1)
classifications <- read_tsv("data/classifications.tsv", col_types = "ccccccc")
images <- read_tsv("data/images.tsv", col_types = "ccccc")
aspect_cols <-
  read_tsv("data/aspects.tsv") %>% 
  mutate(aspect_class = glue("col-xs-{xs} col-sm-{sm} col-md-{md} col-lg-{lg} col-xl-{xl}"))
challenges <-
  read_tsv("data/challenges.tsv") %>% 
  left_join(classifications %>% transmute(Day, n = 1), by = "Day") %>% 
  group_by(Day, Challenge) %>% 
  summarise(num_maps = sum(n, na.rm = T)) %>% 
  ungroup()

areas <- 
  classifications %>% 
  select(area, handle) %>% 
  separate_rows(area, sep = ",") %>% 
  group_by(area) %>% 
  summarise(num_maps = n(),
            num_people = n_distinct(handle)) %>% 
  ungroup() %>% 
  mutate(datavalue = area %>% str_to_lower() %>% str_replace_all(" ", ""))
continents <- 
  read_csv("data/continents.csv") %>% 
  inner_join(areas, by = "area")
countries <-
  areas %>% 
  anti_join(continents, by = "area") %>% 
  arrange(area)
cities <- 
  classifications %>% 
  select(city, handle) %>% 
  separate_rows(city, sep = ",") %>% 
  group_by(city) %>% 
  summarise(num_maps = n(),
            num_people = n_distinct(handle)) %>% 
  ungroup() %>% 
  arrange(city)
cities <- 
  bind_rows(
    cities %>% filter(city != "_"),
    cities %>% filter(city == "_")
  )

topics <- 
  classifications %>% 
  select(topics, handle) %>% 
  separate_rows(topics, sep = ",") %>% 
  group_by(topics) %>% 
  summarise(num_maps = n(),
            num_people = n_distinct(handle)) %>% 
  ungroup() %>% 
  arrange(topics)
topics <- 
  bind_rows(
    topics %>% filter(topics != "_"),
    topics %>% filter(topics == "_")
  )

types_of_maps <- 
  classifications %>% 
  select(types, handle) %>% 
  separate_rows(types, sep = ",") %>% 
  group_by(types) %>% 
  summarise(num_maps = n(),
            num_people = n_distinct(handle)) %>% 
  ungroup() %>% 
  arrange(types)
types_of_maps <- 
  bind_rows(
    types_of_maps %>% filter(types != "_"),
    types_of_maps %>% filter(types == "_")
  )

tools <- 
  classifications %>% 
  select(tools, handle) %>% 
  separate_rows(tools, sep = ",") %>% 
  group_by(tools) %>% 
  summarise(num_maps = n(),
            num_people = n_distinct(handle)) %>% 
  ungroup() %>% 
  arrange(tools)
tools <- 
  bind_rows(
    tools %>% filter(tools != "_"),
    tools %>% filter(tools == "_")
  )




# Make Index --------------------------------------------------------------

index_page <- 
  paste(
    index_header,
    div(class = "row",
        div(class = "col-12",
            p("In 2019 Topi Tjukanov sparked a lot of interest when he announced the #30DayMapChalleng",
              "on Twitter",
              a(href = "https://david.frigge.nz/30DayMapChallenge/", "(see my 2019 gallery)"),
              "that he decided to run it again in November 2020:",
              a(href="https://github.com/tjukanovt/30DayMapChallenge", "https://github.com/tjukanovt/30DayMapChallenge")),
            img(src = "images/map_challenge_themes_2020.jpg", alt = "List of 30 different map themes from Topi Tjukanov"),
            p("I've managed to record all the maps from the Twitter firehose for posterity.",
              "There are so many awesome creations from many talented people around the world that",
              "it's a shame not to be able to go back and look through them for inspiration."),
            h3("Where are the maps?"),
            p(span(class = "text-info",
                  "Click through to the", a(href = "maps.html", "map gallery"),
                  "where you can explore all* the maps."),
              "Turning the unstructured data of thousands of tweets (map submissions and random discussion)",
              "into a structured dataset is naturally a long and partly manual process, so the gallery",
              "content continues to grow."
              ),
            p("The interface allows you to filter by challenge days and the",
              "areas being mapped, as well as other metadata - the types of",
              "maps and the tools used (where they've so far been classified).",
              "Click on a map card to see the full image, and to link through",
              "to the original tweet and the creator's webpage."),
            p("As you might expect, loading the full page and looking at every map in detail",
              "will load 10s or 100s of MB of data, but the images are loaded lazily",
              "so you don't have to download it all at once!"),
            h3("How many people took part? What were the most popular countries?"),
            p("Take a look at the",
              a(href = "stats.html", "stats page"),
              "(though it needs a bit more data at this stage)."),
            p("There is far too much for me to get through in a timely fashion, so",
              "I'm hoping to draw on the wonders of crowdsourcing. Speaking of which..."),
            h3("Can I help complete the metadata for the maps?"),
            p("I'm so glad you asked!",
              span(class = "text-info",
                   "The easiest option is probably to",
                   a(href = "https://docs.google.com/spreadsheets/d/1j2iLnWtBATMxpvDZLXlqaOd0zmcclyg8VIgkPgVMklQ/edit?usp=sharing", "this Google spreadsheet"),
                   "and I'll manually incorporate your changes.")),
            p("Unlike last year, I had the foresight to create a spearate git repository",
              "for the metadata:",
              a(href="https://github.com/dakvid/30DayMapChallenge2020Metadata", "https://github.com/dakvid/30DayMapChallenge2020Metadata"),
              "which means that people can clone just the text files",
              "without the 100s of MB of map images. That's the best place for a pull request if you're so inclined.",
              "You can also drop me an email (myname at frigge.nz) or tweet if you prefer."),
            p("I think I'm managing to keep up with harvesting the tweeets, so",
              "if a map is missing from the gallery then it should be in my",
              "todo list (actually a todo data frame) and will appear soon.",
              "The most helpful area to crowdsource is the metadata on areas,",
              "topics, types and tools."),
            p("Note that I decided to only allow one map per theme/day per person.",
              "Some people made multiple maps for a theme - generally you can see",
              "the others if you click through to the original tweet."),
            h3("Can I make my own?"),
            p("Of course, you can use the metadata I've collated to create your own gallery",
              "or analysis - it would be great to see others' perspectives.",
              "Just don't forget to observe the challenge",
              a(href="https://github.com/tjukanovt/30DayMapChallenge/#code-of-conduct", "code of conduct.")),
            h3("Who are you?"),
            p("I'm", a(href = "https://david.frigge.nz/about", "David Friggens"),
              a(href="https://twitter.com/dakvid", "(@dakvid)"),
              "- just another guy on Twitter with an interest in maps.",
              "I have a number of other commitments in November so my map contributions are/will",
              "be light, but I'm looking forward to being inspired by everyone else."),
            h3("How did you make this site?"),
            p("With", a(href = "https://getbootstrap.com/", "Bootstrap 4"),
              "and the", a(href = "https://bootswatch.com/lux/", "Lux"), "theme from Bootswatch.",
              "The gallery was made with", a(href="https://vestride.github.io/Shuffle/", "shuffle.js"),
              "and", a(href="https://github.com/aFarkas/lazysizes", "lazysizes."),
              "The data munging and HTML construction is performed by some rough",
              a(href = "https://github.com/dakvid/30DayMapChallenge2020", "R code.")),
            p("I've made almost no changes to the code from the 2019 gallery, so the areas that could",
              "be done better aren't. But it still works, and that's the main thing!"),
            h3("Is a FAQ style the best way to structure this page?"),
            p("No, probably not."),
        )
    ),
    index_footer,
    collapse = "\n"
  )

write_file(index_page, "index.html")





# Make Statistics ---------------------------------------------------------

num_indexed_cartographers <- classifications %>% distinct(handle) %>% nrow()
num_countries <- nrow(countries)
num_cities <- nrow(cities)-1
num_maps <- nrow(classifications)

num_unc_cartloc <- cartographers %>% filter(is.na(location)) %>% nrow()
pc_unc_cartloc <- round(num_unc_cartloc / nrow(cartographers) * 100, 1)
num_unc_area <- classifications %>% filter(area == "_") %>% nrow()
pc_unc_area <- round(num_unc_area / num_maps * 100, 1)
num_unc_city <- classifications %>% filter(city == "_") %>% nrow()
pc_unc_city <- round(num_unc_city / num_maps * 100, 1)
num_unc_topic <- classifications %>% filter(topics == "_") %>% nrow()
pc_unc_topic <- round(num_unc_topic / num_maps * 100, 1)
num_unc_type <- classifications %>% filter(types == "_") %>% nrow()
pc_unc_type <- round(num_unc_type / num_maps * 100, 1)
num_unc_tool <- classifications %>% filter(tools == "_") %>% nrow()
pc_unc_tool <- round(num_unc_tool / num_maps * 100, 1)

latest_challenge <- 
  classifications %>% 
  pull(Day) %>% 
  max() %>% 
  as.integer()

num_per_person <- 
  classifications %>% 
  count(handle) %>% 
  rename(num_maps = n) %>% 
  count(num_maps) %>% 
  rename(num_people = n)

full30 <- 
  classifications %>% 
  count(handle) %>% 
  filter(n == 30) %>% 
  inner_join(cartographers, by = "handle")


# > Graphs ----------------------------------------------------------------

g_cartographers_data <- 
  cartographers %>% 
  count(location) %>% 
  drop_na() %>% 
  arrange(-n) %>% 
  head(30) %>% 
  mutate(location = location %>% fct_inorder() %>% fct_rev())
g_cartographers <- 
  ggplot(g_cartographers_data,
         aes(x = location, y = n)) +
  geom_col() +
  geom_text(data = g_cartographers_data %>% 
              filter(n > 9),
            aes(x = location, y = n, label = n),
            hjust = 1, nudge_y = -1,
            color = "white",
            family = CHART_FONT) +
  geom_text(data = g_cartographers_data %>% 
              filter(n <= 9),
            aes(x = location, y = n, label = n),
            hjust = 1, nudge_y = 2,
            color = "black",
            family = CHART_FONT) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT) +
  labs(x = NULL, y = NULL,
       title = "Top 30 Cartographer Locations")
ggsave(filename = "cartographer_location_count.png",
       path = "images/",
       plot = g_cartographers,
       width = 7, height = 5.5, units = "cm", scale = 3)

g_challenges_data <- 
  classifications %>% 
  inner_join(challenges, by = "Day") %>% 
  mutate(challenge = paste(Day, Challenge)) %>% 
  count(Day, challenge) %>% 
  mutate(challenge = challenge %>% fct_inorder() %>% fct_rev(),
         n_display = if_else(Day == "19", 0L, n))
num_null_challenge <- 
  g_challenges_data %>% 
  filter(Day == "19") %>% 
  pull(n)
g_challenges <- 
  ggplot(g_challenges_data,
         aes(x = challenge, y = n_display)) +
  geom_col() + 
  geom_text(data = g_challenges_data,
            aes(x = challenge, y = n_display, label = n),
            hjust = 1, nudge_y = -3,
            color = "white",
            family = CHART_FONT) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT) +
  labs(x = NULL, y = NULL,
       title = "People who completed each daily map (so far)")
ggsave(filename = "challenge_count.png",
       path = "images/",
       plot = g_challenges,
       width = 7, height = 5.5, units = "cm", scale = 3)


g_countries_data <- 
  bind_rows(
    continents,
    countries
  ) %>% 
  filter(area != "_") %>% 
  arrange(desc(num_maps)) %>% 
  head(30) %>% 
  mutate(area = area %>% fct_inorder() %>% fct_rev())
g_countries <- 
  ggplot(g_countries_data,
         aes(x = area, y = num_maps)) +
  geom_col() +
  geom_col(data = g_countries_data,
           aes(x = area, y = num_people),
           fill = "orange", width = 0.3) +
  geom_text(data = g_countries_data %>% 
              filter(num_maps > 15),
            aes(x = area, y = num_maps, label = num_maps),
            hjust = 1, nudge_y = -1,
            color = "white",
            family = CHART_FONT) +
  geom_text(data = g_countries_data %>% 
              filter(num_maps <= 15),
            aes(x = area, y = num_maps, label = num_maps),
            hjust = 1, nudge_y = 3,
            color = "black",
            family = CHART_FONT) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT) +
  labs(x = NULL, y = NULL,
       title = "Top 30 Map Areas")
ggsave(filename = "area_count.png",
       path = "images/",
       plot = g_countries,
       width = 7, height = 5.5, units = "cm", scale = 3)

g_cities_data <- 
  cities %>% 
  filter(city != "_") %>% 
  arrange(desc(num_maps)) %>% 
  head(20) %>% 
  mutate(city = city %>% fct_inorder() %>% fct_rev())
g_cities <- 
  ggplot(g_cities_data,
         aes(x = city, y = num_maps)) +
  geom_col() +
  geom_col(data = g_cities_data,
           aes(x = city, y = num_people),
           fill = "orange", width = 0.3) +
  geom_text(data = g_cities_data %>% 
              filter(num_maps > 7),
            aes(x = city, y = num_maps, label = num_maps),
            hjust = 1, nudge_y = -0.5,
            color = "white",
            family = CHART_FONT) +
  geom_text(data = g_cities_data %>% 
              filter(num_maps <= 7),
            aes(x = city, y = num_maps, label = num_maps),
            hjust = 1, nudge_y = 0.5,
            color = "black",
            family = CHART_FONT) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT) +
  labs(x = NULL, y = NULL,
       title = "Top 20 Cities Mapped")
ggsave(filename = "city_count.png",
       path = "images/",
       plot = g_cities,
       width = 7, height = 4, units = "cm", scale = 3)

g_tools_data <- 
  tools %>% 
  filter(tools != "_") %>% 
  arrange(desc(num_maps)) %>% 
  head(20) %>% 
  mutate(tools = tools %>% fct_inorder() %>% fct_rev())
g_tools <- 
  ggplot(g_tools_data,
         aes(x = tools, y = num_maps)) +
  geom_col() +
  geom_col(data = g_tools_data,
           aes(x = tools, y = num_people),
           fill = "orange", width = 0.3) +
  geom_text(data = g_tools_data %>% 
              filter(num_maps > 50),
            aes(x = tools, y = num_maps, label = num_maps),
            hjust = 1, nudge_y = -2,
            color = "white",
            family = CHART_FONT) +
  geom_text(data = g_tools_data %>% 
              filter(num_maps <= 50),
            aes(x = tools, y = num_maps, label = num_maps),
            hjust = 1, nudge_y = 18,
            color = "black",
            family = CHART_FONT) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT) +
  labs(x = NULL, y = NULL,
       title = "Top 20 Tools Used to Make Maps")
ggsave(filename = "tool_count.png",
       path = "images/",
       plot = g_tools,
       width = 7, height = 4, units = "cm", scale = 3)

g_topics_data <- 
  topics %>% 
  filter(topics != "_") %>% 
  arrange(desc(num_maps)) %>%
  head(20) %>% 
  mutate(topics = topics %>% fct_inorder() %>% fct_rev())
g_topics <- 
  ggplot(g_topics_data,
         aes(x = topics, y = num_maps)) +
  geom_col() +
  geom_col(data = g_topics_data,
           aes(x = topics, y = num_people),
           fill = "orange", width = 0.3) +
  geom_text(data = g_topics_data %>% 
              filter(num_maps > 15),
            aes(x = topics, y = num_maps, label = num_maps),
            hjust = 1, nudge_y = -1,
            color = "white",
            family = CHART_FONT) +
  geom_text(data = g_topics_data %>% 
              filter(num_maps <= 15),
            aes(x = topics, y = num_maps, label = num_maps),
            hjust = 1, nudge_y = 2,
            color = "black",
            family = CHART_FONT) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT) +
  labs(x = NULL, y = NULL,
       title = "Top 20 Topics of Maps Recorded")
ggsave(filename = "topic_count.png",
       path = "images/",
       plot = g_topics,
       width = 7, height = 4, units = "cm", scale = 3)

g_types_data <- 
  types_of_maps %>% 
  filter(types != "_") %>% 
  arrange(desc(num_maps)) %>% 
  head(20) %>% 
  mutate(types = types %>% fct_inorder() %>% fct_rev())
g_types <- 
  ggplot(g_types_data,
         aes(x = types, y = num_maps)) +
  geom_col() +
  geom_col(data = g_types_data,
           aes(x = types, y = num_people),
           fill = "orange", width = 0.3) +
  geom_text(data = g_types_data %>% 
              filter(num_maps > 10),
            aes(x = types, y = num_maps, label = num_maps),
            hjust = 1, nudge_y = -0.5,
            color = "white",
            family = CHART_FONT) +
  geom_text(data = g_types_data %>% 
              filter(num_maps <= 10),
            aes(x = types, y = num_maps, label = num_maps),
            hjust = 1, nudge_y = 0.5,
            color = "black",
            family = CHART_FONT) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT) +
  labs(x = NULL, y = NULL,
       title = "Top 20 Types of Maps Recorded")
ggsave(filename = "type_count.png",
       path = "images/",
       plot = g_types,
       width = 7, height = 4, units = "cm", scale = 3)


# > Page & Save -----------------------------------------------------------

stats_page <- 
  paste(
    stats_header,
    stats_header_nav,
    div(class = 'container',
        h1("Statistics"),
        div(class = "row",
            div(class = "col-12",
                p(glue("There have been at least {num_tweeters} ",
                       "people tweeting on the hashtag. ",
                       "Currently I've indexed {num_maps} maps ",
                       "by {num_indexed_cartographers} people.")),
                p("Maps won't appear automatically - there will be a bit of a lag",
                  "but they'll end up here eventually."),
                h3("Progress"),
                p("Every map that appears here has been assigned a day/challenge",
                  "by me but the majority of the other classifications will take",
                  "months without the wonders of",
                  a(href="https://docs.google.com/spreadsheets/d/1j2iLnWtBATMxpvDZLXlqaOd0zmcclyg8VIgkPgVMklQ/edit?usp=sharing", "crowdsourcing!")),
                p("The graphs below should give you an idea of progress..."),

                h3("Daily Challenges"),
                img(src = "images/challenge_count.png", style="width: 800px;"),
                p("Just kidding - there were", num_null_challenge,
                  "maps for Day 19. 🤡"),
                
                h3("People"),
                # p(glue("There were {nrow(full30)} people who managed the massive task of creating all 30 maps!"),
                #   "(If you're not on this list and should be then let me know.)"),
                # tags$ul(
                #   full30 %>%
                #     pmap(function(handle, username, realname, location, ...) {
                #       tags$li(
                #         if (is.na(realname)) { username } else { realname },
                #         "-",
                #         a(href = glue("https://twitter.com/{handle}/"), glue("@{handle}")),
                #         if (!is.na(location)) { glue("- in {location}") }
                #       )
                #     })
                # ),
                p("So far I have recorded",
                  num_per_person %>% tail(1) %>% pull(num_people),
                  "people submitting",
                  num_per_person %>% tail(1) %>% pull(num_maps),
                  "maps, and",
                  num_per_person %>% tail(2) %>% head(1) %>% pull(num_people),
                  "people submitting",
                  num_per_person %>% tail(2) %>% head(1) %>% pull(num_maps),
                  "maps, leaving them on track for the magic 30 at the end of the month.",
                  "(Something I can't comprehend myself! 😀)"),
                
                # p(tags$em("I'll aim to identify the location of all the map authors, but haven't done that yet.")),
                p("Currently",
                  span(class = "text-danger", glue("only {round(100 - pc_unc_cartloc, 1)}%")),
                  "of cartographers have a country assigned to them."),
                img(src = "images/cartographer_location_count.png", style = "width: 800px;"),
                
                h3("Places"),
                p("Currently",
                  span(class = "text-danger", glue("only {round(100 - pc_unc_area, 1)}%")),
                  "have an area assigned (ie continent or country) and",
                  span(class = "text-danger", glue("only {round(100 - pc_unc_city, 1)}%")),
                  "have a city assigned (though many don't need one)."),
                p("The main bar is the number of maps with that label.",
                  "The small orange bar is the number of cartographers who have produced the maps in that area."),
                img(src = "images/area_count.png", style="width: 800px;"),
                img(src = "images/city_count.png", style="width: 800px;"),
                
                h3("Tools"),
                p("Currently",
                  span(class = "text-danger", glue("only {round(100 - pc_unc_tool, 1)}%")),
                  "have any tools assigned. I have/will automate harvesting as much information",
                  "included in tweets as I can, but many tweets don't mention tools.",
                  a(href = "https://docs.google.com/spreadsheets/d/1j2iLnWtBATMxpvDZLXlqaOd0zmcclyg8VIgkPgVMklQ/edit?usp=sharing",
                    "Please add your missing tool info!"), "🙏"),
                img(src = "images/tool_count.png", style="width: 800px;"),
                
                h3("Map Types"),
                p("Currently",
                  span(class = "text-danger", glue("only {round(100 - pc_unc_type, 1)}%")),
                  "have the type of map assigned.",
                  "This is a manual and semi-subjective classification, so please feel free",
                  a(href = "https://docs.google.com/spreadsheets/d/1j2iLnWtBATMxpvDZLXlqaOd0zmcclyg8VIgkPgVMklQ/edit?usp=sharing", "to contribute.")),
                img(src = "images/type_count.png", style="width: 800px;"),
                
                h3("Topics"),
                p("Currently",
                  span(class = "text-danger", glue("only {round(100 - pc_unc_topic, 1)}%")),
                  "have topics assigned.",
                  "This is a manual and semi-subjective classification, so please feel free",
                  a(href = "https://docs.google.com/spreadsheets/d/1j2iLnWtBATMxpvDZLXlqaOd0zmcclyg8VIgkPgVMklQ/edit?usp=sharing", "to contribute.")),
                img(src = "images/topic_count.png", style="width: 800px;"),
            )
        ),
    ),
    stats_footer,
    collapse = "\n"
)

write_file(stats_page, "stats.html")
        




# Make Maps ---------------------------------------------------------------


# > Cards -----------------------------------------------------------------

make_a_card <- 
  function(mapid, extension, Day, Challenge, handle, date_posted, tweet_id, area, area_norm, city, topics, types, tools, aspect, aspect_class, ...) {
    div(class = glue("map-card {aspect_class}"),
        `data-challenge` = Day,
        `data-area` = area_norm,
        `data-city` = city,
        `data-topics` = topics,
        `data-types` = types,
        `data-tools` = tools,
        `data-date-posted` = date_posted,
        `data-handle` = handle %>% str_to_lower(),
        a(`data-toggle` = "modal",
          `data-target` = glue("#{if_else(str_detect(mapid,'^[0-9]'),'_','')}{mapid}_details"),
          div(class = "card",
              div(class = glue("aspect aspect--{aspect}"),
                  div(class = "aspect__inner",
                      img(class = "card-img lazyload",
                          `data-src` = glue("thumbnails/{mapid}.{extension}"))
                  )
              ),
              div(class = "card-img-overlay",
                  span(class = "badge badge-pill badge-warning",
                       Day),
                  span(class = "badge badge-pill badge-primary",
                       Challenge),
                  span(class = "badge badge-pill badge-secondary",
                       handle)
              )
          )
        )
    )
  }

map_cards <- 
  maps %>% 
  inner_join(classifications, by = c("handle", "Day")) %>% 
  inner_join(images, by = c("handle", "Day")) %>% 
  mutate(area_norm = area %>% str_to_lower() %>% str_replace_all(" ", "")) %>% 
  inner_join(challenges, by = "Day") %>% 
  inner_join(aspect_cols, by = "aspect") %>% 
  pmap(make_a_card)




# > Modals ----------------------------------------------------------------

make_a_modal <- 
  function(mapid, extension, Day, Challenge, handle, date_posted, tweet_id, website, area, city, topics, types, tools, description, username, realname, location, ...) {
    div(id = glue("{if_else(str_detect(mapid,'^[0-9]'),'_','')}{mapid}_details"),
        class = "modal fade",
        tabindex = "-1",
        role = "dialog",
        div(class = "modal-dialog modal-xl",
            role = "document",
            div(class = "modal-content",
                div(class = "modal-header",
                    h5(class = "modal-title",
                       glue("Day {Day} ({Challenge}) by {handle}")),
                    tags$button(type = "button", class = "close",
                                `data-dismiss` = "modal",
                                `aria-label` = "Close",
                                span(`aria-hidden` = "true",
                                     "×"))
                    ),
                div(class = "modal-body",
                    img(style = "max-width: 100%;",
                        class = "lazyload",
                        `data-src` = glue("maps/{mapid}.{extension}")),
                    p(description),
                    p("By ",
                      if_else(is.na(realname),
                              glue("{username} (@{handle})"),
                              glue("{realname} (@{handle} / {username})")),
                      if (!is.na(location)) { glue("in {location}") }
                      ),
                    tags$ul(class = "list-group",
                            tags$li(class = "list-group-item",
                                    strong("Areas:"),
                                    if_else(area == "_",
                                            "unclassified",
                                            area %>% str_replace_all(",", "; "))),
                            if (city != "_") {
                              tags$li(class = "list-group-item",
                                      strong("Cities:"),
                                      city %>% str_replace_all(",", "; "))
                            },
                            tags$li(class = "list-group-item",
                                    strong("Topics:"),
                                    if_else(topics == "_",
                                            "unclassified",
                                            topics %>% str_replace_all(",", "; "))),
                            tags$li(class = "list-group-item",
                                    strong("Map Type:"),
                                    if_else(types == "_",
                                            "unclassified",
                                            types %>% str_replace_all(",", "; "))),
                            tags$li(class = "list-group-item",
                                    strong("Tools:"),
                                    if_else(tools == "_",
                                            "unknown",
                                            tools %>% str_replace_all(",", "; "))),
                           )
                    ),
                div(class = "modal-footer",
                    tags$button(type = "button", class = "btn btn-secondary",
                                `data-dismiss` = "modal",
                                "Close"),
                    tags$button(type = "button", class = "btn btn-info",
                                a(target = "_blank",
                                  href = glue("https://twitter.com/{handle}/status/{tweet_id}"),
                                  "See Tweet")),
                                  if (website != "_") {
                                    tags$button(type = "button", class = "btn btn-info",
                                                a(target = "_blank",
                                                  href = website,
                                                  "See webpage"))
                                  }
                    )
                )
            )
        )
  }

maps_modals <- 
  maps %>% 
  inner_join(classifications, by = c("handle", "Day")) %>% 
  inner_join(images, by = c("handle", "Day")) %>% 
  inner_join(challenges, by = "Day") %>% 
  inner_join(cartographers, by = "handle") %>%
  pmap(make_a_modal) %>% 
  map(as.character) %>% 
  paste(collapse = "\n")





# > Filter Challenge ------------------------------------------------------

maps_filter_challenge_0110 <- 
    div(class = "col-12@sm",
        div(class = "dropdown",
            tags$button(class = "btn btn-primary dropdown-toggle",
                        type = "button",
                        id = "FilterDay0110",
                        `data-toggle` = "dropdown",
                        `aria-haspopup` = "true",
                        `aria-expanded` = "false",
                        "01-10"),
            div(class = "dropdown-menu mapfilter-challenge",
                `aria-labelledby` = "FilterDay0110",
                challenges %>% 
                  filter(Day <= "10") %>% 
                  pmap(function (Day, Challenge, num_maps, ...) {
                    tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                                type = "button",
                                `data-value` = Day,
                                glue("{Day} {Challenge}"),
                                span(class = "badge badge-primary badge-pill",
                                     num_maps))
                  })
            )
        )
    )
maps_filter_challenge_1120 <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-primary dropdown-toggle",
                      type = "button",
                      id = "FilterDay1120",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "11-21"),
          div(class = "dropdown-menu mapfilter-challenge",
              `aria-labelledby` = "FilterDay1120",
              challenges %>% 
                filter(Day >= "11", Day <= "20") %>% 
                pmap(function (Day, Challenge, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = Day,
                              glue("{Day} {Challenge}"),
                              span(class = "badge badge-primary badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_challenge_2130 <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-primary dropdown-toggle",
                      type = "button",
                      id = "FilterDay2130",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "21-30"),
          div(class = "dropdown-menu mapfilter-challenge",
              `aria-labelledby` = "FilterDay2130",
              challenges %>% 
                filter(Day >= "21") %>% 
                pmap(function (Day, Challenge, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = Day,
                              glue("{Day} {Challenge}"),
                              span(class = "badge badge-primary badge-pill",
                                   num_maps))
                })
          )
      )
  )




# > Filter Area -----------------------------------------------------------

maps_filter_area_continent <- 
    div(class = "col-12@sm",
        div(class = "dropdown",
            tags$button(class = "btn btn-info dropdown-toggle",
                        type = "button",
                        id = "FilterAreaContinent",
                        `data-toggle` = "dropdown",
                        `aria-haspopup` = "true",
                        `aria-expanded` = "false",
                        `data-toggle-tt` = "tooltip",
                        `data-placement` = "top",
                        title = "Only includes maps at this level, not all the individual countries",
                        "Continent"),
            div(class = "dropdown-menu mapfilter-area",
                `aria-labelledby` = "FilterAreaContinent",
                continents %>% 
                  pmap(function (area, datavalue, num_maps, ...) {
                    tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                                type = "button",
                                `data-value` = datavalue,
                                if (area == "_") { "unclassified" } else { area },
                                span(class = "badge badge-info badge-pill",
                                     num_maps))
                  })
            )
        )
    )
maps_filter_area_country_AD <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterAreaCountryAD",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Country A-D"),
          div(class = "dropdown-menu mapfilter-area",
              `aria-labelledby` = "FilterAreaCountryAD",
              countries %>% 
                filter(area < "E") %>% 
                pmap(function (area, datavalue, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = datavalue,
                              if (area == "_") { "unclassified" } else { area },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_area_country_EI <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterAreaCountryEI",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "E-I"),
          div(class = "dropdown-menu mapfilter-area",
              `aria-labelledby` = "FilterAreaCountryEI",
              countries %>% 
                filter(area > "E", area < "J") %>% 
                pmap(function (area, datavalue, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = datavalue,
                              if (area == "_") { "unclassified" } else { area },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_area_country_JN <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterAreaCountryJN",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "J-N"),
          div(class = "dropdown-menu mapfilter-area",
              `aria-labelledby` = "FilterAreaCountryJN",
              countries %>% 
                filter(area > "J", area < "O") %>% 
                pmap(function (area, datavalue, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = datavalue,
                              if (area == "_") { "unclassified" } else { area },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_area_country_OS <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterAreaCountryOS",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "O-S"),
          div(class = "dropdown-menu mapfilter-area",
              `aria-labelledby` = "FilterAreaCountryOS",
              countries %>% 
                filter(area > "O", area < "T") %>% 
                pmap(function (area, datavalue, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = datavalue,
                              if (area == "_") { "unclassified" } else { area },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_area_country_TZ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterAreaCountryTZ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "T-Z"),
          div(class = "dropdown-menu mapfilter-area",
              `aria-labelledby` = "FilterAreaCountryTZ",
              countries %>% 
                filter(area > "T") %>% 
                pmap(function (area, datavalue, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = datavalue,
                              if (area == "_") { "unclassified" } else { area },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )



# > Filter City -----------------------------------------------------------

maps_filter_cities_AB <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesAB",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "City A-B"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesAB",
              cities %>% 
                filter(city < "C", city != "_") %>% 
                pmap(function (city, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = city,
                              if (city == "_") { "unclassified" } else { city },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_cities_CF <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesCF",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "C-F"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesCF",
              cities %>% 
                filter(city > "C", city < "G", city != "_") %>% 
                pmap(function (city, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = city,
                              if (city == "_") { "unclassified" } else { city },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_cities_GI <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesGI",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "G-I"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesGI",
              cities %>% 
                filter(city > "G", city < "J", city != "_") %>% 
                pmap(function (city, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = city,
                              if (city == "_") { "unclassified" } else { city },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_cities_JL <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesJL",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "J-L"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesJL",
              cities %>% 
                filter(city > "J", city < "M", city != "_") %>% 
                pmap(function (city, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = city,
                              if (city == "_") { "unclassified" } else { city },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_cities_MO <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesMO",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "M-O"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesMO",
              cities %>% 
                filter(city > "M", city < "P", city != "_") %>% 
                pmap(function (city, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = city,
                              if (city == "_") { "unclassified" } else { city },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_cities_PR <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesPR",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "P-R"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesPR",
              cities %>% 
                filter(city > "P", city < "S", city != "_") %>% 
                pmap(function (city, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = city,
                              if (city == "_") { "unclassified" } else { city },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_cities_ST <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesST",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "S-T"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesST",
              cities %>% 
                filter(city > "S", city < "U", city != "_") %>% 
                pmap(function (city, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = city,
                              if (city == "_") { "unclassified" } else { city },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_cities_UZ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterCitiesUZ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "U-Z"),
          div(class = "dropdown-menu mapfilter-city",
              `aria-labelledby` = "FilterCitiesUZ",
              cities %>% 
                filter(city > "U" | city == "_") %>% 
                pmap(function (city, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = city,
                              if (city == "_") { "unclassified" } else { city },
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )



# > Filter Topic -----------------------------------------------------------

maps_filter_topics_AF <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-secondary dropdown-toggle",
                      type = "button",
                      id = "FilterTopicsAF",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Topic A-F"),
          div(class = "dropdown-menu mapfilter-topic",
              `aria-labelledby` = "FilterTopicsAF",
              topics %>% 
                filter(topics < "G", topics != "_") %>% 
                pmap(function (topics, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = topics,
                              if (topics == "_") { "unclassified" } else { topics },
                              span(class = "badge badge-dark badge-pill",
                                   num_maps))
                })
          )
      )
  )

maps_filter_topics_GZ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-secondary dropdown-toggle",
                      type = "button",
                      id = "FilterTopicsGZ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "G-Z"),
          div(class = "dropdown-menu mapfilter-topic",
              `aria-labelledby` = "FilterTopicsGZ",
              topics %>% 
                filter(topics >= "G" | topics == "_") %>% 
                pmap(function (topics, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = topics,
                              if (topics == "_") { "unclassified" } else { topics },
                              span(class = "badge badge-dark badge-pill",
                                   num_maps))
                })
          )
      )
  )



# > Filter Type -----------------------------------------------------------

maps_filter_types <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-secondary dropdown-toggle",
                      type = "button",
                      id = "FilterTypes",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Map Types"),
          div(class = "dropdown-menu mapfilter-type",
              `aria-labelledby` = "FilterTypes",
              types_of_maps %>% 
                pmap(function (types, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = types,
                              if (types == "_") { "unclassified" } else { types },
                              span(class = "badge badge-dark badge-pill",
                                   num_maps))
                })
          )
      )
  )




# > Filter Tool -----------------------------------------------------------

maps_filter_tools_AJ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-secondary dropdown-toggle",
                      type = "button",
                      id = "FilterToolsAJ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Tool A-J"),
          div(class = "dropdown-menu mapfilter-tool",
              `aria-labelledby` = "FilterToolsAJ",
              tools %>% 
                filter(tools < "K", tools != "_") %>% 
                pmap(function (tools, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = tools,
                              if (tools == "_") { "unclassified" } else { tools },
                              span(class = "badge badge-dark badge-pill",
                                   num_maps))
                })
          )
      )
  )

maps_filter_tools_KZ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-secondary dropdown-toggle",
                      type = "button",
                      id = "FilterToolsKZ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "K-Z"),
          div(class = "dropdown-menu mapfilter-tool",
              `aria-labelledby` = "FilterToolsKZ",
              tools %>% 
                filter(tools >= "K" | tools == "_") %>% 
                pmap(function (tools, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = tools,
                              if (tools == "_") { "unclassified" } else { tools },
                              span(class = "badge badge-dark badge-pill",
                                   num_maps))
                })
          )
      )
  )


# > Sort ------------------------------------------------------------------

maps_sorting <-
  div(class = "col-12@sm",
      span(class = "button_legend",
           "Sort: "),
      div(class = "btn-group btn-group-toggle sort-options",
          `data-toggle` = "buttons",
          tags$label(class = "btn btn-success active",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "dom",
                                checked = NA),
                     "Default"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "challenge"),
                     "Day #"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "posted-old-new"),
                     "Tweet (old-new)"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "posted-new-old"),
                     "Tweet (new-old)"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "handle"),
                     "Handle")
          )
     )



# > Search Handle ---------------------------------------------------------

maps_search_handle <- 
  div(class = "col-12@sm",
      tags$input(type = "text",
                 class = "form-control mapfilter-handle-search",
                 placeholder = "Search Twitter Handle",
                 id = "searchHandle")
      )


# > Reset Filters ---------------------------------------------------------

maps_reset_filters <- 
  div(class = "col-12@sm",
      tags$button(type = "button",
                  class = "btn btn-danger mapfilter-reset",
                  id = "mapfilter-reset-button",
                  "Reset All Filters"
                  )
      )



# > Page & Save -----------------------------------------------------------
  
maps_page <- 
  paste(
    maps_header,
    maps_header_nav,
    div(class = 'container',
        h1("Map Gallery"),
        # filter/sort
        div(class = "row",
            maps_filter_challenge_0110, maps_filter_challenge_1120, maps_filter_challenge_2130,
            maps_filter_area_continent, maps_filter_area_country_AD, maps_filter_area_country_EI, maps_filter_area_country_JN, maps_filter_area_country_OS, maps_filter_area_country_TZ,
        ),
        div(class = "row",
            maps_filter_cities_AB, maps_filter_cities_CF, maps_filter_cities_GI, maps_filter_cities_JL,
            maps_filter_cities_MO, maps_filter_cities_PR, maps_filter_cities_ST, maps_filter_cities_UZ,
            ),
        div(class = "row",
            maps_filter_topics_AF, maps_filter_topics_GZ,
            maps_filter_types,
            maps_filter_tools_AJ, maps_filter_tools_KZ,
            maps_search_handle,
            maps_reset_filters),
        div(class = "row",
            maps_sorting
            ),
        ),
    # cards
    div(class = "container-fluid",
        div(id = "grid", class = "row my-shuffle-container",
            map_cards,
            div(class = "col-1 my-sizer-element"))),
    # modals
    maps_modals,
    # JS libraries & code
    maps_scripts,
    maps_footer,
    collapse = "\n"
  )


write_file(maps_page, "maps.html")

