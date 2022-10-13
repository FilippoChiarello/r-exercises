## Scrape the list of top 250 movies from https://www.imdb.com/chart/top

# Load packages ---------------------------------------------------------------

library(tidyverse)
library(rvest)

# see if we are allowed to scrape the page ----

# install.packages("robotstxt")

library(robotstxt)

paths_allowed("https://twitter.com/")

# Read html page ---------------------------------------------------------------

page <- read_html("https://www.imdb.com/chart/top")

# Titles -----------------------------------------------------------------------

titles <- page %>%
  html_nodes(".titleColumn a") %>%
  html_text()

# Years-------------------------------------------------------------------------

years <- page %>%
  html_nodes(".secondaryInfo") %>%
  html_text() %>%
  str_remove("\\(") %>%
  str_remove("\\)") %>%
  as.numeric()

# Scores -----------------------------------------------------------------------

ratings <- page %>%
  html_nodes("strong") %>%
  html_text() %>%
  as.numeric()

# Put it all in a data frame ---------------------------------------------------

imdb_top_250 <- tibble(
  title = titles,
  rating = ratings,
  year = years
)

# Add rank ---------------------------------------------------------------------

imdb_top_250 <- imdb_top_250 %>%
  mutate(rank = 1:nrow(imdb_top_250)) %>%
  relocate(rank)


imdb_top_250 %>% 
  mutate(before_1994 = ifelse(year <= 1994, "hold", "new")) %>% 
  ggplot(aes(x = rating, fill = before_1994)) +
  geom_boxplot() +
  facet_wrap(vars(before_1994)) +
  coord_flip()
  



imdb_top_250 %>% 
  group_by(year) %>%
  summarise(avg_score = mean(rating)) %>%
  ggplot(aes(y = avg_score, x = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Year", y = "Average score")




