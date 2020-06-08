library(tidyverse)
library(gapminder)

df <- gapminder %>%
  filter(year == 2007) %>%
  left_join(country_codes) %>% 
  rename("iso_a3" = "iso_alpha")

library(rnaturalearth)
world <- ne_countries(type = "countries",  returnclass = 'sf')
sf <- ne_countries(type = "countries",  returnclass = 'sf') %>% 
  left_join(., df, by = "iso_a3", sort = FALSE) %>% 
  filter(!is.na(country)) %>% 
  select("country", "continent" = "continent.y", "year", "lifeExp", "pop", "gdpPercap", "geometry")


ggplot(data=sf) + geom_sf(aes(fill=lifeExp))