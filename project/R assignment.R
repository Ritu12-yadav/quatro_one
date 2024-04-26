
install.packages("tidyverse")
install.packages("maps")
install.packages("maps")

library(tidyverse)
library(maps)

library(ggplot2)
library(readr)
library(gapminder)
library(dplyr)

unicef_metadata <- read_csv("unicef_metadata.csv")

str(unicef_metadata)

summary(unicef_metadata)


#Scatter plot
ggplot(data = unicef_metadata, aes(x = lifeExp, y = year)) +
  geom_point() +
  labs(title = "Scatter Plot of Life Expectancy vs. year",
       x = "Life Expectancy",
       y = "year")

ggplot(data = unicef_metadata, aes(x = lifeExp, y = year)) +
  geom_point(color = "orange", size = 0.5, shape = 8) +  # Change color, size, and shape
  labs(title = "Increase in  Life Expectancy since 1960 ",
       x = "Life Expectancy",
       y = "Year")


# bar graph

avg_lifeExp <- unicef_metadata %>%
  group_by(country) %>%
  summarize(avg_lifeExp = mean(lifeExp, na.rm = TRUE))

ggplot(avg_lifeExp, aes(x = country, y = avg_lifeExp)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Average Life Expectancy by Country",
       x = "Country",
       y = "Average Life Expectancy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 2)) +
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 0, "", x))


# timeseries 

unicef_metadata$year <- as.Date(paste0(unicef_metadata$year, "-01-01"))
ggplot(unicef_metadata, aes(x = year, y = lifeExp, group = 1)) +
  geom_line(color = "skyblue") +
  labs(title = "Life Expectancy Over Time",
       x = "Year",
       y = "Life Expectancy") +
  theme_minimal()


# world map
library(maps)

ggplot(data = unicef_metadata) +
  aes(x = long, y = lat, group = group, fill = population) +  # Assuming 'population' is the column name for population
  geom_polygon() +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey") +
  labs(
    title = "Differences between countries regarding their population in 2007",
    subtitle = "Countries in grey have no data due to a mismatch with their names",
    caption = "Source: UNICEF Metadata",
    x = "Longitude",
    y = "Latitude",
    fill = "Country Population"
  ) +
  theme_bw()

library(ggplot2)

map_world <- map_data("world")
unicef_metadata_2007 <- unicef_metadata %>%
  filter(year == 2007)
map_pop_2007 <- full_join(map_world, unicef_metadata_2007, by = c("region" = "country"))
ggplot(data = map_pop_2007, aes(x = long, y = lat, group = group, fill = lifeExp)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey") +
  labs(
    title = "Differences between countries regarding their life expectancy in 2007",
    subtitle = "Countries in grey have no data due to a mismatch with their names",
    caption = "Source: UNICEF Metadata",
    x = "Longitude",
    y = "Latitude",
    fill = "Life Expectancy"
  ) +
  theme_bw()
