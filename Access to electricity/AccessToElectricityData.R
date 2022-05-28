library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gganimate)
library(ggthemes)
library(extrafont)
library(gifski)
library(RColorBrewer)


electricity_data <- read.csv("share-of-the-population-with-access-to-electricity.csv")

head(electricity_data)

str(electricity_data)

southafrica_data <- electricity_data %>%
  filter(Entity == "South Africa")

southafrica_data


#Line graph visualization 
southafrica_graph <- ggplot(
  southafrica_data,
  aes(x = Year, y =Access.to.electricity....of.population., color = Entity)
) +
  geom_line(size = 2, alpha = 0.75) +
  theme_fivethirtyeight() +
  labs(title = "Access to electricity (% of population)",
       subtitle = "Share of the population with access to electricity",
       x = "Year",
       y = "Percentage of population (%)",
       color = "Country",
       caption = "Source: Our World in Data") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(),
        text = element_text(family="Amiri"),
        axis.title.x = element_text()) +
  scale_color_brewer(palette = "Accent")

southafrica_graph

southafrica.animation = southafrica_graph +
  geom_point() +
  transition_reveal(Year)

southafrica.animation

animate(southafrica.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)
anim_save("South Africa access to electricity.gif")


brics_countries <- c("Brazil", "Russia",
                     "India", "China", "South Africa")

brics_data <- electricity_data %>%
  filter(Entity %in% brics_countries)


#Line graph visualization 
brics_graph <- ggplot(
  brics_data,
  aes(x = Year, y =Access.to.electricity....of.population., color = Entity)
) +
  geom_line(size = 2, alpha = 0.75) +
  theme_fivethirtyeight() +
  labs(title = "Access to electricity (% of population)",
       subtitle = "Share of the population with access to electricity",
       x = "Year",
       y = "Percentage of population (%)",
       color = "Country",
       caption = "Source: Our World in Data") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(),
        text = element_text(family="Amiri"),
        axis.title.x = element_text()) +
  scale_color_brewer(palette = "Set2")

brics_graph

brics.animation = brics_graph +
  geom_point() +
  transition_reveal(Year)

brics.animation

animate(brics.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)
anim_save("BRICS access to electricity.gif")

#Column visualization 

southafrica_col <- ggplot(southafrica_data, aes(Year, Access.to.electricity....of.population., fill = Access.to.electricity....of.population.)) +
  geom_col() +
  labs(title = "Access to electricity in South Africa (% of population)",
       subtitle = "Share of the population with access to electricity",
       x = "Year",
       y = "Percentage of population (%)",
       fill = "Percentage of population (%)",
       caption = "Source: Our World in Data") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE,
    axis.title.y = element_text(),
    text = element_text(family="Amiri"),
    axis.title.x = element_text() 
  )
southafrica_col

sacol.animation = southafrica_col +
  transition_reveal(Year)

sacol.animation

# Second South Africa visualization 
southafrica_col2 <- ggplot(southafrica_data, aes(Year, Access.to.electricity....of.population., fill = Access.to.electricity....of.population.)) +
  geom_col() +
  labs(title = "Access to electricity in South Africa (% of population)",
       subtitle = "Share of the population with access to electricity",
       x = "Year",
       y = "Percentage of population (%)",
       fill = "Percentage of population (%)",
       caption = "Source: Our World in Data") +
  scale_fill_distiller(palette = "Reds",direction = 1) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "white"),
        panel.ontop = TRUE,
        axis.title.y = element_text(),
        text = element_text(family="Amiri"),
        axis.title.x = element_text() 
  )
southafrica_col2

#Example code 
southafrica_col <- ggplot(mean.temp, aes(Month, Temp, fill = Temp)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )
southafrica_col
