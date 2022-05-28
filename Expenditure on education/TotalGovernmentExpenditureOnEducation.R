library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(gganimate)
library(extrafont)
library(gifski)
library(RColorBrewer)


education_data <- read.csv("total-government-expenditure-on-education-gdp.csv")


head(education_data)

str(education_data)

View(education_data)

colnames(education_data)

southafrica_data <- education_data %>%
  filter(Entity == "South Africa") %>%
  select(Year, Entity, Government.expenditure.on.education..total....of.GDP.)

southafrica_data

graph_one <- ggplot(
  southafrica_data,
  aes(x = Year, y =Government.expenditure.on.education..total....of.GDP. , color = Entity)
) +
  geom_line(size = 2, alpha = 0.75) +
  theme_fivethirtyeight() +
  labs(title = "Total Government Expenditure on education (% GDP)",
       subtitle = "General government expenditure on education, expressed as a percentage of GDP.",
       x = "Year",
       y = "Total expenditure percentage (% GDP)",
       color = "Country",
       caption = "Source: Our World in Data") +
  theme(legend.position = "bottom",
        axis.title.y = element_text(),
        text = element_text(family="Amiri"),
        axis.title.x = element_text()) +
  scale_color_brewer(palette = "Set2")

graph_one

southafrica.animation = graph_one +
  geom_point() +
  transition_reveal(Year) 

southafrica.animation

animate(southafrica.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)
anim_save("South Africa total government expenditure on education.gif")


 

graph_two = southafrica_data %>%
  ggplot(aes(x=Year, y=Government.expenditure.on.education..total....of.GDP., fill=Entity)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  labs(title = "Total government expenditure on education",
       y = "Percentage of GDP (%)",
       caption = "Source: Kaggle") +
  theme(legend.position = "none",
        axis.title.y = element_text(),
        text = element_text(family="Amiri"),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_tableau()

graph_two

brics_countries <- c("Brazil", "Russia",
                     "India", "China", "South Africa")

brics_data <- education_data %>%
  filter(Entity %in% brics_countries)

head(brics_data)

brics_graph <- ggplot(
  brics_data,
  aes(x = Year, y =Government.expenditure.on.education..total....of.GDP. , color = Entity)
) +
  geom_line(size = 2, alpha = 0.75) +
  theme_fivethirtyeight() +
  labs(title = "Total government expenditure on education (% GDP)",
       subtitle = "General government expenditure on education, expressed as a percentage of GDP.",
       x = "Year",
       y = "Total expenditure percentage (% GDP)",
       color = "Country",
       caption = "Source: Our World in Data") +
  theme(legend.position = "bottom",
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
anim_save("BRICS total government expenditure on education.gif")

