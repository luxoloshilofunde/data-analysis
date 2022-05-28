library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gganimate)
library(ggthemes)
library(extrafont)
library(gifski)
library(RColorBrewer)
library(ggridges)

annual_temperature <- read.csv("observed-average-annual-mean-temperature-of-south-africa-for-1901-2020.csv")
monthly_temperature <- read.csv("monthly-climatology-of-min-temperature,-mean-temperature,-max-temperature-&-precipitation-1991-2020_br__south-africa.csv")

head(annual_temperature)
head(monthly_temperature)

str(annual_temperature)
str(monthly_temperature)

summary(annual_temperature)
summary(monthly_temperature)

mean.temp <- monthly_temperature %>%
  group_by(Category) %>%
  select(Mean.Temperature)

mean.temp

# Data visualization
annual_graph <- ggplot(
  annual_temperature,
  aes(x = Category, y =Annual.Mean)
) +
  geom_line(size = 2, alpha = 0.75) +
  theme_fivethirtyeight() +
  labs(title = "Average Annual Mean-Temperature of South Africa",
       subtitle = "Annual mean-temperature of South Africa, 1901-2020",
       x = "Year",
       y = "Temperature (°C)",
       color = "Country",
       caption = "Source: World Bank") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(),
        text = element_text(family="Amiri"),
        axis.title.x = element_text()) +
  scale_color_brewer(palette = "Set2")

annual_graph

annual.animation = annual_graph +
  geom_point() +
  transition_reveal(Category)

annual.animation

annual2.animation = annual_graph +
  transition_reveal(Category) + 
  view_follow(fixed_y = TRUE)

annual2.animation


monthly_temperature$Category = factor(monthly_temperature$Category, levels = month.abb)

# Visualization 
monthly_col <- ggplot(monthly_temperature, aes(Category, Mean.Temperature, fill = Mean.Temperature)) +
  geom_col() +
  labs(title = "Monthly Climatology of Mean-Temperature of South Africa",
       subtitle = "Monthly mean-temperature of South Africa, 1991-2020",
       x = "Year",
       y = "Temperature (°C)",
       fill = "Temperature (°C)",
       caption = "Source: World Bank, Visualisation: @shilofunde") +
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

monthly_col

monthly.animation = monthly_col + transition_states(Category, wrap = FALSE) +
  shadow_mark()

monthly.animation

animate(monthly.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)
anim_save("Mean Temperature graph of South Africa.gif")

monthly2.animation = monthly_col + transition_states(Category, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()

monthly2.animation

animate(monthly2.animation, height = 500, width = 800, fps = 30, duration = 12,
        end_pause = 60, res = 100)
anim_save("Mean Temperature South Africa.gif")







