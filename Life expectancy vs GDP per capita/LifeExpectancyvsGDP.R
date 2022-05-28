library(ggplot2)
library(gganimate)
library(ggthemes)
library(dplyr)
library(extrafont)
library(readr)
library(dplyr)
library(gifski)
library(RColorBrewer)
library(ggstatsplot)

graph_data <- read.csv("Life Expectancy vs GDP 1950-2018.csv")

graph_data

View(graph_data)

options(scipen = 999)
#Static Plot

graph_one = graph_data %>%
  ggplot(aes(x=GDP.per.capita, y=Life.expectancy, size=Population..historical.estimates., color=Continent)) +
  geom_point(alpha = 0.7, stroke = 0) +
  theme_fivethirtyeight() +
  scale_size(range=c(2,12), guide="none") +
  scale_x_log10() +
  labs(title = "Life Expectancy vs GDP Per Capita",
       x = "GDP per capita",
       y = "Life expectancy (years)",
       color = "Continent",
       caption = "Source: Our World in Data") +
  theme(axis.title = element_text(),
      text = element_text(family = "Amiri"),
      legend.text=element_text(size=10)) +
    scale_color_brewer(palette = "Set2")

graph_one

#Transition through distinct countries in time

#Key R function: transition_time(). The transition length between the states will be set to correspond to the actual time difference between them.

#Label variables: frame_time. Gives the time that the current frame corresponds to.

graphone_animation = graph_one + 
  transition_time(Year) +
  labs(subtitle = "Year: {frame_time}") +
  shadow_wake(wake_length = 0.1)

graphone_animation

animate(graphone_animation, height = 500, width = 800, fps = 30, duration = 14,
        end_pause = 60, res = 100)
anim_save("life expectancy vs gdp graph.gif")

#Create facets by continent:

graph_one + facet_wrap(~Continent) +
  transition_time(Year) +
  labs(title = "Year: {frame_time}")

#Let the view follow the data in each frame

graph_one + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  view_follow(fixed_y = TRUE)

#Show preceding frames with gradual falloff

graph_one + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

#Show the original data as background marks

graph_one + transition_time(year) +
  labs(title = "Year: {frame_time}") +
  shadow_mark(alpha = 0.3, size = 0.5)

# Visualization

df_year <- dplyr::filter(graph_data, Year == 2019 | Year == 1950)

p1 <- ggbetweenstats(
  data = df_year,
  x = Year,
  y = Life.expectancy,
  xlab = "Year",
  ylab = "Life expectancy",
  plot.type = "box",
  type = "p",
  conf.level = 0.99,
  title = "Parametric Test For Life Expectancy Around The World",
  package = "ggsci",
  palette = "nrc_npg",
)
p1

animate(graphone_animation, height = 500, width = 800, fps = 30, duration = 14,
        end_pause = 60, res = 100)
anim_save("life expectancy vs gdp graph.gif")

p2 <- ggbetweenstats(
  data = df_year,
  x = Year,
  y = Life.expectancy,
  xlab = "Year",
  ylab = "Life expectancy",
  plot.type = "violin",
  type = "np",
  conf.level = 0.99,
  title = "Non-parametric Test (violin plot)",
  package = "ggsci",
  palette = "uniform_startrek"
)
p2

p3 <- ggbetweenstats(
  data = df_year,
  x = Year,
  y = Life.expectancy,
  xlab = "Year",
  ylab = "Life expectancy",
  plot.type = "boxviolin",
  type = "r",
  conf.level = 0.99,
  title = "Robust Test (box & violin plot)",
  tr = 0.005,
  package = "wesanderson",
  palette = "Royal2",
  k = 3
)
p3

p4 <- ggbetweenstats(
  data = df_year,
  x = Year,
  y = Life.expectancy,
  xlab = "Year",
  ylab = "Life expectancy",
  type = "bayes",
  plot.type = "box",
  title = "Bayesian Test (box plot)",
  package = "ggsci",
  palette = "nrc_npg"
)
p4

p5 <- combine_plots(
  list(p1, p2, p3, p4),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "Comparison of life expectancy between 1957 and 2007",
    caption = "Source: Gapminder Foundation"
  )
)
p5
