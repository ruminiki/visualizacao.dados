library(ggplot2)
library(dplyr)


##################
# LOAD DATA
# Reference: 
##################


df <- read.csv("data/gapminder_data.csv", sep=",")

#versão básica
df %>% ggplot(aes(x = income/1000, y = lifeexp)) +
  geom_point()


df %>% ggplot(aes(x = income/1000, y = lifeexp, size = population/1000000, fill = continent)) +
  geom_point() +
  geom_point(shape = 21) +
  scale_x_log10() + 
  scale_size_area(max_size = 32, name = "Population (millions)", breaks = c(10, 100, 500, 1000)) +
  scale_fill_manual(values = c("#FF265C", "#FFE700", "#4ED7E9", "#70ED02", "purple"),  name = "Continent") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  xlab("Income (GDP/capita, in thousands of dollars)") +
  ylab("Life expectancy (years)") +
  ggtitle("Strong correlation between economic development and life expectancy")



df %>% ggplot(aes(x = continent, y = lifeexp, fill = continent)) +
  geom_bar(stat="identity") +
  scale_x_log10() + 
  scale_size_area(max_size = 32, name = "Population (millions)", breaks = c(10, 100, 500, 1000)) +
  scale_fill_manual(values = c("#FF265C", "#FFE700", "#4ED7E9", "#70ED02", "purple"),  name = "Continent") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  xlab("Income (GDP/capita, in thousands of dollars)") +
  ylab("Life expectancy (years)") +
  ggtitle("Strong correlation between economic development and life expectancy") +
  facet_wrap(~continent)


ggsave(filename = "ggplot-bubble-chart.png", units = "cm", width = 25, height = 18)