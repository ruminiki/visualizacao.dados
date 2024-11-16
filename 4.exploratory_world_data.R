library(ggplot2)
library(dplyr)
library(scales)

##################
# LOAD DATA
# Reference: 
##################

df <- read.csv("data/world_data_by_country.csv", sep=",")

df <- df %>% filter(sigla %in% c('BRA', 'USA', 'ENG', 'PAR', 'ARG'))

df %>% ggplot(aes(x = pais, y = pib_corrente, fill = pais)) +
  geom_bar(stat = "identity") +
  geom_line(aes(x=pais, y=inflacao))+
  #scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "none")


df %>% ggplot(aes(x = pais, y = pib_corrente, fill = pais)) +
  geom_bar(stat = "identity") +
  geom_line(aes(x=pais, y=inflacao))+
  #scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "none")


df %>%
  group_by(continente) %>%
  summarise(media_desemprego = mean(desemprego_total, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(continente, media_desemprego), y = media_desemprego)) +
  geom_bar(stat = "identity", fill="steelblue") +
  coord_flip() +
  labs(title = "Average Unemployment Rate by Continent", x = "Continent", y = "Average Unemployment Rate") +
  theme_minimal()

