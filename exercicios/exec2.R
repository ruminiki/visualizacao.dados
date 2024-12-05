library(ggplot2)
library(dplyr)
library(scales)

##################
# LOAD DATA
# Reference: World Data Bank
##################

# diferença de acesso a água potável entre países do grupo High Income e Low Income,
# no formato de box-plot

df <- read.csv("data/world_data_by_country.csv", sep=",")


df %>%  
  ggplot(aes(x = continente, y = acesso_agua_potavel, fill = continente)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Acesso à Água Potável por Continente",
       x = "Continente",
       y = "Acesso à Água Potável (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


#storytelling

aux <- df %>% 
  group_by(continente, ano) %>% 
  summarise(acesso_agua_potavel = mean(acesso_agua_potavel, na.rm=T))


aux %>% 
  filter(ano == 2022) %>% 
  ggplot(aes(x=continente, y=acesso_agua_potavel)) +
  geom_bar(stat="identity")
