library(ggplot2)
library(dplyr)
library(scales)

##################
# LOAD DATA
# Reference: World Data Bank
##################

df <- read.csv("data/world_data_by_country.csv", sep=",")


#Filtrar países de interesse
aux <- df %>% filter(continente %in% c("South America"))


#agrupa por país e calcula a média do período
aux <- aux %>% 
  group_by(pais) %>% 
  summarise(populacao_urbana = mean(populacao_urbana))


#Atividade: elaborar gráfico de barras da taxa de urbanização dos países
aux %>% 
  ggplot(aes(x=pais, y=populacao_urbana)) +
  geom_bar(stat = "identity")

