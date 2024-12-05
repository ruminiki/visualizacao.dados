library(ggplot2)
library(dplyr)
library(scales)

##################
# LOAD DATA
# Reference: World Data Bank
##################

# a expectativa de vida, em formato de gráfico de linhas. Compare cinco países

df <- read.csv("data/world_data_by_country.csv", sep=",")


aux <- df %>% 
  filter(sigla %in% c("BRA", "PRY", "COL", "BOL", "CHL")) %>% 
  mutate(ano = as.factor(ano))


aux %>% 
  ggplot(aes(x = ano, y=expec_vida, color = pais, group = pais)) +
  geom_line()
