library(ggplot2)
library(dplyr)
library(scales)

##################
# LOAD DATA
# Reference: World Data Bank
##################

# Elabore um histograma do crescimento da população. 
# Utilize facetas para separação dos gráficos por continente.


df <- read.csv("data/world_data_by_country.csv", sep=",")

aux <- df %>% 
  filter(ano == 2023)

aux %>% 
  ggplot(aes(x=crescimento_populacao)) +
  geom_histogram(binwidth = 0.8) + 
  facet_wrap(~continente)
  
