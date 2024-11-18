library(dplyr)
library(ggplot2)

df <- read.csv("data/world_data_by_country.csv", sep=",")

unique(df$sigla)


aux <- df %>% 
  group_by(continente, ano) %>% 
  filter(continente %in% c("Africa", "Asia", "Europe", "North America", "Oceania", "South America", "World")) %>% 
  summarise(acesso_agua_potaval = mean(acesso_agua_potavel, na.rm = T),
            acesso_energia = mean(acesso_energia_eletrica, na.rm = T),
            densidade_populacional = mean(densidade_populacional, na.rm = T),
            expectativa_vida = mean(expec_vida, na.rm = T),
            controle_corrupcao = mean(controle_corrupcao, na.rm = T))


aux %>% 
  ggplot(aes(x=ano, y=expectativa_vida, fill=continente)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=seq(2010,2024)) +
  geom_hline(yintercept = mean(aux$expectativa_vida, na.rm = T), 
             color = "red", 
             linetype = "dashed")
  





