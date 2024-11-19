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
  summarise(populacao_urbana = mean(populacao_urbana),
            populacao_rural = mean(populacao_rural))

#altera a estrutura do dataset para ser possível a representação gráfica
aux <- pivot_longer(aux, cols = c(populacao_urbana, populacao_rural), 
               names_to = "tipo", 
               values_to = "percentual")

#altera o tipo da variável categórica para fator
aux <- aux %>% 
  mutate(tipo = factor(tipo, levels = c("populacao_urbana", "populacao_rural"))) 

aux %>% 
  ggplot(aes(x = pais, y = percentual, fill = tipo)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(
    x = "País", 
    y = "População (%)", 
    title = "População Urbana e Rural por País",
    fill = "Tipo de População"
  ) +
  scale_fill_manual(
    values = c("populacao_urbana" = "darkblue", "populacao_rural" = "lightblue"),
    labels = c("Urbana", "Rural")
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "solid", size = 0.5)
  )




