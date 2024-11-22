library(ggplot2)
library(dplyr)
library(scales)

##################
# LOAD DATA
# Reference: World Data Bank
##################

df <- read.csv("data/world_data.csv", sep=",")


#Filtrar países de interesse
aux <- df %>% filter(continente %in% c("South America"))


#agrupar por país e calcula a média do período
aux <- aux %>% 
  group_by(pais) %>% 
  summarise(populacao_urbana = mean(populacao_urbana, na.rm = T))


#Atividade: elaborar gráfico de barras da taxa de urbanização dos países
aux %>% 
  ggplot(aes(x=pais, y=populacao_urbana)) +
  geom_bar(stat = "identity")


## Converter para barras horizontal
## Melhorar tamanho títulos eixo X e Y
## Dar nome aos Eixos

#! PROMPT
#! para o seguinte gráfico em R, aumente o tamanho do título do eixo X e Y, 
#! dê nomes representativos para os eixos e converta o gráfico para barras horizontais

aux %>% 
  ggplot(aes(x=pais, y=populacao_urbana)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Convert to horizontal bars
  labs(
    x = "País", 
    y = "População Urbana (%)", 
    title = "Taxa de Urbanização por País"
  ) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

## Remover cor de fundo
## Ordenar as barras

#! PROMPT
#! alter o gráfico, remova a cor de fundo,
#! remova as linhas de grade e ordene os países pela maior taxa de urbanização

aux %>% 
  arrange(desc(populacao_urbana)) %>%  # Sort countries by urbanization rate
  ggplot(aes(x=reorder(pais, populacao_urbana), y=populacao_urbana)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    x = "País", 
    y = "População Urbana (%)", 
    title = "Taxa de Urbanização por País"
  ) +
  theme_minimal() +  # Remove background color
  theme(
    axis.line = element_line(color = "gray"), 
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid = element_blank()  # Remove grid lines
  )

## Manter linhas dos eixos X e Y
## Alterar para escala de cores 

#! PROMPT
#! altere o gráfico, mantenha as linhas dos eixos X e Y, 
#! mude a cor para uma escala de intensidade do maior para o menor, 
#! remova a legenda

aux %>% 
  arrange(desc(populacao_urbana)) %>%
  ggplot(aes(x=reorder(pais, populacao_urbana), y=populacao_urbana, fill=populacao_urbana)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "País", 
    y = "População Urbana (%)", 
    title = "Taxa de Urbanização por País"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "gray"), 
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid.minor.x = element_line(color = "gray"), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_gradient(low = "lightblue", high = "#4E79A7")


#! PROMPT
#! altere o gráfico, dê destaque ao Brazil. Inclua o rótulo de dados em percentual, apenas para o Brazil

aux %>%
  ggplot(aes(x = reorder(pais, populacao_urbana), y = populacao_urbana)) +
  geom_bar(stat = "identity", aes(fill = ifelse(pais == "Brazil", "#4E79A7", "lightgray"))) +
  geom_text(aes(label = ifelse(pais == "Brazil", paste0(round(populacao_urbana, 0), "%"), "")), 
            position = position_stack(vjust = 1.05),  # Center the text vertically
            color = "black", 
            size = 4) +  # Adjust text size as needed
  coord_flip() +
  labs(
    x = "País", 
    y = "População Urbana (%)", 
    title = "Taxa de Urbanização por País da América Latina",
    subtitle = {"O Brasil é o 5º país mais urbanizado da América Latina, com 86% de sua população vivendo em cidades.\nO Uruguai tem a maior taxa de urbanização do continente, 95%."}
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "gray"), 
    axis.title.x = element_text(size = 12, hjust = 0),
    axis.title.y = element_text(size = 12),
    panel.grid.minor.x = element_line(color = "lightgray"), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    
  ) +
  scale_fill_identity() +  # Use scale_fill_identity to apply fill colors directly
  theme(
    legend.position = "none"  # Remove the legend since it is not necessary
  )


