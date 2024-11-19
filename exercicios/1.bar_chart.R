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
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid = element_blank()  # Remove grid lines
  )

## Manter linhas dos eixos X e Y
## Alterar para escala de cores 

#! PROMPT
#! altere o gráfico, mantenha as linhas dos eixos X e Y, 
#! mude a cor para uma escala de intensidade do maior para o menor

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
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid.y = element_line(color = "lightgray"),
    panel.grid.x = element_line(color = "lightgray")
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue")














# Criando o gráfico
ggplot(df, aes(x = pais, y = populacao_urbana)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("populacao_urbana" = "#4E79A7", "populacao_urbana" = "#59A14F"),
                    name = "População") +
  geom_hline(yintercept = media_total, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Distribuição da População Urbana e Rural",
       subtitle = "Países da América Latina, EUA e Japão",
       x = "País",
       y = "População (milhões)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "top",
    plot.background = element_rect(fill = "#E8F5E9", color = NA),  # Fundo verde claro
    panel.background = element_rect(fill = "#E8F5E9", color = NA)  # Fundo verde claro para o painel
  )


aux <- df %>% head(50)
#salva o resultado em um novo arquivo
write.csv(aux, "data/world_data_sample.csv", fileEncoding = "UTF-8",  row.names = FALSE)

