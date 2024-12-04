# Instalar pacotes necessários
library(ggplot2)
library(viridis)
library(dplyr)

# Criar o dataset
data <- data.frame(
  studytime = c("<2 horas", "2 à 5 horas", "5 à 10 horas", ">10 horas"),
  G1 = c(10.6, 10.8, 12.1, 11.9),
  G2 = c(10.3, 10.7, 11.7, 12.1),
  G3 = c(10.1, 10.2, 11.6, 11.3)
)

# Transformar para formato longo
library(reshape2)
data_long <- melt(data, id.vars = "studytime", variable.name = "Notas", value.name = "Media")

# Ordenar os níveis de studytime
data_long$studytime <- factor(
  data_long$studytime,
  levels = c("<2 horas", "2 à 5 horas", "5 à 10 horas", ">10 horas")
)

# Criar o gráfico
ggplot(data_long, aes(x = studytime, y = Media, fill = Notas)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_viridis_d(option = "D") + # Aplica a paleta viridis
  labs(
    title = "Comparação das Médias de Notas (G1, G2, G3) por Tempo de Estudo",
    x = "Tempo de Estudo",
    y = "Média das Notas",
    fill = "Notas"
  ) +
  ylim(0, 20) + # Define os limites do eixo Y
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                 # Remove todas as linhas de grade
    axis.line = element_line(color = "gray"),    # Adiciona linha nos eixos X e Y
    legend.position = "top",                      # Coloca a legenda no topo do gráfico
    axis.title = element_text(size = 14),         # Aumenta o tamanho da fonte dos títulos dos eixos
    axis.text = element_text(size = 12)           # Aumenta o tamanho da fonte dos rótulos dos eixos
  )


#linear chart
ggplot(data_long, aes(x = studytime, y = Media, color = Notas, group = Notas)) +
  geom_line(size = 1.2) +  # Adiciona as linhas
  geom_point(size = 3) +  # Adiciona os pontos para destacar os valores
  scale_fill_viridis_d(option = "D") + # Aplica a paleta viridis
  labs(
    title = "Comparação das Médias de Notas (G1, G2, G3) por Tempo de Estudo",
    x = "Tempo de Estudo",
    y = "Média das Notas",
    fill = "Notas"
  ) +
  #ylim(0, 20) + # Define os limites do eixo Y
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                 # Remove todas as linhas de grade
    axis.line = element_line(color = "gray"),    # Adiciona linha nos eixos X e Y
    legend.position = "top",                      # Coloca a legenda no topo do gráfico
    axis.title = element_text(size = 14),         # Aumenta o tamanho da fonte dos títulos dos eixos
    axis.text = element_text(size = 12)           # Aumenta o tamanho da fonte dos rótulos dos eixos
  )



###########################################
aux1 <- data_long %>% 
  filter(studytime %in% c("<2 horas", "2 à 5 horas")) %>% 
  group_by(Notas) %>% 
  summarise(Media = mean(Media)) %>% 
  mutate(studytime = "até 5h")


aux2 <- data_long %>% 
  filter(studytime %in% c("5 à 10 horas", ">10 horas")) %>% 
  group_by(Notas) %>% 
  summarise(Media = mean(Media)) %>% 
  mutate(studytime = "até 10h")

aux <- rbind(aux1,aux2)


# Ordenar os níveis de studytime
aux$studytime <- factor(
  aux$studytime,
  levels = c("até 5h", "até 10h")
)

#linear chart
ggplot(aux, aes(x = studytime, y = Media, color = Notas, group = Notas)) +
  geom_line(size = 1.2) +  # Adiciona as linhas
  geom_point(size = 3) +  # Adiciona os pontos para destacar os valores
  scale_fill_viridis_d(option = "D") + # Aplica a paleta viridis
  labs(
    title = "Comparação das Médias de Notas (G1, G2, G3) por Tempo de Estudo",
    subtitle = "Comparação entre grupos, indica que a quantidade de horas de estudo\ntem pouca influência sobre a nota.",
    x = "Tempo de Estudo",
    y = "Média das Notas",
    fill = "Notas"
  ) +
  scale_color_manual(values = c("G1" = "#5589c8", "G2"="lightgray", "G3"="lightgray"), name = "Grupos") +
  scale_alpha_manual(values = c("G1" = 1, "G2"=0.5, "G3"=0.5), name = "Grupos") +
  annotate("text", x = "até 10h", y = 12.6, 
           label = "G1 apresentou maior média de notas\nem ambos os casos", 
           hjust = 0, vjust = 2.3, 
           color = "black", size = 3, 
           fontface = "italic") + # Anotação com caixa próxima a 2020
  ylim(10, 12.6) + # Define os limites do eixo Y
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                 # Remove todas as linhas de grade
    axis.line = element_line(color = "gray"),    # Adiciona linha nos eixos X e Y
    legend.position = "top",                      # Coloca a legenda no topo do gráfico
    axis.title = element_text(size = 14),         # Aumenta o tamanho da fonte dos títulos dos eixos
    axis.text = element_text(size = 12)           # Aumenta o tamanho da fonte dos rótulos dos eixos
  )

