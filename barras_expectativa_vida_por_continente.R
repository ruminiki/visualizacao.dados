library(dplyr)
library(ggplot2)
library(RColorBrewer)

df <- read.csv("data/gapminder_data.csv", sep=",")

# Calcular a média de expectativa de vida e renda por continente
df_summary <- df %>%
  group_by(continent) %>%
  summarise(mean_lifeexp = mean(lifeexp), mean_income = mean(income)) %>%
  arrange(mean_lifeexp)  # Ordena pelo valor médio de renda

# Transformar 'continent' em um fator com níveis ordenados por renda
df_summary$continent <- factor(df_summary$continent, levels = df_summary$continent)

# Criar o gráfico com uma paleta de cores Brewer em gradiente
df_summary %>% 
  ggplot(aes(x = continent, y = mean_lifeexp, fill = mean_lifeexp)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(y = "Expectativa de Vida Média", x = "Continente", fill = "Expectativa de Vida") +
  theme_minimal() + 
  theme(legend.position = "top") 

ggplot(df_summary, aes(x = reorder(continent, mean_lifeexp), y = mean_lifeexp, fill = mean_lifeexp)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#dcecff", high = "#5589c8") +  # Gradiente de cores manual
  labs(title = "Expectativa de Vida Média por Continente",
       subtitle = "Comparação entre continentes com base na expectativa de vida média, \nmostra que o continente europeu tem a maior expectativa de vida. \nEla é X% maior que no continente Africano.",
       x = "Continente", 
       y = "Expectativa de Vida Média (em anos)",
       fill = "Expectativa de Vida") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(50, 100, by = 10)) + # Ajusta a escala do eixo x (horizontal)
  theme_minimal() +
  theme(
    legend.position = "none", 
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"), 
    plot.title = element_text(size = 16, face = "bold"),     # Tamanho e estilo do título
    axis.title.x = element_text(size = 14, hjust = 1),       # Alinha o título do eixo x à esquerda
    axis.title.y = element_text(size = 14),                  # Tamanho do título do eixo y
    axis.text.x = element_text(size = 14),                   # Tamanho dos rótulos do eixo x
    axis.text.y = element_text(size = 14)                    # Tamanho dos rótulos do eixo y
  )

#########################
# Versão uma cor por continente
#########################
ggplot(df_summary, aes(x = reorder(continent, mean_lifeexp), y = mean_lifeexp, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Expectativa de Vida Média por Continente",
       subtitle = "Comparação entre continentes com base na expectativa de vida média, \nmostra que o continente europeu tem a maior expectativa de vida. \nEla é X% maior que no continente Africano.",
       x = "Continente", 
       y = "Expectativa de Vida Média (em anos)",
       fill = "Expectativa de Vida") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 85), breaks = seq(50, 85, by = 10)) + # Ajusta a escala do eixo x (horizontal)
  theme_minimal() +
  theme(
    legend.position = "none", 
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"), 
    plot.title = element_text(size = 16, face = "bold"),     # Tamanho e estilo do título
    axis.title.x = element_text(size = 14, hjust = 1),       # Alinha o título do eixo x à esquerda
    axis.title.y = element_text(size = 14),                  # Tamanho do título do eixo y
    axis.text.x = element_text(size = 14),                   # Tamanho dos rótulos do eixo x
    axis.text.y = element_text(size = 14)                    # Tamanho dos rótulos do eixo y
  )



