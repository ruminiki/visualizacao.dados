library(ggplot2)

# Sample data
data_barplot <- data.frame(
  category = c("A", "B", "C", "D"),
  values = c(10, 20, 15, 30)
)

# Basic bar chart with standard y-axis
p1 <- ggplot(data_barplot, aes(x = category, y = values)) +
  geom_col(fill = "steelblue") +
  labs(title = "Standard Bar Chart",
       x = "Category",
       y = "Values") +
  theme_minimal()

# Bar chart with manipulated y-axis (starting at a non-zero point)
p2 <- ggplot(data_barplot, aes(x = category, y = values)) +
  geom_col(fill = "tomato") +
  coord_cartesian(ylim = c(10, 30)) +
  labs(title = "Manipulated Bar Chart (Y-axis starts at 15)",
       x = "Category",
       y = "Values") +
  theme_minimal()

# Display both plots
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)


#########################
# Versão gapminder manipulação de escala
#########################

df <- read.csv("data/gapminder_data.csv", sep=",")

# Calcular a média de expectativa de vida e renda por continente
df_summary <- df %>%
  group_by(continent) %>%
  summarise(mean_lifeexp = mean(lifeexp), mean_income = mean(income)) %>%
  arrange(mean_lifeexp)  # Ordena pelo valor médio de renda

# Transformar 'continent' em um fator com níveis ordenados por renda
df_summary$continent <- factor(df_summary$continent, levels = df_summary$continent)

p1 <- ggplot(df_summary, aes(x = reorder(continent, mean_lifeexp), y = mean_lifeexp, fill = mean_lifeexp)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#dcecff", high = "#5589c8") +  # Gradiente de cores manual
  labs(
    #title = "Expectativa de Vida Média por Continente",
    #subtitle = "Comparação entre continentes com base na expectativa de vida média, \nmostra que o continente europeu tem a maior expectativa de vida. \nEla é X% maior que no continente Africano.",
    x = "Continente", 
    y = "Expectativa de Vida Média (em anos)",
    fill = "Expectativa de Vida") +
  #coord_flip() +
  #scale_y_continuous(limits = c(0, 100), breaks = seq(50, 100, by = 10)) + # Ajusta a escala do eixo x (horizontal)
  theme_bw() +
  theme(
    legend.position = "none", 
    #panel.grid = element_blank(),
    #axis.line = element_line(color = "black"), 
    plot.title = element_text(size = 16, face = "bold"),     # Tamanho e estilo do título
    axis.title.x = element_text(size = 14, hjust = 1),       # Alinha o título do eixo x à esquerda
    axis.title.y = element_text(size = 14),                  # Tamanho do título do eixo y
    axis.text.x = element_text(size = 14),                   # Tamanho dos rótulos do eixo x
    axis.text.y = element_text(size = 14)                    # Tamanho dos rótulos do eixo y
  )


# Criar o gráfico com barras horizontais, escala de cor gradiente manual e ajustes de título e tamanho dos textos
p2 <- ggplot(df_summary, aes(x = continent, y = mean_lifeexp, fill = mean_lifeexp)) +
  geom_bar(stat='identity') +
  scale_fill_gradient(low = "#dcecff", high = "#5589c8") +  # Gradiente de cores manual
  #coord_cartesian(ylim = c(10, 30)) +
  #coord_flip() +
  coord_cartesian(ylim = c(60, 80)) + 
  
  #scale_y_continuous(limits = c(60, 85), breaks = seq(60, 85, by = 10)) + # Ajusta a escala do eixo x (horizontal)
  
  labs(
    #title = "Expectativa de Vida Média por Continente",
    #subtitle = "Comparação entre continentes com base na expectativa de vida média, \nmostra que o continente europeu tem a maior expectativa de vida. \nEla é X% maior que no continente Africano.",
    x = "Continente", 
    y = "Expectativa de Vida Média (em anos)",
    fill = "Expectativa de Vida"
  ) +
  
  theme_bw() +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16, face = "bold"),     # Tamanho e estilo do título
    axis.title.x = element_text(size = 14, hjust = 1),       # Tamanho do título do eixo x
    axis.title.y = element_text(size = 14),                  # Tamanho do título do eixo y
    axis.text.x = element_text(size = 14),                   # Tamanho dos rótulos do eixo x
    axis.text.y = element_text(size = 14)                    # Tamanho dos rótulos do eixo y
  )

# Display both plots
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
