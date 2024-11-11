library(ggplot2)
##################
# LOAD DATA
##################

df <- read.csv("data/world_data.csv", sep=",")
glimpse(df)

#filtra continentes de interesse
df <- df %>% filter(continente %in% c('Europe & Central Asia', 'Africa Eastern and Southern', 'Latin America & Caribbean', 'North America', 'Pacific island small states'))

df <- df %>% mutate(expec_vida_geral = round(as.numeric(expec_vida_geral), 2))

#versão padrão
ggplot(df, aes(x=pib_per_capita, y=expec_vida_geral), fill=continente) +
  geom_point()

#
ggplot(df, aes(x = pib_per_capita, y = expec_vida_geral, color = continente)) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust size and transparency for better visibility
  
  labs(title = "Relação entre pib per capita e expectativa de vida da população",
       subtitle = "Resultados mostram que quanto, maior a renda maior também a expectativa de vida",
       x = "PIB per Capita",
       y = "Expectativa de Vida",
       fill = "Continente") +
  theme_minimal() +  # Use a clean theme for clarity
  
  theme(
    panel.grid = element_blank(),                 # Remove todas as linhas de grade
    axis.line = element_line(color = "black"),    # Adiciona linha nos eixos X e Y
    legend.position = "right",                      # Coloca a legenda no topo do gráfico
    axis.title = element_text(size = 14),         # Aumenta o tamanho da fonte dos títulos dos eixos
    axis.text = element_text(size = 12)           # Aumenta o tamanho da fonte dos rótulos dos eixos
  )
  
  
  
