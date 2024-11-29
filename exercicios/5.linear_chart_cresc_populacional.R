# Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# Carregar os dados (substitua o caminho pelo caminho correto do seu arquivo)
world_data <- read.csv("data/world_data.csv")

######################
# PADRÃO
######################

# Selecionar as colunas necessárias e converter tipos de dados
df_chart <- world_data %>%
  group_by(continente, ano) %>%
  summarise(crescimento_populacional = mean(crescimento_populacao, na.rm = T))

# Criar o gráfico de linhas
df_chart %>% ggplot(aes(x = ano, y = crescimento_populacional, color = continente, group = continente)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Taxa de Crescimento do PIB por Continente ao Longo do Tempo",
       x = "Ano", y = "Taxa de Crescimento do PIB (%)") 


ggplot(df_chart, aes(x=0,y=0))
####################
# DESTAQUE PARA UM CONTINENTE
####################
# destaque para um continente
df_chart <- df_chart %>%
  mutate(destaque = ifelse(continente == "World", "World", "Outros"))

head(df_chart)

# Criar o gráfico de linhas
ggplot(df_chart, aes(x = ano, y = crescimento_populacional, group = continente)) +
  geom_line(aes(color = destaque, alpha = destaque), size = 1) +
  geom_point(aes(color = destaque, alpha = destaque)) +
  scale_color_manual(values = c("World" = "#5589c8", "Outros"="lightgray")) +
  scale_alpha_manual(values = c("World" = 1, "Outros" = 0.5)) +
  labs(title = "Taxa de Crescimento Populacional por Continente (Destaque: World)",
       x = "Ano", y = "Taxa de Crescimento Populacional (%)") +
  theme_minimal() +
  theme(legend.position = "none")


####################
# REMOVER ELEMENTOS 
####################

# Criar o gráfico de linhas
ggplot(df_chart, aes(x = ano, y = crescimento_populacional, group = continente)) +
  geom_line(aes(color = destaque, alpha = destaque), size = 1) +
  geom_point(aes(color = destaque, alpha = destaque)) +
  scale_color_manual(values = c("World" = "#5589c8", "Outros"="lightgray")) +
  scale_alpha_manual(values = c("World" = 1, "Outros" = 0.5)) +
  labs(title = "Taxa de Crescimento do PIB por Continente (Destaque: Low income)",
       x = "Ano", y = "Taxa de Crescimento do PIB (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid lines
    axis.line = element_line(color = "black"),
    legend.position = "none")


####################
# Aumentar tamanho fonte axis X e Y
####################
ggplot(df_chart, aes(x = ano, y = crescimento_populacional, group = continente)) +
  geom_line(aes(color = destaque, alpha = destaque), size = 1) +
  geom_point(aes(color = destaque, alpha = destaque)) +
  scale_color_manual(values = c("World" = "#5589c8", "Outros"="lightgray")) +
  scale_alpha_manual(values = c("World" = 1, "Outros" = 0.5)) +
  labs(title = "Taxa de Crescimento do PIB por Continente (Destaque: Low income)",
       x = "Ano", y = "Taxa de Crescimento do PIB (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid lines
    legend.position = "none",
    axis.line = element_line(color = "gray"),
    axis.title = element_text(size = 14), # Aumenta o tamanho da fonte dos títulos dos eixos
    axis.text = element_text(size = 12))


####################
# Criar um intervalo de interesse a partir da média
####################
# Calcular a média geral da taxa de crescimento
media_crescimento <- mean(df_chart$crescimento_populacional, na.rm = TRUE)

ggplot(df_chart, aes(x = ano, y = crescimento_populacional, group = continente)) +
  geom_line(aes(color = destaque, alpha = destaque), size = 1) +
  geom_point(aes(color = destaque, alpha = destaque)) +
  geom_hline(yintercept = media_crescimento, size = 0.2, linetype = "dashed", colour = "red") +
  scale_color_manual(values = c("World" = "#5589c8", "Outros"="lightgray")) +
  scale_alpha_manual(values = c("World" = 1, "Outros" = 0.5)) +
  labs(title = "Taxa de Crescimento do PIB por Continente (Destaque: Países de baixa renda)",
       x = "Ano", y = "Taxa de Crescimento do PIB (%)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                 # Remove todas as linhas de grade
    axis.line = element_line(color = "black"),    # Adiciona linha nos eixos X e Y
    legend.position = "none",                     # Ocultar legenda
    axis.title = element_text(size = 14),         # Aumenta o tamanho da fonte dos títulos dos eixos
    axis.text = element_text(size = 12)           # Aumenta o tamanho da fonte dos rótulos dos eixos
  )


####################
# Criar uma anotação e subtítulo
####################
# Define the colors for highlighted continents
# Define the colors for highlighted continents
destaque_continentes <- c("World" = "#5589c8",
                          "Asia" = "#ff9900",
                          "Africa" = "#f36b6b",
                          "Europe" = "#ff9900")

# Get the last year in the dataset
last_year <- max(df_chart$ano)

# Create the plot
# Create the plot with smoothed lines
ggplot(df_chart, aes(x = ano, y = crescimento_populacional, group = continente)) +
  #geom_point(aes(color = continente, alpha = ifelse(continente %in% names(destaque_continentes), 1, 0.5))) +
  geom_line(aes(color = continente, alpha = ifelse(continente %in% names(destaque_continentes), 1, 0.5))) + 
  #geom_smooth(aes(color = continente, 
  #                alpha = ifelse(continente %in% names(destaque_continentes), 1, 0.5)), 
  #            method = "loess", # Use LOESS smoothing
  #            size = 1, 
  #            se = FALSE) +  # Disable confidence interval shading
  #geom_hline(yintercept = mean(df_chart$crescimento_populacional), size = 0.2, linetype = "dashed", colour = "red") +
  scale_color_manual(values = destaque_continentes, name = "Bloco Econômico") +
  scale_alpha_identity() +  # Use identity scale for alpha since we set it manually
  scale_x_continuous(limits = c(1999, 2023), breaks = seq(1999, 2023, by = 3)) +
  scale_y_continuous(limits = c(-0.5, 3.5), breaks = seq(-0.5, 3.5, by = 0.5)) +
  labs(
    title = "Taxa de Crescimento do PIB por Continente (Destaque: Países de baixa renda)",
    subtitle = "Países do bloco apresentaram menor oscilação no PIB durante e no pós pandemia",
    x = "Ano",
    y = "Taxa de Crescimento do PIB (%)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                 # Remove todas as linhas de grade
    axis.line = element_line(color = "black"),    # Adiciona linha nos eixos X e Y
    legend.position = "none",                      # Coloca a legenda no topo do gráfico
    axis.title = element_text(size = 14),         # Aumenta o tamanho da fonte dos títulos dos eixos
    axis.text = element_text(size = 12),          # Aumenta o tamanho da fonte dos rótulos dos eixos
    plot.margin = margin(t = 10, r = 30, b = 10, l = 10) # Increase right margin to create space for labels
  ) +
  # Add labels for selected continents at the last year
  geom_text(data = subset(df_chart, ano == last_year & continente %in% names(destaque_continentes)),
            aes(label = paste(round(crescimento_populacional,2), "%")), 
            vjust = -0.3,      # Adjust vertical position of the text
            size = 4,          # Size of the text
            color = "black")+    # Color of the text
  geom_text(data = subset(df_chart, ano == 1999 & continente %in% names(destaque_continentes)),
            aes(label = paste0(round(crescimento_populacional,2), "%")), 
            vjust = -0.3,      # Adjust vertical position of the text
            size = 4,          # Size of the text
            color = "black")    # Color of the text

