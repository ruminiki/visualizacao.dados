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
  filter(continente %in% c('Europe & Central Asia', 'Africa Eastern and Southern', 'Latin America & Caribbean', 'North America', 'World', 'High income', 'Low income')) %>% 
  select(continente, ano, pib_taxa_crescimento) %>%
  mutate(ano = as.integer(ano),
         pib_taxa_crescimento = as.numeric(pib_taxa_crescimento))

head(df_chart)

# Criar o gráfico de linhas
ggplot(df_chart, aes(x = ano, y = pib_taxa_crescimento, color = continente, group = continente)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Taxa de Crescimento do PIB por Continente ao Longo do Tempo",
       x = "Ano", y = "Taxa de Crescimento do PIB (%)") 
  #theme_minimal()


####################
# DESTAQUE PARA UM CONTINENTE
####################

# destaque para um continente
df_chart <- df_chart %>%
  mutate(linha_cor = ifelse(continente == "Low income", "Low income", "Outros"))

head(df_chart)

# Criar o gráfico de linhas
ggplot(df_chart, aes(x = ano, y = pib_taxa_crescimento, group = continente)) +
  geom_line(aes(color = linha_cor, alpha = linha_cor), size = 1) +
  geom_point(aes(color = linha_cor, alpha = linha_cor)) +
  scale_color_manual(values = c("Low income" = "#5589c8", "Outros"="lightgray")) +
  scale_alpha_manual(values = c("Low income" = 1, "Outros" = 0.5)) +
  labs(title = "Taxa de Crescimento do PIB por Continente (Destaque: Low income)",
       x = "Ano", y = "Taxa de Crescimento do PIB (%)") +
  theme_minimal() +
  theme(legend.position = "none")


####################
# REMOVER ELEMENTOS 
####################

# Criar o gráfico de linhas
ggplot(df_chart, aes(x = ano, y = pib_taxa_crescimento, group = continente)) +
  geom_line(aes(color = linha_cor, alpha = linha_cor), size = 1) +
  geom_point(aes(color = linha_cor, alpha = linha_cor)) +
  scale_color_manual(values = c("Low income" = "#5589c8", "Outros"="lightgray")) +
  scale_alpha_manual(values = c("Low income" = 1, "Outros" = 0.5)) +
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

# Criar o gráfico de linhas
ggplot(df_chart, aes(x = ano, y = pib_taxa_crescimento, group = continente)) +
  geom_line(aes(color = linha_cor, alpha = linha_cor), size = 1) +
  geom_point(aes(color = linha_cor, alpha = linha_cor)) +
  scale_color_manual(values = c("Low income" = "#5589c8", "Outros"="lightgray")) +
  scale_alpha_manual(values = c("Low income" = 1, "Outros" = 0.5)) +
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
media_crescimento <- mean(df_chart$pib_taxa_crescimento, na.rm = TRUE)
limite_inferior <- media_crescimento * 0.5  # 50% da média
limite_superior <- media_crescimento * 1.5  # 150% da média

ggplot(df_chart, aes(x = ano, y = pib_taxa_crescimento, group = continente)) +
  geom_line(aes(color = linha_cor, alpha = linha_cor), size = 1) +
  geom_point(aes(color = linha_cor, alpha = linha_cor)) +
  geom_hline(yintercept = media_crescimento, size = 0.2, linetype = "dashed", colour = "red") +
  scale_color_manual(values = c("Low income" = "#5589c8", "Outros"="lightgray")) +
  scale_alpha_manual(values = c("Low income" = 1, "Outros" = 0.5)) +
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
# Crie uma anotação
####################

ggplot(df_chart, aes(x = ano, y = pib_taxa_crescimento, group = continente)) +
  geom_line(aes(color = linha_cor, alpha = linha_cor), size = 1) +
  #geom_point(aes(color = linha_cor, alpha = linha_cor)) +
  geom_hline(yintercept = media_crescimento, size = 0.2, linetype = "dashed", colour = "red") +
  scale_color_manual(values = c("Low income" = "#5589c8", "Outros"="lightgray"), name = "Bloco Econômico") +
  scale_alpha_manual(values = c("Low income" = 1, "Outros" = 0.5), name = "Bloco Econômico") +
  scale_x_discrete(limits=seq(2020,2023)) +
  #scale_y_continuous(limits=seq(-11,+12, by=2)) +
  scale_y_continuous(limits=c(-8,8), breaks = seq(-10,10, by=2)) +
  #limits=seq(-11,+12, by=2) +
  labs(
    title = "Taxa de Crescimento do PIB por Continente (Destaque: Países de baixa renda)",
    subtitle = "Países do bloco apresentaram menor oscilação no PIB durante e no pós pandemia",
    x = "Ano",
    y = "Taxa de Crescimento do PIB (%)"
  ) +
  annotate("text", x = 2020, y = media_crescimento * 1.1, 
           label = "Ano de Mudanças Globais\n(E.g., COVID-19)", 
           hjust = 0, vjust = 2.3, 
           color = "black", size = 4, 
           fontface = "italic") + # Anotação com caixa próxima a 2020
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                 # Remove todas as linhas de grade
    axis.line = element_line(color = "black"),    # Adiciona linha nos eixos X e Y
    legend.position = "top",                      # Coloca a legenda no topo do gráfico
    axis.title = element_text(size = 14),         # Aumenta o tamanho da fonte dos títulos dos eixos
    axis.text = element_text(size = 12)           # Aumenta o tamanho da fonte dos rótulos dos eixos
  )


