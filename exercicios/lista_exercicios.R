# Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# Carregar os dados (substitua o caminho pelo caminho correto do seu arquivo)
df <- read.csv("data/world_data.csv")

######################
# Expectativa de Vida: Comparare cinco países
######################

#agrupa por país e calcula a média do período
aux <- df %>% 
  filter(sigla %in% c("BRA", "ARG", "PRY", "CHL", "USA", "FRA", "WLD"))

head(aux)
unique(aux$pais)

# Criar o gráfico de linhas
aux %>% 
  ggplot(aes(x = ano, y = expec_vida, group = pais, color=sigla)) +
  geom_line() + 
  scale_color_manual(values = c("WLD" = "#f36b6b",
                                "BRA" = "#5589c8")) +  
  labs(title = "Expectativa de Vida por País",      
       subtitle = "Análise da expectativa de vida ao longo dos anos sugere uma relação entre continente Africano\ne países de baixa renda e continente Europeu e países de alta renda.",  
       x = "Ano",                   
       y = "Expectativa de Vida",   
       color = "País") +
  scale_y_continuous(limits=c(50,85), breaks = seq(50,85, by=5)) +
  scale_x_continuous(limits=c(2000,2023), breaks = seq(2000,2023, by=5)) +
  theme_minimal() +                
  theme(legend.position = "top",   
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "gray"))


######################
# Diferença de acesso a água potável por continente, no formato de box-plot;
######################

#agrupa por país e calcula a média do período
aux <- df %>% 
  select(continente, acesso_agua_potavel)


aux %>% 
  ggplot(aes(x=continente, y=acesso_agua_potavel, group=continente, color=continente)) +
  #geom_boxplot() + 
  geom_jitter(width = 0.2, alpha=0.2) + 
  coord_flip() + 
  labs(title = "Acesso a água potável por continente",      
       subtitle = "Análise do acesso a água potável indica que países da África e Ásia\npossuem as menores taxas de acesso.",  
       x = "",                   
       y = "",   
       color = "País") +
  theme_minimal() + 
  theme(legend.position = "none",   
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "gray"))



######################
# Evolução do crescimento médio populacional entre todos os continentes, 
# em formato de gráfico de linhas;
######################
# Selecionar as colunas necessárias e converter tipos de dados
aux <- df %>%
  group_by(continente, ano) %>%
  summarise(crescimento_populacional = mean(crescimento_populacao, na.rm = T))

destaque_continentes <- c("World" = "#5589c8",
                          "Asia" = "#ff9900",
                          "Africa" = "#f36b6b",
                          "Europe" = "#ff9900")

# Criar o gráfico de linhas
aux %>% 
  ggplot(aes(x = ano, y = crescimento_populacional, group = continente)) +
  geom_line(aes(color = continente, alpha = ifelse(continente %in% names(destaque_continentes), 1, 0.3)), size = 1) +
  geom_point(aes(color = continente, alpha = ifelse(continente %in% names(destaque_continentes), 1, 0.3))) +
  scale_color_manual(values = destaque_continentes, name = "Bloco Econômico") +
  scale_alpha_identity() +  # Use identity scale for alpha since we set it manually
  labs(title = "Taxa de Crescimento Populacional por Continente (Destaque: World)",
       x = "Ano", y = "Taxa de Crescimento Populacional (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~continente)

######################
# Elabore uma análise de correlação bivariada utilizando gráfico de dispersão
######################
# Selecionar as colunas necessárias
aux <- df %>%
  select(pais, continente, ano, acesso_agua_potavel, expec_vida) + 
  filter(ano == 2022)

aux %>% 
  ggplot(aes(x = acesso_agua_potavel, y=expec_vida, color=continente)) +
  geom_point()

aux2 <- df %>%
  group_by(continente) %>% 
  summarise(mean_acesso_agua_potavel = mean(acesso_agua_potavel, na.rm = T),
            mean_expec_vida = mean(expec_vida, na.rm = T),
            populacao = mean(populacao_total))

aux2 %>% 
  ggplot(aes(x = mean_acesso_agua_potavel, y=mean_expec_vida, color=continente)) +
  geom_point()

aux2 %>% 
  ggplot(aes(x = mean_acesso_agua_potavel, y=mean_expec_vida, color=continente)) +
  geom_point(aes(size=populacao)) + 
  theme_minimal() +
  theme(legend.position = "top") 
  

######################
# Elabore um histograma do crescimento da população. Utilize facetas para separação dos gráficos por continente.
######################
