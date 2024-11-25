# Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# Carregar os dados (substitua o caminho pelo caminho correto do seu arquivo)
df <- read.csv("data/world_data.csv")

#agrupa por país e calcula a média do período
aux <- df %>% 
  group_by(continente, ano) %>% 
  summarise(expec_vida = mean(expec_vida, na.rm = T))

head(aux)
unique(aux$continente)

# Criar o gráfico de linhas
ggplot(aux, aes(x = ano, y = expec_vida, group = continente, color=continente)) +
  geom_line()

####################
# DESTAQUE PARA CONTINENTES
####################
ggplot(aux, aes(x = ano, y = expec_vida, group = continente, color=continente)) +
  geom_line() +
  scale_color_manual(values = c("Low income" = "#f36b6b",
                                "High income" = "#5589c8",
                                "Africa" = "#eae754",
                                "Europe" = "blue")) +
  theme_minimal() 



####################
# Aumentar legendas, títulos eixo x e y, título e subtítulo
####################
ggplot(aux, aes(x = ano, y = expec_vida, group = continente, color = continente)) +
  geom_line() +
  scale_color_manual(values = c("Low income" = "#f36b6b",
                                "High income" = "#5589c8",
                                "Africa" = "#eae754",     
                                "Europe" = "#4B0082")) +  
  labs(title = "Expectativa de Vida por Continente",      
       subtitle = "Análise da expectativa de vida ao longo dos anos sugere uma relação entre continente Africano\ne países de baixa renda e continente Europeu e países de alta renda.",  
       x = "Ano",                   
       y = "Expectativa de Vida",   
       color = "Continente") +
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

