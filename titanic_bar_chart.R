##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
library(tidyverse)
library(dplyr)
library(caret)

#carrega o dataset
titanic <- read.csv("data/titanic.csv", sep = ",")
glimpse(titanic)

titanic <- titanic %>% mutate(Survived = as.factor(ifelse(Survived=='0', 'Não', 'Sim')),
                              Pclass = as.factor(Pclass))

#Survival Rate by Passenger Class (Bar Chart)
#Acrescente título e subtítulo
#Ajuste rótulos dos eixo x e y
#Crie variações com barras empilhadas e agrupadas (position = "dodge/fill/stack")

ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar()

#duas barras lado a lado (sobreviveu e não sobreviveu)
ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar(position="dodge")

#escala em percentual, uma barra com 100%
ggplot(titanic, aes(x = factor(Pclass), fill = Survived)) +
  geom_bar(position = "fill") +  # "fill" makes it proportional, showing survival rate
  scale_y_continuous(labels = scales::percent)

ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "fill", width = 0.8) +  # "fill" makes it proportional, showing survival rate
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  
  # Cores mais intensas para a terceira classe e neutras para outras
  scale_fill_manual(values = c("Não" = "#d5d5d5", "Sim" = "#aad7ff")) +

  # Destaca o título e subtítulo para contexto claro sobre terceira classe
  labs(
    title = "Distribuição de Sobrevivência por Classe no Titanic",
    subtitle = "Apenas 25% dos passageiros da terceira classe sobreviveram. \nA baixa taxa de sobrevivência reflete tanto a localização no navio \nquanto as condições sociais e econômicas dos passageiros dessa classe.",
    x = "Classe do Passageiro",
    y = "Proporção Sobrevivência",
    fill = "Sobreviveu"
  ) +
  
  # Anotação destacando a terceira classe
  geom_rect(aes(xmin = 2.60, xmax = 3.40, ymin = 0.25, ymax = 1),
            fill = NA, color = "#6a6a6a", linetype = 'dashed', size = 0.11) +
  
  # Define o tema
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),               # Remove as linhas de grade horizontais para foco na classe
    panel.grid.minor.y = element_blank(),               # Remove grade menor
    axis.title = element_text(size = 14, face = "bold"),# Aumenta o tamanho da fonte dos títulos dos eixos
    axis.text = element_text(size = 12),                # Aumenta o tamanho da fonte dos rótulos dos eixos
    legend.position = "top",                            # Coloca a legenda no topo do gráfico
    legend.title = element_text(face = "bold")          # Destaca título da legenda
  )



