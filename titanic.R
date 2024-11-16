##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
library(tidyverse)
library(dplyr)
library(caret)

#carrega o dataset
titanic <- read.csv("data/titanic.csv", sep = ",")
glimpse(titanic)

#Survival Rate by Passenger Class (Bar Chart)
#Acrescente título e subtítulo
#Ajuste rótulos dos eixo x e y
#Crie variações com barras empilhadas e agrupadas (position = "dodge/fill/stack")

titanic <- titanic %>% mutate(Survived = as.factor(ifelse(Survived=='0', 'Não', 'Sim')),
                              Pclass = as.factor(Pclass))

ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar(position="dodge") +  # "fill" makes it proportional, showing survival rate

  # Destaca o título e subtítulo para contexto claro sobre terceira classe
  labs(
    title = "Distribuição de Sobrevivência por Classe no Titanic",
    x = "Classe do Passageiro",
    y = "Proporção Sobrevivência",
    fill = "Sobreviveu"
  ) +
  
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




ggplot(titanic, aes(x = factor(Survived), y = Age, fill = Sex)) +
  #geom_violin() +
  geom_boxplot() +
  labs(x = "Survived", y = "Age") +
  theme_minimal()


ggplot(titanic, aes(x=Sex, y=Age, fill=Survived)) +
  geom_bar(stat="identity", position="fill")


# Filtrar e modificar os dados para garantir variáveis categóricas corretas
df <- titanic %>% 
  filter(!is.na(Survived)) %>%                 # Remover valores NA na coluna Survived
  mutate(Survived = factor(Survived,           # Transformar Survived em fator
                           labels = c("Did not survive", "Survived")),
         Sex = factor(Sex))  

ggplot(titanic, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.4, adjust = 1.5) +    # Definir transparência e suavização
  facet_grid(~Survi)
  labs(
    title = "Distribuição de Sobrevivência por Sexo no Titanic",
    x = "Sobrevivência",
    y = "Densidade",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "top"
  )

