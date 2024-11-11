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

ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "fill") +  # "fill" makes it proportional, showing survival rate
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  scale_fill_manual(values = c("0" = "#d5d5d5", "1" = "#57cbc3")) +
  coord_flip() +
  labs(
    title = "Distribuição de Sobrevivência por Classe no Titanic",
    subtitle = "Proporção mostra que 75% dos passageiros na terceira classe não sobreviveram.",
    x = "Sobrevivência",
    y = "Densidade",
    fill = "Sobreviveu") +
  theme_minimal()


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

