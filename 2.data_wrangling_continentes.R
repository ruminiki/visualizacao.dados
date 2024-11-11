##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################

library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(plotly)
library(jtools)
library(kableExtra)
library(equatiomatic)
library(correlation)
library(see)
library(ggraph)
library(PerformanceAnalytics)
library(nortest)
library(lavaan)
library(qgraph)
library(olsrr)
library(car)
library(mctest)
library(data.table)

#data from https://databank.worldbank.org/Indicadores-sociais-1999-a-2023/id/7cca906c#

#carrega o dataset
dataset <- read.csv("data/world_data.csv", sep = ",")

# Informações básicas do banco de dados
dim(dataset)
glimpse(dataset)

#nomes das colunas anos
anos <- colnames(dataset[,5:13])

#renomeia colunas que não são anos
dataset <- dataset %>% 
  rename(indicador = 1,
         continente = 3)

#seleciona apenas registros válidos
dataset <- dataset[1:720,-c(2,4)]

#transforma as colunas ano uma coluna, repetindo o pais para cada ano
tmp <- pivot_longer(dataset,
                       cols = anos,
                       values_to = "valor")

tmp <- tmp %>% rename(ano = 3)

#transforma a serie em coluna
tmp <- pivot_wider(tmp,
                       id_cols = c("continente", "ano"),
                       names_from = c("indicador"),
                       values_from = "valor")

#padroniza o campo ano
tmp <- tmp %>% mutate(ano = substr(ano,2,5))
tmp <- tmp[,-c(3)] #remove indicador repetido
#renomear colunas
tmp <- tmp %>% rename(continente = 1,
                      ano = 2,
                      pib_corrente = 3,
                      pib_taxa_crescimento = 4,
                      pib_per_capita = 5,
                      expec_anos_escolaridade = 6,
                      expec_vida_mulheres = 7,
                      expec_vida_homens = 8,
                      expec_vida_geral = 9,
                      forca_trabalho_ensino_basico = 10,
                      forca_trabalho_ensino_intermediario = 11,
                      forca_trabalho_ensino_superior = 12,
                      pib_gasto_educacao = 13,
                      desemprego_ensino_basico = 14,
                      desemprego_ensino_intermediario = 15,
                      desemprego_ensino_avancad = 16)

#converte para numérico
tmp <- tmp %>% mutate_at(c(3:14), as.numeric)

#salva o resultado em um novo arquivo
write.csv(tmp, "data/world_data.csv", fileEncoding = "UTF-8",  row.names = FALSE)

