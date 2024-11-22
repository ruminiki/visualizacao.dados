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

#carrega o dataset
dataset <- read.csv("data/world_data_raw.csv", sep = ",")

# Informações básicas do banco de dados
dim(dataset)
glimpse(dataset)

#nomes das colunas anos
anos <- colnames(dataset[,5:29])

#renomeia colunas que não são anos
dataset <- dataset %>% 
  rename(pais = 1,
         sigla = 2,
         serie = 3,
         codigo_serie = 4)

#seleciona apenas registros válidos
dataset <- dataset[1:4641,-c(4)]

#transforma as colunas ano uma coluna, repetindo o pais para cada ano
tmp <- pivot_longer(dataset,
                       cols = anos,
                       values_to = "valor")

tmp <- tmp %>% rename(ano = 4)

#transforma a serie em coluna
tmp <- pivot_wider(tmp,
                       id_cols = c("pais", "sigla", "ano"),
                       names_from = c("serie"),
                       values_from = "valor")

#padroniza o campo ano
tmp <- tmp %>% mutate(ano = substr(ano,2,5))

as.data.frame(colnames(tmp))

#renomear colunas
tmp <- tmp %>% rename(pib_corrente = 4,
                      pib_gasto_saude = 5,
                      pib_per_capita = 6,
                      pib_taxa_crescimento = 7,
                      pib_per_capita_crescimento = 8,
                      inflacao = 9,
                      expec_vida_mulheres = 10,
                      expec_vida_homens = 11,
                      expec_vida = 12,
                      desemprego_total = 13,
                      desemprego_ensino_superior = 14,
                      controle_corrupcao = 15,
                      acesso_energia_eletrica = 16,
                      cobertura_programas_sociais = 17,
                      acesso_agua_potavel = 18,
                      populacao_acima_65 = 19,
                      crescimento_populacao = 20,
                      densidade_populacional = 21,
                      populacao_total = 22,
                      populacao_urbana = 23,
                      populacao_rural = 24)


#converte para numérico
tmp <- tmp %>% mutate_at(c(4:24), as.numeric)

# Função para categorizar países em continentes
assign_continent <- function(country) {
  if (country %in% c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", "Brunei Darussalam", 
                     "Cambodia", "China", "Georgia", "India", "Indonesia", "Iran, Islamic Rep.", "Iraq", 
                     "Israel", "Japan", "Jordan", "Kazakhstan", "Korea, Dem. People's Rep.", "Korea, Rep.", 
                     "Kuwait", "Kyrgyz Republic", "Lao PDR", "Lebanon", "Malaysia", "Maldives", "Mongolia", 
                     "Myanmar", "Nepal", "Oman", "Pakistan", "Philippines", "Qatar", "Saudi Arabia", 
                     "Singapore", "Sri Lanka", "Syrian Arab Republic", "Tajikistan", "Thailand", "Timor-Leste", 
                     "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Viet Nam", "Yemen, Rep.", "Russian Federation")) {
    return("Asia")
  } else if (country %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", 
                            "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo, Dem. Rep.", 
                            "Congo, Rep.", "Djibouti", "Egypt, Arab Rep.", "Equatorial Guinea", "Eritrea", 
                            "Eswatini", "Ethiopia", "Gabon", "Gambia, The", "Ghana", "Guinea", "Guinea-Bissau", 
                            "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
                            "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", 
                            "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", 
                            "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", 
                            "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")) {
    return("Africa")
  } else if (country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
                            "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela, RB")) {
    return("South America")
  } else if (country %in% c("Curacao","Antigua and Barbuda", "Aruba", "Bahamas, The", "Barbados","Belize",
                            "Sint Maarten (Dutch part)", "St. Kitts and Nevis", "St. Lucia", "St. Martin (French part)",
                            "St. Vincent and the Grenadines","Virgin Islands (U.S.)","Bermuda","Dominica",
                            "Cayman Islands", "British Virgin Islands", "Cuba", "Dominican Republic", 
                            "Saint Kitts and Nevis", "Saint Lucia",  "Grenada","Jamaica","Haiti", 
                            "Turks and Caicos Islands", "Puerto Rico","Trinidad and Tobago",
                            "Saint Vincent and the Grenadines")) {
    return("Caribe")
  } else if (country %in% c("Canada", "Honduras", 
                            "Costa Rica", "El Salvador",
                            "Guatemala",  "Mexico", "Nicaragua",  
                            "Panama", "United States", "Greenland")) {
    return("North America")
  } else if (country %in% c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", 
                            "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Faroe Islands", 
                            "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", 
                            "Italy", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", 
                            "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", 
                            "Poland", "Portugal", "Romania", "San Marino", "Serbia", "Slovak Republic", 
                            "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")) {
    return("Europe")
  } else if (country %in% c("Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia, Fed. Sts.", 
                            "Nauru", "New Zealand", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", 
                            "Tonga", "Tuvalu", "Vanuatu", "American Samoa", "French Polynesia", "Guam", "New Caledonia", "Northern Mariana Islands")) {
    return("Oceania")
  } else if (country %in% c("World")) {
      return("World")
  } else if (country %in% c("Low income")) {
    return("Low income")
  } else if (country %in% c("High income")) {
    return("High income")
  } else if (country %in% c("Middle income")) {
    return("Middle income")
  } else {
   return("Other")
  }
}

# Aplicar a função aos países e criar o atributo continente
tmp$continente <- sapply(tmp$pais, assign_continent)

aux <- tmp %>% filter(continente == "Other")
unique(aux$pais)

tmp <- tmp %>% relocate(continente, .after="sigla")

#as.data.frame(unique(tmp$pais))

#salva o resultado em um novo arquivo
write.csv(tmp, "data/world_data.csv", fileEncoding = "UTF-8",  row.names = FALSE)
