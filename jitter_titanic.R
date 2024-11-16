library(ggplot2)
library(dplyr)
library(scales)

##################
# LOAD DATA
# Reference: 
##################


df <- read.csv("data/titanic.csv", sep=",")

df <- df %>% mutate(Survived = as.factor(Survived))

df %>% ggplot(aes(x=Survived, y=Age, fill=Age))+
  geom_jitter(width = 0.3)
