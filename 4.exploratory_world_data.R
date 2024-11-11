library(ggplot2)
library(dplyr)
library(scales)

##################
# LOAD DATA
# Reference: 
##################


df <- read.csv("data/world_data.csv", sep=",")

df <- df %>% filter(continente %in% c('Europe & Central Asia', 'Africa Eastern and Southern', 'Latin America & Caribbean', 'North America', 'Pacific island small states'))

df %>% ggplot(aes(x = continente, y = pib_corrente, fill = continente)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "none")

10^log10(10000)

log10(50)
10 ^ 1.69897

log2(50)
2 ^ 5.64

print(format(1e+05, scientific = F))

colnames(df)


