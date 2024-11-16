library(ggplot2)
library(dplyr)

df <- read.csv("data/world_data.csv", sep=",")

df <- df %>% filter(continente %in% c('Europe & Central Asia', 'Africa Eastern and Southern', 'Latin America & Caribbean', 'North America', 'Pacific island small states'))

df %>% ggplot(aes(x = continente, y = pib_corrente, fill = continente)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "none")
