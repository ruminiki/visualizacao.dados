
df <- read.csv("data/world_data_by_country.csv", sep=",")

aux <- df %>% filter(sigla %in% c("USA", "PRY", "BRA"))

# Calculando a média total das populações
media_total <- mean(df$populacao_urbana)

# Criando o gráfico
ggplot(aux, aes(x = pais, y = populacao_urbana)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("populacao_urbana" = "#4E79A7", "populacao_urbana" = "#59A14F"),
                    name = "População") +
  geom_hline(yintercept = media_total, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Distribuição da População Urbana e Rural",
       subtitle = "Países da América Latina, EUA e Japão",
       x = "País",
       y = "População (milhões)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "top",
    plot.background = element_rect(fill = "#E8F5E9", color = NA),  # Fundo verde claro
    panel.background = element_rect(fill = "#E8F5E9", color = NA)  # Fundo verde claro para o painel
  )


aux <- df %>% head(50)
#salva o resultado em um novo arquivo
write.csv(aux, "data/world_data_sample.csv", fileEncoding = "UTF-8",  row.names = FALSE)

