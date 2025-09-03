if (!requireNamespace("ggimage", quietly = TRUE)) install.packages("ggimage")

library(ggimage)

dados$Imagem <- c(
  "C:/Users/aldre/Documents/tipster nash/inferno.webp",
  "C:/Users/aldre/Documents/tipster nash/nuke.webp",
  "C:/Users/aldre/Documents/tipster nash/mirage.webp",
  "C:/Users/aldre/Documents/tipster nash/anubis.webp",
  "C:/Users/aldre/Documents/tipster nash/dust2.webp"
)

library(ggimage)

grafico <- library(ggimage)

plotar_com_tamanho <- function(tamanho_imagem = 0.13) {
  ggplot(dados, aes(x = Porcentagem, y = reorder(Confronto, Porcentagem), fill = Cor)) +
    geom_col(width = 0.6) +
    geom_image(aes(image = Imagem), x = -5, size = tamanho_imagem, by = "width") +
    geom_text(aes(label = sprintf("%.1f%%", Porcentagem)),
              hjust = -0.1, family = "Poppins", size = 4, color = "white") +
    scale_fill_identity() +
    labs(
      title = "Qual mapa Liquid mais vence contra a FURIA?",
      subtitle = "Dados de confrontos diretos - 2024/2025",
      caption = "Fonte: @HLTVorg | @tipsternash",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_family = "Poppins") +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 11, color = "white"),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 16, face = "bold", color = "white", hjust = 0),
      plot.subtitle = element_text(size = 12, color = "white", hjust = 0),
      plot.caption = element_text(size = 8, color = "white", hjust = 0),
      plot.margin = margin(20, 20, 20, 50)
    ) +
    coord_cartesian(xlim = c(-10, 105))
}

grafico <- plotar_com_tamanho()  # salva o gráfico gerado numa variável

print(grafico)  # opcional: exibe o gráfico

ggsave(
  filename = "grafico_liquid_vs_furia.png",
  plot = grafico,
  width = 12,
  height = 8,
  dpi = 220,
  units = "cm"
)


# Salvar gráfico em arquivo PNG
ggsave(filename = "grafico_liquid_vs_furia.png", plot = grafico, width = 12, height = 8, dpi = 220, units = "cm")

list.files("C:/Users/aldre/Documents/tipster nash")


library(ggplot2)
library(ggimage)
library(dplyr)
library(showtext)

# Fonte Poppins para o gráfico
font_add_google("Poppins", "Poppins")
showtext_auto()

# Dados de rating médio por time e mapa
dados <- data.frame(
  Time = c(rep("Pain", 2), rep("MongolZ", 2)),
  Mapa = c("Dust 2", "Ancient", "Dust 2", "Ancient"),
  Rating_medio = c(
    mean(c(1.62, 1.16, 1.08, 0.93, 0.87)),  # Brazil Dust 2
    mean(c(1.21, 0.97, 0.89, 0.65, 0.59)),  # Brazil Ancient
    mean(c(1.39, 1.34, 0.95, 0.85, 0.70)),  # MongolZ Dust 2
    mean(c(1.69, 1.33, 1.23, 1.17, 0.81))   # MongolZ Ancient
  )
)

# Criar uma coluna para o eixo Y que junta time + mapa
dados$Confronto <- paste(dados$Time, dados$Mapa, sep = " - ")

# Cores para times
dados$Cor <- ifelse(dados$Time == "Pain", "#E95429", "#D9D9D9") # Verde Brasil e Roxo MongolZ

# Caminho das imagens dos mapas (relacionado aos mapas)
mapas_imgs <- c(
  "Dust 2" = "C:/Users/aldre/Documents/tipster nash/dust2.webp",
  "Ancient" = "C:/Users/aldre/Documents/tipster nash/anubis.webp"  # Usando anubis porque não enviou Ancient?
)

# Adicionar coluna Imagem baseada no mapa
dados$Imagem <- mapas_imgs[dados$Mapa]

plotar_com_tamanho <- function(tamanho_imagem = 0.10) {
  ggplot(dados, aes(x = Rating_medio, y = reorder(Confronto, Rating_medio), fill = Cor)) +
    geom_col(width = 0.6) +
    geom_image(aes(image = Imagem), x = 0, size = tamanho_imagem, by = "width") +
    geom_text(aes(label = sprintf("%.2f", Rating_medio)),
              hjust = -0.1, family = "Poppins", size = 4, color = "white") +
    scale_fill_identity() +
    labs(
      title = "Rating Médio por Time e Mapa",
      subtitle = "Partidas Dust 2 e Ancient",
      caption = "Fonte: @HLTVorg | @tipsternash",
      x = "Rating Médio",
      y = NULL
    ) +
    theme_minimal(base_family = "Poppins") +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(color = "white"),
      axis.text.y = element_text(size = 12, color = "white"),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 18, face = "bold", color = "white", hjust = 0),
      plot.subtitle = element_text(size = 12, color = "white", hjust = 0),
      plot.caption = element_text(size = 10, color = "white", hjust = 0, lineheight = 1.2),
      plot.margin = margin(20, 20, 40, 50)  # aumentar margem inferior para o caption
    ) +
    coord_cartesian(xlim = c(0, 2))
}

grafico <- plotar_com_tamanho()
print(grafico)


# Salvar gráfico
ggsave("C:/Users/aldre/Documents/tipster nash/rating_times_mapas.png",
       plot = grafico, width = 12, height = 8, dpi = 220, units = "cm")


library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)

# Fonte Poppins
font_add_google("Poppins", "Poppins")
showtext_auto()

# Dados consolidados (exemplo com Dust 2 + Ancient)
# Inclua aqui todos os jogadores e dados dos dois mapas, ajustando se quiser incluir "Mapa" na análise.

dados_raw <- data.frame(
  Player = c(
    "João 'snow' Vinicius", "David 'dav1deuS' Tapia Maldonado", "Lucas 'nqz' Soares",
    "Franco 'dgt' Garcia", "Rodrigo 'biguzera' Bittencourt",
    "Sodbayar 'Techno' Munkhbold", "Usukhbayar '910' Banzragch", "Azbayar 'Senzu' Munkhbold",
    "Garidmagnai 'bLitz' Byambasuren", "Ayush 'mzinho' Batbold",
    "João 'snow' Vinicius", "David 'dav1deuS' Tapia Maldonado", "Lucas 'nqz' Soares",
    "Franco 'dgt' Garcia", "Rodrigo 'biguzera' Bittencourt",
    "Garidmagnai 'bLitz' Byambasuren", "Azbayar 'Senzu' Munkhbold", "Sodbayar 'Techno' Munkhbold",
    "Usukhbayar '910' Banzragch", "Ayush 'mzinho' Batbold"
  ),
  Mapa = rep(c("Dust 2", "Ancient"), each = 10),
  Kills = c(
    24, 20, 14, 15, 14, 25, 20, 16, 13, 8,
    13, 22, 16, 12, 6, 23, 16, 20, 17, 7
  ),
  KD_Diff = c(
    11, 3, 1, -3, -7, 8, 5, -1, -6, -11,
    0, 4, -3, -8, -8, 10, 1, 5, 3, -7
  ),
  ADR = c(
    105.4, 73.1, 74.7, 71.9, 73.3, 105.0, 90.3, 69.5, 61.8, 47.0,
    63.0, 104.5, 79.7, 60.7, 50.4, 98.3, 93.9, 80.0, 82.4, 45.0
  ),
  KAST = c(
    83.3, 79.2, 75.0, 79.2, 62.5, 75.0, 79.2, 75.0, 62.5, 66.7,
    60.9, 69.6, 65.2, 60.9, 60.9, 91.3, 91.3, 82.6, 73.9, 87.0
  ),
  Rating = c(
    1.62, 1.16, 1.08, 0.93, 0.87, 1.39, 1.34, 0.95, 0.85, 0.70,
    0.89, 1.21, 0.97, 0.65, 0.59, 1.69, 1.33, 1.23, 1.17, 0.81
  )
)

# Selecionando só categorias que vamos plotar (por exemplo Kills, KD_Diff e Rating)
# Se quiser incluir rounds vencidos, precisa adicionar essa coluna ao dados_raw

dados_long <- dados_raw %>%
  select(Player, Mapa, Kills, KD_Diff, Rating) %>%
  pivot_longer(cols = c(Kills, KD_Diff, Rating), names_to = "Categoria", values_to = "Valor") %>%
  # Somar os valores por jogador e categoria considerando ambos mapas juntos
  group_by(Player, Categoria) %>%
  summarise(Valor = sum(Valor), .groups = "drop")

# Calcular o total por jogador somando todas as categorias para label
dados_agregado <- dados_long %>%
  group_by(Player) %>%
  summarise(Total = sum(Valor)) %>%
  ungroup()

# Paleta de cores (você pode ajustar as cores aqui)
cores <- c(
  "Kills" = "#E84A5F",
  "KD_Diff" = "#7367F0",
  "Rating" = "#EAEAEA"
)

# Plot mantendo seu tema e estética
ggplot(dados_long, aes(x = Valor, y = reorder(Player, Valor), fill = Categoria)) +
  geom_col(position = "stack", width = 0.7, color = "black", linewidth = 0.2) +
  geom_text(data = dados_agregado,
            aes(x = Total + max(Total)*0.02, y = Player, label = round(Total, 2)),
            inherit.aes = FALSE, size = 3, family = "Poppins", hjust = 0) +
  scale_fill_manual(values = cores) +
  labs(
    title = "Atuação em Mapas - CS2",
    subtitle = "Kills, KD Diff e Rating (soma dos mapas Dust 2 + Ancient)",
    x = "Quantidade / Valor",
    y = NULL,
    fill = NULL,
    caption = "Fonte: HLTV / DataViz @tipsternash"
  ) +
  theme_minimal(base_family = "Poppins") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9),
    legend.position = "bottom"
  ) +
  coord_cartesian(clip = "off") 
# Pacotes
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("showtext", quietly = TRUE)) install.packages("showtext")

library(tidyverse)
library(showtext)

# Fonte Poppins
font_add_google("Poppins", "Poppins")
showtext_auto()

# ============================
# Dados combinados
# ============================
dados <- tribble(
  # Overpass
  ~Mapa, ~Time, ~Player, ~Kills, ~KD_Diff, ~Rating,
  "Overpass", "CYBERSHOKE", "Denis 'notineki' Kalachev", 19,  2, 1.14,
  "Overpass", "CYBERSHOKE", "Ilya 'FenomeN' Kolodko",    19, -1, 1.12,
  "Overpass", "CYBERSHOKE", "Igor 'Forester' Bezotecheskiy", 23,  2, 1.11,
  "Overpass", "CYBERSHOKE", "Aleksandr 'glowiing' Matsievich", 18, -4, 1.02,
  "Overpass", "CYBERSHOKE", "David 'bl1x1' Stepanyants",  19,  0, 0.91,
  "Overpass", "BC.Game",    "Aleksandar 'CacaNito' Kjulukoski", 29,  8, 1.44,
  "Overpass", "BC.Game",    "Nemanja 'nexa' Isaković",          18, -3, 1.13,
  "Overpass", "BC.Game",    "Andreas 'aNdu' Maasing",           19, -2, 0.92,
  "Overpass", "BC.Game",    "Luca 'pr1metapz' Voigt",           16, -2, 0.90,
  "Overpass", "BC.Game",    "Oleksandr 's1mple' Kostyliev",     17, -1, 0.89,
  
  # Mirage
  "Mirage", "CYBERSHOKE", "Aleksandr 'glowiing' Matsievich", 27, -1, 1.10,
  "Mirage", "CYBERSHOKE", "David 'bl1x1' Stepanyants",       26, -1, 0.98,
  "Mirage", "CYBERSHOKE", "Ilya 'FenomeN' Kolodko",          23, -2, 0.97,
  "Mirage", "CYBERSHOKE", "Igor 'Forester' Bezotecheskiy",   20, -6, 0.94,
  "Mirage", "CYBERSHOKE", "Denis 'notineki' Kalachev",       24, -1, 0.92,
  "Mirage", "BC.Game",    "Andreas 'aNdu' Maasing",          31,  7, 1.37,
  "Mirage", "BC.Game",    "Luca 'pr1metapz' Voigt",          28,  7, 1.18,
  "Mirage", "BC.Game",    "Oleksandr 's1mple' Kostyliev",    29,  3, 1.13,
  "Mirage", "BC.Game",    "Aleksandar 'CacaNito' Kjulukoski",24, -4, 1.08,
  "Mirage", "BC.Game",    "Nemanja 'nexa' Isaković",         18, -3, 1.01,
  
  # Ancient
  "Ancient", "CYBERSHOKE", "Igor 'Forester' Bezotecheskiy",  16,  3, 1.18,
  "Ancient", "CYBERSHOKE", "Denis 'notineki' Kalachev",      14,  2, 1.04,
  "Ancient", "CYBERSHOKE", "Aleksandr 'glowiing' Matsievich",12, -3, 0.84,
  "Ancient", "CYBERSHOKE", "Ilya 'FenomeN' Kolodko",         11, -3, 0.80,
  "Ancient", "CYBERSHOKE", "David 'bl1x1' Stepanyants",       9, -3, 0.67,
  "Ancient", "BC.Game",    "Nemanja 'nexa' Isaković",        14,  5, 1.17,
  "Ancient", "BC.Game",    "Aleksandar 'CacaNito' Kjulukoski",17, 0, 1.08,
  "Ancient", "BC.Game",    "Oleksandr 's1mple' Kostyliev",   10,  0, 1.03,
  "Ancient", "BC.Game",    "Luca 'pr1metapz' Voigt",         12, -1, 0.99,
  "Ancient", "BC.Game",    "Andreas 'aNdu' Maasing",         13,  0, 0.93
)

# ============================
# Transformar para formato longo
# ============================
dados_long <- dados %>%
  pivot_longer(cols = c(Kills, KD_Diff, Rating),
               names_to = "Categoria",
               values_to = "Valor")

# Calcular totais para os labels
dados_agregado <- dados_long %>%
  group_by(Mapa, Player) %>%
  summarise(Total = sum(Valor), .groups = "drop")

# Paleta de cores
cores <- c(
  "Kills" = "#E84A5F",
  "KD_Diff" = "#7367F0",
  "Rating" = "#EAEAEA"
)

# ============================
# Plot
# ============================
figura_3 = ggplot(dados_long, aes(x = Valor, y = reorder(Player, Valor), fill = Categoria)) +
  geom_col(position = "stack", width = 0.7, color = "black", linewidth = 0.2) +
  geom_text(data = dados_agregado,
            aes(x = Total + max(Total)*0.02, y = Player, label = round(Total, 2)),
            inherit.aes = FALSE, size = 3, family = "Poppins", hjust = 0) +
  scale_fill_manual(values = cores) +
  labs(
    title = "Atuação por Mapa - CS2",
    subtitle = "Kills, KD Diff e Rating - Overpass, Mirage e Ancient",
    x = "Quantidade / Valor",
    y = NULL,
    fill = NULL,
    caption = "Fonte: HLTV / DataViz @tipsternash"
  ) +
  theme_minimal(base_family = "Poppins") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(size = 8),
    legend.position = "bottom"
  ) +
  coord_cartesian(clip = "off")

# Salvar gráfico
ggsave("C:/Users/aldre/Documents/tipster nash/rating_times_mapas.png",
       plot = figura_3, width = 14, height = 8, dpi = 220, units = "cm", bg="white")

# Pacotes
library(tidyverse)
library(showtext)

font_add_google("Poppins", "Poppins")
showtext_auto()

# Dados
dados <- tribble(
  # Overpass
  ~Mapa, ~Time, ~Player, ~Kills, ~KD_Diff, ~Rating,
  "Overpass", "CYBERSHOKE", "Denis 'notineki' Kalachev", 19,  2, 1.14,
  "Overpass", "CYBERSHOKE", "Ilya 'FenomeN' Kolodko",    19, -1, 1.12,
  "Overpass", "CYBERSHOKE", "Igor 'Forester' Bezotecheskiy", 23,  2, 1.11,
  "Overpass", "CYBERSHOKE", "Aleksandr 'glowiing' Matsievich", 18, -4, 1.02,
  "Overpass", "CYBERSHOKE", "David 'bl1x1' Stepanyants",  19,  0, 0.91,
  "Overpass", "BC.Game",    "Aleksandar 'CacaNito' Kjulukoski", 29,  8, 1.44,
  "Overpass", "BC.Game",    "Nemanja 'nexa' Isaković",          18, -3, 1.13,
  "Overpass", "BC.Game",    "Andreas 'aNdu' Maasing",           19, -2, 0.92,
  "Overpass", "BC.Game",    "Luca 'pr1metapz' Voigt",           16, -2, 0.90,
  "Overpass", "BC.Game",    "Oleksandr 's1mple' Kostyliev",     17, -1, 0.89,
  
  # Mirage
  "Mirage", "CYBERSHOKE", "Aleksandr 'glowiing' Matsievich", 27, -1, 1.10,
  "Mirage", "CYBERSHOKE", "David 'bl1x1' Stepanyants",       26, -1, 0.98,
  "Mirage", "CYBERSHOKE", "Ilya 'FenomeN' Kolodko",          23, -2, 0.97,
  "Mirage", "CYBERSHOKE", "Igor 'Forester' Bezotecheskiy",   20, -6, 0.94,
  "Mirage", "CYBERSHOKE", "Denis 'notineki' Kalachev",       24, -1, 0.92,
  "Mirage", "BC.Game",    "Andreas 'aNdu' Maasing",          31,  7, 1.37,
  "Mirage", "BC.Game",    "Luca 'pr1metapz' Voigt",          28,  7, 1.18,
  "Mirage", "BC.Game",    "Oleksandr 's1mple' Kostyliev",    29,  3, 1.13,
  "Mirage", "BC.Game",    "Aleksandar 'CacaNito' Kjulukoski",24, -4, 1.08,
  "Mirage", "BC.Game",    "Nemanja 'nexa' Isaković",         18, -3, 1.01,
  
  # Ancient
  "Ancient", "CYBERSHOKE", "Igor 'Forester' Bezotecheskiy",  16,  3, 1.18,
  "Ancient", "CYBERSHOKE", "Denis 'notineki' Kalachev",      14,  2, 1.04,
  "Ancient", "CYBERSHOKE", "Aleksandr 'glowiing' Matsievich",12, -3, 0.84,
  "Ancient", "CYBERSHOKE", "Ilya 'FenomeN' Kolodko",         11, -3, 0.80,
  "Ancient", "CYBERSHOKE", "David 'bl1x1' Stepanyants",       9, -3, 0.67,
  "Ancient", "BC.Game",    "Nemanja 'nexa' Isaković",        14,  5, 1.17,
  "Ancient", "BC.Game",    "Aleksandar 'CacaNito' Kjulukoski",17,  0, 1.08,
  "Ancient", "BC.Game",    "Oleksandr 's1mple' Kostyliev",   10,  0, 1.03,
  "Ancient", "BC.Game",    "Luca 'pr1metapz' Voigt",         12, -1, 0.99,
  "Ancient", "BC.Game",    "Andreas 'aNdu' Maasing",         13,  0, 0.93
)

# Cores
cores <- c("CYBERSHOKE" = "#283618", "BC.Game" = "#ffb703")

# Transformar Rating em positivo/negativo
dados_plot <- dados %>%
  mutate(
    Rating_adj = ifelse(Time == "BC.Game", -Rating, Rating),
    Ordem = row_number()
  )

# Gráfico no estilo The Athletic
figura_4  = ggplot(dados_plot, aes(x = Ordem, y = Rating_adj, fill = Time)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = cores) +
  geom_hline(yintercept = 0, color = "gray80", linewidth = 0.4) +
  facet_wrap(~Mapa, scales = "free_x", ncol = 1) +
  labs(
    title = "Comparativo de Rating — CYBERSHOKE vs BC.Game",
    subtitle = "Valores positivos = CYBERSHOKE, negativos = BC.Game",
    y = "Rating ajustado"
  ) +
  theme_minimal(base_family = "Poppins") +
  theme(
    plot.background = element_rect(fill = "#F9F8F4", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    axis.title.x = element_blank(),
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13, colour = "gray40")
  )
# Salvar gráfico
ggsave("C:/Users/aldre/Documents/tipster nash/rating_times_mapas.png",
       plot = figura_4, width = 10, height = 14, dpi = 220, units = "cm", bg="white")

library(tidyverse)
library(showtext)
library(patchwork)

# 1. Fonte
font_add_google("Poppins", "poppins")
showtext_auto()

# 2. Dados (seu tribble)
dados <- tribble(
  # Overpass
  ~Mapa, ~Time, ~Player, ~Kills, ~KD_Diff, ~Rating,
  "Overpass", "CYBERSHOKE", "Denis 'notineki' Kalachev", 19,  2, 1.14,
  "Overpass", "CYBERSHOKE", "Ilya 'FenomeN' Kolodko",    19, -1, 1.12,
  "Overpass", "CYBERSHOKE", "Igor 'Forester' Bezotecheskiy", 23,  2, 1.11,
  "Overpass", "CYBERSHOKE", "Aleksandr 'glowiing' Matsievich", 18, -4, 1.02,
  "Overpass", "CYBERSHOKE", "David 'bl1x1' Stepanyants",  19,  0, 0.91,
  "Overpass", "BC.Game",    "Aleksandar 'CacaNito' Kjulukoski", 29,  8, 1.44,
  "Overpass", "BC.Game",    "Nemanja 'nexa' Isaković",          18, -3, 1.13,
  "Overpass", "BC.Game",    "Andreas 'aNdu' Maasing",           19, -2, 0.92,
  "Overpass", "BC.Game",    "Luca 'pr1metapz' Voigt",           16, -2, 0.90,
  "Overpass", "BC.Game",    "Oleksandr 's1mple' Kostyliev",     17, -1, 0.89,
  
  # Mirage
  "Mirage", "CYBERSHOKE", "Aleksandr 'glowiing' Matsievich", 27, -1, 1.10,
  "Mirage", "CYBERSHOKE", "David 'bl1x1' Stepanyants",       26, -1, 0.98,
  "Mirage", "CYBERSHOKE", "Ilya 'FenomeN' Kolodko",          23, -2, 0.97,
  "Mirage", "CYBERSHOKE", "Igor 'Forester' Bezotecheskiy",   20, -6, 0.94,
  "Mirage", "CYBERSHOKE", "Denis 'notineki' Kalachev",       24, -1, 0.92,
  "Mirage", "BC.Game",    "Andreas 'aNdu' Maasing",          31,  7, 1.37,
  "Mirage", "BC.Game",    "Luca 'pr1metapz' Voigt",          28,  7, 1.18,
  "Mirage", "BC.Game",    "Oleksandr 's1mple' Kostyliev",    29,  3, 1.13,
  "Mirage", "BC.Game",    "Aleksandar 'CacaNito' Kjulukoski",24, -4, 1.08,
  "Mirage", "BC.Game",    "Nemanja 'nexa' Isaković",         18, -3, 1.01,
  
  # Ancient
  "Ancient", "CYBERSHOKE", "Igor 'Forester' Bezotecheskiy",  16,  3, 1.18,
  "Ancient", "CYBERSHOKE", "Denis 'notineki' Kalachev",      14,  2, 1.04,
  "Ancient", "CYBERSHOKE", "Aleksandr 'glowiing' Matsievich",12, -3, 0.84,
  "Ancient", "CYBERSHOKE", "Ilya 'FenomeN' Kolodko",         11, -3, 0.80,
  "Ancient", "CYBERSHOKE", "David 'bl1x1' Stepanyants",       9, -3, 0.67,
  "Ancient", "BC.Game",    "Nemanja 'nexa' Isaković",        14,  5, 1.17,
  "Ancient", "BC.Game",    "Aleksandar 'CacaNito' Kjulukoski",17,  0, 1.08,
  "Ancient", "BC.Game",    "Oleksandr 's1mple' Kostyliev",   10,  0, 1.03,
  "Ancient", "BC.Game",    "Luca 'pr1metapz' Voigt",         12, -1, 0.99,
  "Ancient", "BC.Game",    "Andreas 'aNdu' Maasing",         13,  0, 0.93
)

# 3. Função para gráfico de rosca por mapa
grafico_rosca_mapa <- function(df, mapa_nome) {
  df %>%
    filter(Mapa == mapa_nome) %>%                              # 3.1 Filtra mapa
    group_by(Time) %>%                                         # 3.2 Agrupa por time
    summarise(Total_Kills = sum(Kills)) %>%                   # 3.3 Soma kills por time
    mutate(
      frac = Total_Kills / sum(Total_Kills),                   # 3.4 Proporção kills
      ymax = cumsum(frac),                                     # 3.5 Limite superior do setor
      ymin = lag(ymax, default = 0)                            # 3.6 Limite inferior do setor
    ) %>%
    ggplot(aes(ymax = ymax, ymin = ymin,                        # 3.7 Mapeia para gráfico
               xmax = 4, xmin = 3, fill = Time)) +
    geom_rect(color = "white") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    theme_void() +
    scale_fill_manual(values = c("CYBERSHOKE" = "#283618", "BC.Game" = "#ffb703")) +
    annotate("text", x = 0, y = 0, label = paste0("Kills\n", sum(df$Kills)),  # 3.8 Texto centro
             family = "poppins", size = 5, fontface = "bold") +
    labs(title = mapa_nome) +
    theme(
      plot.title = element_text(family = "poppins", size = 14, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(family = "poppins", size = 12)
    )
}

# 4. Criar os 3 gráficos
g_overpass <- grafico_rosca_mapa(dados, "Overpass")  # 4.1 Gráfico Overpass
g_mirage <- grafico_rosca_mapa(dados, "Mirage")      # 4.2 Gráfico Mirage
g_ancient <- grafico_rosca_mapa(dados, "Ancient")    # 4.3 Gráfico Ancient

# 5. Combinar gráficos com patchwork
figura_5 = g_overpass + g_mirage + g_ancient +
  plot_annotation(
    title = "Kills por Time em Cada Mapa",                   # 5.1 Título geral
    subtitle = "Distribuição proporcional de kills entre CYBERSHOKE e BC.Game", # 5.2 Subtítulo
    caption = "Fonte: HLTV / DataViz @tipsternash",      # 5.3 Fonte/caption
    theme = theme(
      plot.title = element_text(family = "poppins", size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(family = "poppins", size = 14, hjust = 0.5),
      plot.caption = element_text(family = "poppins", size = 12, color = "grey40", hjust = 1)
    )
  )
figura_5
# Salvar gráfico
ggsave("C:/Users/aldre/Documents/tipster nash/rating_times_mapas.png",
       plot = figura_5, width = 10, height = 14, dpi = 220, units = "cm", bg="white")
# Pacotes
library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)

# Fonte parecida com a usada no The Athletic
font_add_google("Roboto Condensed", "athletic")
showtext_auto()

# Dados
df <- tribble(
  ~metric,                ~Frankfurt, ~Palace,
  "Chance prevention",     56,         38,
  "Intensity",             63,         54,
  "High line",             51,         34,
  "Deep build-up",         64,         13,
  "Press resistance",      57,          3,
  "Possession",            72,          8,
  "Central progression",   66,         50,
  "Circulate",             56,         41,
  "Field tilt",            71,         15,
  "Patient attack",        83,         62,
  "Shot quality",          92,         26,
  "Chance creation",       74,         28
)

# Categorias para cores
category_colors <- c(
  "Chance prevention" = "#8B2F3D", # Defesa
  "Intensity"         = "#8B2F3D",
  "High line"         = "#8B2F3D",
  "Deep build-up"     = "#0F5B5C", # Posse
  "Press resistance"  = "#0F5B5C",
  "Possession"        = "#0F5B5C",
  "Central progression" = "#F4A300", # Construção
  "Circulate"           = "#F4A300",
  "Field tilt"          = "#F4A300",
  "Patient attack"      = "#4F81BD", # Ataque
  "Shot quality"        = "#4F81BD",
  "Chance creation"     = "#4F81BD"
)

# Converter Frankfurt para negativo para espelhamento
df_long <- df %>%
  mutate(Frankfurt = -Frankfurt) %>%
  pivot_longer(cols = c(Frankfurt, Palace),
               names_to = "Team",
               values_to = "Value") %>%
  mutate(
    metric = factor(metric, levels = rev(df$metric))
  )

# Texto da legenda de categorias
category_text <- "DEFENCE: Chance prevention Non-pen GA p90 | Intensity: Opp. touches per tackle | High line: Offsides + thr balls ag. + GK sweeper per opp. pass into att. 3rd\nPOSSESSION: Deep build-up: Inverted GK launch rate | Press resistance: Touches per opp. tackles (first two-thirds) | Possession: Share of passes attempted\nPROGRESSION: Central progression: Touches per opp. tackles per opp. touch in mid 3rd | Circulate: Passes as share of total | Field tilt: Share of final third passes\nATTACK: Patient attack: Shots per 100 final third touches | Shot quality: Non-pen xG per shot | Chance creation: Non-pen xG p90"

# Gráfico
ggplot(df_long, aes(x = Value, y = metric, fill = metric)) +
  # Linha central
  geom_vline(xintercept = 0, color = "grey50", size = 1) +
  
  # Barras
  geom_col(width = 0.6) +
  
  # Números dentro das barras
  geom_text(aes(label = abs(Value)),
            position = position_nudge(x = ifelse(df_long$Value < 0, -4, 4)),
            color = "white",
            family = "athletic",
            size = 4) +
  
  # Cores
  scale_fill_manual(values = category_colors) +
  
  # Escala X simétrica sem espaço morto
  scale_x_continuous(
    limits = c(-95, 95),
    breaks = seq(-90, 90, 30),
    labels = abs
  ) +
  
  # Títulos
  labs(
    title = "How does 2022-23 Eintracht Frankfurt compare with 2023-24 Crystal Palace?",
    subtitle = "Team percentiles per season compared with top seven European Leagues",
    caption = category_text
  ) +
  
  # Tema
  theme_minimal(base_family = "athletic") +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 7, hjust = 0, color = "grey30")
  )
library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)

font_add_google("Roboto Condensed", "athletic")
showtext_auto()

# Agrupar kills por Mapa e Time
df_kills <- dados %>%
  group_by(Mapa, Time) %>%
  summarise(Kills = sum(Kills), .groups = "drop") %>%
  # Negativar kills do time "CYBERSHOKE" para espelhamento
  mutate(Kills = ifelse(Time == "CYBERSHOKE", -Kills, Kills)) %>%
  mutate(Mapa = factor(Mapa, levels = rev(unique(Mapa))))

# Cores para times
team_colors <- c(
  "CYBERSHOKE" = "#8B2F3D",  # cor exemplo para time 1
  "BC.Game" = "#F4A300"      # cor exemplo para time 2
)

ggplot(df_kills, aes(x = Kills, y = Mapa, fill = Time)) +
  geom_vline(xintercept = 0, color = "grey50", size = 1) +
  geom_col(width = 0.6) +
  geom_text(aes(label = abs(Kills)),
            position = position_nudge(x = ifelse(df_kills$Kills < 0, -4, 4)),
            color = "white",
            family = "athletic",
            size = 4) +
  scale_fill_manual(values = team_colors) +
  scale_x_continuous(
    limits = c(-max(abs(df_kills$Kills)) - 10, max(abs(df_kills$Kills)) + 10),
    breaks = seq(-max(abs(df_kills$Kills)), max(abs(df_kills$Kills)), by = 10),
    labels = abs
  ) +
  labs(
    title = "Kills totais por time e mapa - CS:GO",
    subtitle = "Comparação entre CYBERSHOKE e BC.Game",
    caption = "Fonte: dados de partidas Overpass, Mirage e Ancient"
  ) +
  theme_minimal(base_family = "athletic") +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 7, hjust = 0, color = "grey30")
  )

library(readr)
library(tidyverse)
kills <- read_csv("C:/Users/aldre/demo-parser/kills.csv")
glimpse(kills)
View(kills)


library(ggplot2) 
library(dplyr)
library(readr)
library(showtext)

# Fonte estilo The Athletic
font_add_google("Rajdhani", "optatext")
showtext_auto()

# Carregar seus dados
df <- read_csv("data/seus_dados.csv")

# Paleta de cores
category_colors <- c(
  "Defence" = "#A33D5A",
  "Possession" = "#00706E",
  "Progression" = "#E59E28",
  "Attack" = "#0D62A3"
)

# Texto de rodapé
footnote_text <- paste(
  "DEFENCE: Chance prevention: Non-pen xGA p90 | Intensity: Opp. touches per tackle | High line: Offsides + thr balls ag + GK sweeper per opp. pass into att. 3rd",
  "\nPOSSESSION: Deep build-up: Inversed GK launch rate | Press resistance: Touches per opp. tackles (first two-thirds) | Possession: Share of passes attempted",
  "\nPROGRESSION: Central progression: Inversed crosses per 100 passes | Circulate: Inversed prog. distance as share of total | Field tilt: Share of final third passes",
  "\nATTACK: Patient attack: Shots per 100 final third touches | Shot quality: Non-pen xG per shot | Chance creation: Non-pen xG p90"
)

# Criar gráfico com escalas independentes
p <- ggplot(df, aes(x = season, y = value, group = metric, color = category)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = value), vjust = -1, family = "optatext", size = 3.5) +
  facet_wrap(~ metric, scales = "free_y", ncol = 3) + # Escalas independentes
  scale_color_manual(values = category_colors) +
  theme_minimal(base_family = "optatext") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey30")
  ) +
  labs(
    title = "Variação por Métrica ao Longo das Temporadas",
    subtitle = "Percentis por temporada comparados com ligas de elite",
    caption = footnote_text,
    y = NULL, x = NULL
  )

# Salvar imagem
ggsave("output/variacao_metricas.png", p, width = 10, height = 12, dpi = 300)
