# =========================
# PALETA PRINCIPAL (Edward Hopper - lisa)
# =========================

pal_edward_hopper <- c(
  "dark_red"   = "#67161C",
  "green"      = "2F5D50#",
  "beige"      = "#DBD3A4",
  "gold"       = "#A4804C",
  "blue"       = "#2E4F73"
)

# =========================
# CORES PADRÃO DOS ELEMENTOS
# =========================

cor_histograma <- "#CED5DC"   
cor_histograma_tcl <- "#F1EDDE"
cor_histograma_comp <- "#DCE8DF"

cor_borda_hist <- "black"




cor_densidade_teorica <- pal_edward_hopper["blue"]
cor_media <- pal_edward_hopper["green"]
cor_discreta_pontos <- pal_edward_hopper["blue"]
cor_alerta <- pal_edward_hopper["dark_red"]
cor_densidade_teorica_tcl <- "#5A2A5A"



tema_base_graficos <- theme_light(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E6E6E6"),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )