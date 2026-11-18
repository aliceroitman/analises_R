# pacotes utilizados
pacotes <- c(
  "performance",
  "car",
  "emmeans",
  "ggplot2"
)

# listando os instalados e os faltantes
instalados <- rownames(installed.packages())
faltantes <- pacotes[!pacotes %in% instalados]

#instalando os faltantes
if (length(faltantes) > 0) {
  install.packages(faltantes)
}