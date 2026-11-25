# Figuras de la etapa 03. Con ggplot2 se exportan versiones de publicación.
etiquetas <- c(ALGODON = "Algodón", ARROZ = "Arroz", CACAO = "Cacao", CAFE = "Café",
               "CAÑA PANELERA" = "Caña panelera", FRIJOL = "Fríjol", MAIZ = "Maíz",
               "PALMA DE ACEITE" = "Palma de aceite", PLATANO = "Plátano", YUCA = "Yuca")
etiqueta <- function(x) ifelse(x %in% names(etiquetas), unname(etiquetas[x]), x)
violeta <- "#6D45A6"
ocre <- "#AC6330"
gris <- "#383044"
puntos <- anual[is.finite(anual$rendimiento_t_ha), ]
puntos$cultivo <- factor(etiqueta(puntos$cultivo),
                        levels = etiqueta(names(sort(tapply(puntos$rendimiento_t_ha, puntos$cultivo, median)))))
extremos <- puntos[puntos$atipico_severo, ]
trayectorias$cultivo <- etiqueta(trayectorias$cultivo)
cobertura$cultivo <- etiqueta(cobertura$cultivo)
cobertura <- merge(expand.grid(ano = 2007:2018, cultivo = unique(cobertura$cultivo)),
                   cobertura, by = c("ano", "cultivo"), all.x = TRUE)
cobertura$rendimiento_valido[is.na(cobertura$rendimiento_valido)] <- 0
trayectorias <- merge(cobertura[c("ano", "cultivo")], trayectorias, by = c("ano", "cultivo"), all.x = TRUE)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  tema <- theme_minimal(base_size = 12) + theme(
    plot.title = element_text(face = "bold", colour = gris),
    plot.subtitle = element_text(colour = gris, margin = margin(b = 10)),
    panel.grid.minor = element_blank(), legend.position = "bottom",
    strip.text = element_text(face = "bold"), plot.background = element_rect(fill = "white", colour = NA))
  guardar <- function(g, nombre, ancho = 10, alto = 5.2) {
    ggsave(file.path(ruta_figuras, paste0("eva_R_", nombre, ".png")), g + tema,
           width = ancho, height = alto, dpi = 190)
  }

  cajas <- ggplot(puntos, aes(rendimiento_t_ha, cultivo)) +
    geom_boxplot(outlier.shape = NA, width = .55, colour = violeta, fill = "#E9DDF7") +
    geom_point(data = extremos, colour = ocre, size = 2, alpha = .8) +
    labs(title = "Rendimiento anual por cultivo", subtitle = "Puntos ocres: fuera de las cercas de 3 IQR",
         x = "Rendimiento (t/ha)", y = NULL)
  if (all(puntos$rendimiento_t_ha > 0)) cajas <- cajas + scale_x_log10()
  guardar(cajas, "02_cajas")

  guardar(ggplot(puntos, aes(rendimiento_t_ha)) +
    geom_histogram(bins = 18, fill = violeta, colour = "white") +
    facet_wrap(~ cultivo, scales = "free", ncol = 5) +
    labs(title = "Formas del rendimiento por cultivo", subtitle = "Escalas independientes. Cada barra cuenta municipios-año.",
         x = "Rendimiento (t/ha)", y = "Observaciones"), "03_distribuciones", 11, 5.4)

  guardar(ggplot(trayectorias, aes(ano, rendimiento_ponderado)) +
    geom_line(colour = violeta, linewidth = .65) + geom_point(colour = violeta, size = 1.3) +
    facet_wrap(~ cultivo, scales = "free_y", ncol = 5) +
    scale_x_continuous(breaks = c(2007, 2012, 2018), labels = c("07", "12", "18")) +
    labs(title = "Evolución del rendimiento departamental",
         subtitle = "Producción total / área cosechada total. Escalas verticales por cultivo.",
         x = "Año (20xx)", y = "Rendimiento (t/ha)"), "04_trayectorias", 11, 5.4)

  guardar(ggplot(cobertura, aes(ano, cultivo, fill = rendimiento_valido)) +
    geom_tile(colour = "white") + geom_text(aes(label = rendimiento_valido), size = 3, colour = gris) +
    scale_fill_gradient(low = "#F7F2FB", high = "#9B7ACB") + scale_x_continuous(breaks = 2007:2018) +
    labs(title = "Cobertura municipal del rendimiento", x = "Año", y = NULL, fill = "Municipios"), "06_cobertura")
} else {
  # Salida equivalente con R base para equipos sin acceso a CRAN.
  abrir <- function(nombre, ancho = 1640, alto = 860) {
    png(file.path(ruta_figuras, paste0("eva_R_", nombre, ".png")), width = ancho, height = alto, res = 160)
    par(fg = gris, col.axis = gris, col.lab = gris, col.main = gris, las = 1)
  }

  abrir("02_cajas")
  par(mar = c(4.5, 8.8, 4.8, 2))
  usar_log <- all(puntos$rendimiento_t_ha > 0)
  valores <- if (usar_log) log10(puntos$rendimiento_t_ha) else puntos$rendimiento_t_ha
  boxplot(valores ~ puntos$cultivo, horizontal = TRUE, outline = FALSE, axes = FALSE,
          col = "#E9DDF7", border = violeta, boxwex = .58, xlab = "", ylab = "",
          ylim = range(c(valores, if (usar_log) log10(c(.18, 16)))))
  eje <- if (usar_log) c(.25, .5, 1, 2, 4, 8, 12, 16) else pretty(valores)
  axis(1, at = if (usar_log) log10(eje) else eje, labels = eje)
  axis(2, at = seq_along(levels(puntos$cultivo)), labels = levels(puntos$cultivo), tick = FALSE)
  abline(v = if (usar_log) log10(eje) else eje, col = "#E0D7EB", lty = 3)
  points(if (usar_log) log10(extremos$rendimiento_t_ha) else extremos$rendimiento_t_ha,
         as.integer(extremos$cultivo), pch = 19, col = ocre)
  title("Rendimiento anual por cultivo", xlab = if (usar_log) "Rendimiento (t/ha), escala logarítmica" else "Rendimiento (t/ha)")
  mtext("Los puntos ocres quedan fuera de las cercas de 3 IQR", side = 3, line = .1)
  invisible(dev.off())

  paneles <- function(nombre, datos, graficar, titulo, subtitulo, eje_x, eje_y) {
    abrir(nombre, 1930, 960)
    par(mfrow = c(2, 5), mar = c(3.3, 3.3, 2, .8), oma = c(2.3, 2.5, 3.6, .8))
    for (k in unique(datos$cultivo) |> sort()) graficar(datos[datos$cultivo == k, ], k)
    mtext(titulo, side = 3, outer = TRUE, line = 1.8, font = 2, cex = 1.15)
    mtext(subtitulo, side = 3, outer = TRUE, line = .4, cex = .92)
    mtext(eje_x, side = 1, outer = TRUE, line = .55)
    mtext(eje_y, side = 2, outer = TRUE, line = .45, las = 0)
    invisible(dev.off())
  }
  paneles("03_distribuciones", puntos, function(d, k) {
    hist(d$rendimiento_t_ha, breaks = 18, col = violeta, border = "white",
         xlab = "", ylab = "", main = k)
  }, "Formas del rendimiento por cultivo", "Escalas independientes por cultivo.",
  "Rendimiento (t/ha)", "Municipios-año")
  paneles("04_trayectorias", trayectorias, function(d, k) {
    d <- d[order(d$ano), ]
    plot(d$ano, d$rendimiento_ponderado, type = "o", pch = 19, cex = .65, col = violeta, lwd = 1.7,
         xaxt = "n", xlab = "", ylab = "", main = k)
    axis(1, at = c(2007, 2012, 2018), labels = c("07", "12", "18"))
  }, "Evolución del rendimiento departamental", "Producción total / área cosechada total. Escalas verticales por cultivo.",
  "Año (20xx)", "Rendimiento (t/ha)")
  abrir("06_cobertura")
  par(mar = c(4.2, 8.8, 4.2, 1))
  grilla <- xtabs(rendimiento_valido ~ ano + cultivo, cobertura)
  image(2007:2018, seq_len(ncol(grilla)), grilla, axes = FALSE,
        col = colorRampPalette(c("#F7F2FB", "#9B7ACB"))(32), xlab = "Año", ylab = "")
  axis(1, at = 2007:2018, tick = FALSE)
  axis(2, at = seq_len(ncol(grilla)), labels = colnames(grilla), tick = FALSE)
  text(rep(2007:2018, ncol(grilla)), rep(seq_len(ncol(grilla)), each = 12), as.vector(grilla), col = gris)
  title("Cobertura municipal del rendimiento")
  mtext("Número de municipios con rendimiento definido por cultivo y año", side = 3, line = .2)
  invisible(dev.off())
}
