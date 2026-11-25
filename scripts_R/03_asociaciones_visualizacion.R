# EVA Cesar | 03. Comparaciones descriptivas y variabilidad
ruta_limpios <- file.path("datos", "datos_limpios")
ruta_docs <- "documentos"
ruta_figuras <- file.path(ruta_docs, "figuras")
leer <- function(nombre) {
  read.csv(file.path(ruta_limpios, nombre), colClasses = c(cod_mun = "character"),
           na.strings = "", fileEncoding = "UTF-8")
}
exportar <- function(tabla, nombre) {
  write.csv(tabla, file.path(ruta_docs, paste0("e3_R_", nombre, ".csv")), row.names = FALSE, na = "")
}
anual <- leer("eva_cesar_anual_R.csv")
depurada <- leer("eva_cesar_anual_depurada_R.csv")
bases <- list(completa = anual, depurada = depurada)

# Comparación territorial como desviación de la mediana de su cultivo y año.
con_rend <- depurada[is.finite(depurada$rendimiento_t_ha), ]
centro <- ave(con_rend$rendimiento_t_ha, con_rend$cultivo, con_rend$ano, FUN = median)
con_rend$desviacion <- con_rend$rendimiento_t_ha - centro
territorio <- aggregate(cbind(desviacion, rendimiento_t_ha) ~ cod_mun + municipio + cultivo,
                        con_rend, median)
territorio$n_anos <- as.integer(tapply(con_rend$ano, list(con_rend$cod_mun, con_rend$municipio, con_rend$cultivo),
                                      length)[as.matrix(territorio[c("cod_mun", "municipio", "cultivo")])])
cobertura <- aggregate(cbind(registros = rep(1L, nrow(anual)),
                             rendimiento_valido = is.finite(anual$rendimiento_t_ha) * 1L) ~ cultivo + ano,
                       anual, sum)
trayectorias <- aggregate(cbind(area_cosechada_ha, produccion_t) ~ cultivo + ano, anual, sum)
trayectorias$rendimiento_ponderado <- with(trayectorias, produccion_t / area_cosechada_ha)
trayectorias <- merge(trayectorias, cobertura, by = c("cultivo", "ano"), sort = TRUE)


# Fracción descriptiva de la suma de cuadrados entre grupos.
# Cada fila anual válida tiene el mismo peso. La base completa permite revisar sensibilidad.
fraccion_entre <- function(y, centro, referencia) {
  total <- sum((y - referencia)^2)
  if (total == 0) return(NA_real_)
  100 * sum((centro - referencia)^2) / total
}
quintiles_area <- function(x) {
  cortes <- unique(quantile(x, (1:4) / 5, type = 7, names = FALSE))
  cut(x, c(-Inf, cortes, Inf), labels = FALSE, right = TRUE)
}
descomponer <- function(d, base) {
  d <- d[is.finite(d$rendimiento_t_ha) & d$rendimiento_t_ha > 0, ]
  grupos <- list(cultivo = d$cultivo, municipio = d$cod_mun, ano = d$ano,
                 area_sembrada = quintiles_area(d$area_sembrada_ha))
  internos <- grupos[-1L]
  internos$area_sembrada <- ave(d$area_sembrada_ha, d$cultivo, FUN = quintiles_area)
  medidas <- list(t_ha = d$rendimiento_t_ha, log_t_ha = log(d$rendimiento_t_ha))
  do.call(rbind, lapply(names(medidas), function(escala) {
    y <- medidas[[escala]]
    cultivo <- ave(y, d$cultivo, FUN = mean)
    total <- vapply(grupos, function(g) fraccion_entre(y, ave(y, g, FUN = mean), mean(y)), 0)
    dentro <- vapply(internos, function(g) fraccion_entre(y, ave(y, d$cultivo, g, FUN = mean), cultivo), 0)
    data.frame(base, escala, variable = names(total), n = nrow(d),
               pct_total = unname(total), pct_dentro_cultivo = c(NA_real_, unname(dentro)))
  }))
}
variabilidad <- do.call(rbind, Map(descomponer, bases, names(bases)))

exportar(variabilidad, "variabilidad")
exportar(territorio, "comparacion_territorial")
exportar(trayectorias, "cobertura_anual")

source(file.path("scripts_R", "figuras_eda.R"))
