# EVA Cesar | 02. Limpieza, agregación anual, descriptivos y extremos
# dplyr en RStudio. La ruta con aggregate reproduce las sumas sin paquetes externos.
ruta_limpios <- file.path("datos", "datos_limpios")
ruta_docs <- "documentos"
leer <- function(nombre) {
  read.csv(file.path(ruta_limpios, nombre), colClasses = c(cod_dep = "character", cod_mun = "character"),
           na.strings = "", fileEncoding = "UTF-8")
}
exportar <- function(tabla, nombre, carpeta = ruta_docs) {
  write.csv(tabla, file.path(carpeta, nombre), row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
eva <- leer("eva_cesar_2006_2018_sin_limpiar_R.csv")
bitacora <- data.frame(paso = "subconjunto_original", filas = nrow(eva))
registrar <- function(paso, datos) {
  bitacora <<- rbind(bitacora, data.frame(paso, filas = nrow(datos)))
}
texto <- setdiff(names(eva)[vapply(eva, is.character, TRUE)], c("cod_dep", "cod_mun"))
eva[texto] <- lapply(eva[texto], function(x) toupper(trimws(gsub("[[:space:]]+", " ", x))))
eva <- unique(eva)
registrar("sin_duplicados_exactos", eva)

clave <- c("cod_mun", "cultivo", "ano", "periodo", "desagregacion_regional_y_o_sistema_productivo")
stopifnot(!anyNA(eva[clave]), !anyDuplicated(eva[clave]))
stopifnot(!anyDuplicated(unique(eva[c("cod_mun", "municipio")])$cod_mun))
medidas <- c("area_sembrada_ha", "area_cosechada_ha", "produccion_t")
valido <- complete.cases(eva[medidas]) & Reduce(`&`, lapply(eva[medidas], function(x) is.finite(x) & x >= 0)) &
  eva$area_cosechada_ha <= eva$area_sembrada_ha &
  (eva$area_cosechada_ha > 0 | eva$produccion_t == 0)
exportar(eva[!valido, ], "e2_R_incoherentes.csv")
eva <- eva[valido, ]
registrar("medidas_coherentes", eva)

# Un faltante en el rendimiento reportado no impide usar las cantidades válidas.
eva$sin_cosecha <- eva$area_cosechada_ha == 0
eva$rend_discrepante <- with(eva, !sin_cosecha & !is.na(rendimiento_t_ha) &
    abs(produccion_t / area_cosechada_ha - rendimiento_t_ha) > 0.05 * rendimiento_t_ha + 0.01)
es_anual <- eva$ano %in% 2007:2018
elegibles <- eva[es_anual, ]
area_cultivos <- aggregate(area_sembrada_ha ~ cultivo, elegibles, sum)
area_cultivos <- area_cultivos[order(-area_cultivos$area_sembrada_ha, area_cultivos$cultivo, method = "radix"), ]
area_cultivos$fraccion_area <- area_cultivos$area_sembrada_ha / sum(area_cultivos$area_sembrada_ha)
principales <- head(area_cultivos$cultivo, 10)
area_cultivos$principal <- area_cultivos$cultivo %in% principales
eva$cultivo_principal <- eva$cultivo %in% principales
filas <- elegibles[elegibles$cultivo %in% principales, ]
registrar("2007_2018_cultivos_principales", filas)
grupos <- c("cod_mun", "municipio", "cultivo", "ano")
filas$n_registros <- 1L
filas$n_discrepancias <- as.integer(filas$rend_discrepante)
filas$n_sin_cosecha <- as.integer(filas$sin_cosecha)
sumables <- c("n_registros", medidas, "n_discrepancias", "n_sin_cosecha")
if (requireNamespace("dplyr", quietly = TRUE)) {
  anual <- filas |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(sumables), sum), .groups = "drop") |>
    as.data.frame()
} else {
  anual <- aggregate(filas[sumables], filas[grupos], sum)
}
anual <- anual[do.call(order, c(anual[grupos], method = "radix")), ]
stopifnot(!anyDuplicated(anual[c("cod_mun", "cultivo", "ano")]))
anual$rendimiento_t_ha <- with(anual, produccion_t / ifelse(area_cosechada_ha > 0, area_cosechada_ha, NA))
anual$log_rendimiento <- log(ifelse(anual$rendimiento_t_ha > 0, anual$rendimiento_t_ha, NA))

# Cercas exteriores de 3 IQR sobre el log-rendimiento por cultivo.
anual$atipico_severo <- FALSE
anual$cerca_inf_log <- anual$cerca_sup_log <- NA_real_
for (cultivo in principales) {
  indices <- which(anual$cultivo == cultivo & is.finite(anual$log_rendimiento))
  x <- anual$log_rendimiento[indices]
  if (length(x) < 20) next
  q <- unname(quantile(x, c(0.25, 0.75), type = 7))
  amplitud <- diff(q)
  if (amplitud == 0) next
  cercas <- q + c(-3, 3) * amplitud
  anual$cerca_inf_log[indices] <- cercas[1]
  anual$cerca_sup_log[indices] <- cercas[2]
  anual$atipico_severo[indices] <- x < cercas[1] | x > cercas[2]
}
depurada <- anual[!anual$atipico_severo, ]
candidatos <- anual[anual$atipico_severo, ]
candidatos$motivo <- rep("fuera de las cercas exteriores de 3 IQR", nrow(candidatos))
origen <- merge(candidatos[grupos], eva, by = grupos, sort = FALSE)

describir <- function(x) {
  x <- x[is.finite(x)]
  q <- if (length(x)) unname(quantile(x, c(0, .25, .5, .75, 1), type = 7)) else rep(NA_real_, 5)
  data.frame(n = length(x), minimo = q[1], q1 = q[2], mediana = q[3], q3 = q[4], maximo = q[5],
             media = mean(x), de = sd(x), iqr = q[4] - q[2])
}
variables <- c(medidas, "rendimiento_t_ha")
descriptivos <- do.call(rbind, lapply(variables, function(v) data.frame(variable = v, describir(anual[[v]]))))
por_cultivo <- do.call(rbind, lapply(principales, function(k) {
  do.call(rbind, lapply(c("completa", "depurada"), function(base) {
    d <- if (base == "completa") anual else depurada
    data.frame(cultivo = k, base, describir(d$rendimiento_t_ha[d$cultivo == k]))
  }))
}))
exportar(eva, "eva_cesar_limpio_R.csv", ruta_limpios)
exportar(anual, "eva_cesar_anual_R.csv", ruta_limpios)
exportar(depurada, "eva_cesar_anual_depurada_R.csv", ruta_limpios)
exportar(area_cultivos, "e2_R_cobertura_cultivos.csv")
exportar(bitacora, "e2_R_bitacora_limpieza.csv")
exportar(candidatos, "e2_R_atipicos_anuales.csv")
exportar(origen, "e2_R_atipicos_origen.csv")
exportar(descriptivos, "e2_R_descriptivos_generales.csv")
exportar(por_cultivo, "e2_R_descriptivos_por_cultivo.csv")
cat("Filas anuales:", nrow(anual), "| sin cosecha:", sum(is.na(anual$rendimiento_t_ha)),
    "| extremos:", nrow(candidatos), "\n")
