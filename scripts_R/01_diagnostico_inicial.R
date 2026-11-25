# EVA Cesar | 01. Importación, cobertura y primera inspección
# Ejecutar desde la raíz del proyecto. Las rutas son relativas.
ruta_crudos <- file.path("datos", "datos_crudos")
ruta_limpios <- file.path("datos", "datos_limpios")
ruta_docs <- "documentos"
ruta_figuras <- file.path(ruta_docs, "figuras")
dir.create(ruta_limpios, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_figuras, recursive = TRUE, showWarnings = FALSE)

archivo <- file.path(ruta_crudos, "Evaluaciones_Agropecuarias_Municipales_EVA_20261028.csv")
crudo <- read.csv(archivo, check.names = FALSE, colClasses = "character",
                  na.strings = c("", "NA", "N/A", "NULL"), encoding = "UTF-8")
normalizar <- function(x) {
  x <- tolower(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))
  gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", x))
}
names(crudo) <- normalizar(names(crudo))
crudo$ano <- as.integer(gsub(".", "", crudo$ano, fixed = TRUE))
es_cesar <- toupper(trimws(crudo$departamento)) == "CESAR" & crudo$ano %in% 2006:2018
eva <- crudo[which(es_cesar), ]
stopifnot(nrow(eva) > 0)
cat("Base nacional:", nrow(crudo), "filas. Cesar:", nrow(eva), "filas.\n")
rm(crudo)

medidas <- c("area_sembrada_ha", "area_cosechada_ha", "produccion_t", "rendimiento_t_ha")
numero <- function(x) {
  x <- gsub("[[:space:].]", "", x)
  as.numeric(sub(",", ".", x, fixed = TRUE))
}
eva[medidas] <- lapply(eva[medidas], numero)
for (nombre in c("cod_dep", "cod_mun")) {
  eva[[nombre]] <- gsub(".", "", eva[[nombre]], fixed = TRUE)
}
print(utils::head(eva, 5))
str(eva)
print(summary(eva[medidas]))

clave <- c("cod_mun", "cultivo", "ano", "periodo", "desagregacion_regional_y_o_sistema_productivo")
faltantes <- colSums(is.na(eva))
tipos <- data.frame(variable = names(eva), tipo = vapply(eva, typeof, ""),
                    distintos = vapply(eva, function(x) length(unique(stats::na.omit(x))), 0L))
duplicados <- data.frame(criterio = c("filas_identicas", "claves_repetidas"),
                         n = c(sum(duplicated(eva)), sum(duplicated(eva[clave]))))
cobertura <- aggregate(cbind(registros = rep(1L, nrow(eva)),
                            rendimiento_faltante = is.na(eva$rendimiento_t_ha) * 1L),
                       by = list(ano = eva$ano), FUN = sum)
cobertura$municipios <- as.integer(tapply(eva$cod_mun, eva$ano, function(x) length(unique(x))))
cobertura$cultivos <- as.integer(tapply(eva$cultivo, eva$ano, function(x) length(unique(x))))
periodos <- as.data.frame(table(eva$ano, sub("^\\d{4}$", "ANUAL", sub("^\\d{4}([AB])$", "\\1", eva$periodo))))
names(periodos) <- c("ano", "periodo", "registros")

guardar_csv <- function(tabla, nombre, carpeta = ruta_docs) {
  write.csv(tabla, file.path(carpeta, nombre), row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
guardar_csv(tipos, "diagnostico_R_tipos.csv")
guardar_csv(data.frame(variable = names(faltantes), n_na = as.integer(faltantes),
                       pct_na = 100 * faltantes / nrow(eva)), "diagnostico_R_faltantes.csv")
guardar_csv(duplicados, "diagnostico_R_duplicados.csv")
guardar_csv(cobertura, "diagnostico_R_cobertura.csv")
guardar_csv(periodos, "diagnostico_R_periodos.csv")
guardar_csv(eva, "eva_cesar_2006_2018_sin_limpiar_R.csv", ruta_limpios)

png(file.path(ruta_figuras, "eva_R_01_inspeccion.png"), width = 1220, height = 850, res = 140)
par(mfrow = c(2, 2), mar = c(4.3, 4, 2.2, 1.2))
for (i in seq_along(medidas)) {
  x <- eva[[medidas[i]]]
  hist(log10(1 + x[is.finite(x) & x >= 0]), breaks = 30, col = "#6D45A6", border = "white",
       main = c("Área sembrada (ha)", "Área cosechada (ha)", "Producción (t)", "Rendimiento (t/ha)")[i],
       xlab = "log10(1 + valor)", ylab = "Registros")
}
dev.off()
