
# SCRIPT 30: DIAGNOSTICO DE CAMBIOS TEMPORALES
# ENDES 2014-2025

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

# Recuperar grupos de verduras
v <- datos$dias_verduras

datos$verduras_grupo <- ifelse(
  is.na(v), "No informado",
  ifelse(
    v <= 2, "1-2 dias",
    ifelse(v <= 4, "3-4 dias", "5-7 dias")
  )
)

# Pesos normalizados dentro de cada año
datos$peso_modelo <- ave(
  datos$peso_muestral,
  datos$anio,
  FUN = function(x) x / mean(x)
)

# Proporcion ponderada
porcentaje <- function(x, w) {
  100 * weighted.mean(x, w)
}

# Variables para revisar no informacion
variables <- c(
  "asistio_escuela",
  "tiene_seguro",
  "nivel_educativo",
  "frutas_grupo",
  "verduras_grupo"
)

# Tabla por año
resultados <- do.call(
  rbind,
  lapply(2014:2025, function(a) {
    
    d <- datos[datos$anio == a, , drop = FALSE]
    w <- d$peso_modelo
    
    fila <- data.frame(
      anio = a,
      registros = nrow(d),
      sin_compra_pct = porcentaje(
        d$sin_medicacion, w
      ),
      edad_media = weighted.mean(d$edad, w)
    )
    
    for (variable in variables) {
      columna <- paste0(variable, "_NI_pct")
      
      fila[[columna]] <- porcentaje(
        as.character(d[[variable]]) == "No informado",
        w
      )
    }
    
    fila
  })
)

rownames(resultados) <- NULL

cat("\nEVOLUCION ANUAL Y DATOS NO INFORMADOS\n")

print(
  round(resultados, 2),
  row.names = FALSE
)

# Variacion observada en los años recientes
cat("\nCAMBIO OBSERVADO ENTRE 2023 Y 2025\n")

p2023 <- resultados$sin_compra_pct[
  resultados$anio == 2023
]

p2025 <- resultados$sin_compra_pct[
  resultados$anio == 2025
]

cat(
  "Diferencia en puntos porcentuales:",
  round(p2025 - p2023, 3),
  "\n"
)

# Guardar resultados
destino <- file.path(
  raiz, "resultados", "modelos"
)

write.csv(
  resultados,
  file.path(
    destino,
    "30_diagnostico_temporal.csv"
  ),
  row.names = FALSE
)

cat("\nSCRIPT 30 FINALIZADO.\n")
