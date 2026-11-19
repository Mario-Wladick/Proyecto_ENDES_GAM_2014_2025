
# ============================================================
# SCRIPT 31: DIAGNOSTICO DE DATOS FALTANTES Y CALIBRACION
# POR SEXO
#
# PROYECTO: ENDES 2014-2025
# MODELOS ADITIVOS GENERALIZADOS (GAM)
# ============================================================

# ------------------------------------------------------------
# 1. CONFIGURAR RUTAS
# ------------------------------------------------------------

raiz <- "."

ruta_datos <- file.path(
  raiz,
  "datos",
  "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
)

ruta_modelos <- file.path(
  raiz,
  "resultados",
  "modelos"
)

# ------------------------------------------------------------
# 2. CARGAR BASE DE DATOS
# ------------------------------------------------------------

datos <- readRDS(ruta_datos)

cat("\n====================================\n")
cat("BASE ENDES 2014-2025\n")
cat("====================================\n")

cat("Total de registros:", nrow(datos), "\n")

stopifnot(nrow(datos) == 32806)

# Verificar variables necesarias
variables_necesarias <- c(
  "anio",
  "sexo_factor",
  "peso_muestral",
  "sin_medicacion",
  "asistio_escuela",
  "tiene_seguro",
  "nivel_educativo",
  "dias_verduras"
)

faltan <- setdiff(
  variables_necesarias,
  names(datos)
)

if (length(faltan) > 0) {
  stop(
    paste(
      "Faltan variables:",
      paste(faltan, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------
# 3. PREPARAR PESOS MUESTRALES
# ------------------------------------------------------------

stopifnot(
  all(is.finite(datos$peso_muestral)),
  all(datos$peso_muestral > 0)
)

datos$peso_modelo <- ave(
  datos$peso_muestral,
  datos$anio,
  FUN = function(x) {
    x / mean(x)
  }
)

# ------------------------------------------------------------
# 4. FUNCION DE PORCENTAJE PONDERADO
# ------------------------------------------------------------

porcentaje <- function(x, w) {
  
  validos <- !is.na(x) &
    is.finite(w) &
    w > 0
  
  if (!any(validos)) {
    return(NA_real_)
  }
  
  100 * weighted.mean(
    as.numeric(x[validos]),
    w[validos]
  )
}

# ------------------------------------------------------------
# 5. DATOS NO INFORMADOS POR AÑO Y SEXO
# ------------------------------------------------------------

cat("\n====================================\n")
cat("1. DATOS NO INFORMADOS POR AÑO Y SEXO\n")
cat("====================================\n")

variables_ni <- c(
  "asistio_escuela",
  "tiene_seguro",
  "nivel_educativo"
)

grupos <- split(
  seq_len(nrow(datos)),
  interaction(
    datos$anio,
    datos$sexo_factor,
    drop = TRUE
  )
)

tabla_faltantes <- do.call(
  rbind,
  lapply(grupos, function(id) {
    
    d <- datos[id, , drop = FALSE]
    
    w <- d$peso_modelo
    
    fila <- data.frame(
      anio = d$anio[1],
      sexo = as.character(d$sexo_factor[1]),
      registros = nrow(d),
      stringsAsFactors = FALSE
    )
    
    for (variable in variables_ni) {
      
      indicador <- is.na(d[[variable]]) |
        as.character(d[[variable]]) == "No informado"
      
      nombre_columna <- paste0(
        variable,
        "_NI_pct"
      )
      
      fila[[nombre_columna]] <- porcentaje(
        indicador,
        w
      )
    }
    
    fila
  })
)

# Ordenar por año y sexo
tabla_faltantes <- tabla_faltantes[
  order(
    tabla_faltantes$anio,
    tabla_faltantes$sexo
  ),
  ,
  drop = FALSE
]

rownames(tabla_faltantes) <- NULL

# CORRECCION:
# Redondear solamente columnas numericas
tabla_mostrar <- tabla_faltantes

columnas_decimales <- c(
  "asistio_escuela_NI_pct",
  "tiene_seguro_NI_pct",
  "nivel_educativo_NI_pct"
)

tabla_mostrar[columnas_decimales] <- lapply(
  tabla_mostrar[columnas_decimales],
  round,
  digits = 2
)

print(
  tabla_mostrar,
  row.names = FALSE
)

# ------------------------------------------------------------
# 6. PREPARAR VARIABLE DE VERDURAS
# ------------------------------------------------------------

v <- datos$dias_verduras

datos$verduras_grupo <- factor(
  ifelse(
    is.na(v),
    "No informado",
    ifelse(
      v <= 2,
      "1-2 dias",
      ifelse(
        v <= 4,
        "3-4 dias",
        "5-7 dias"
      )
    )
  ),
  levels = c(
    "1-2 dias",
    "3-4 dias",
    "5-7 dias",
    "No informado"
  )
)

# ------------------------------------------------------------
# 7. CARGAR LOS MODELOS GAM TEMPORALES
# ------------------------------------------------------------

modelo_basico <- readRDS(
  file.path(
    ruta_modelos,
    "28_gam_basico_temporal.rds"
  )
)

modelo_ampliado <- readRDS(
  file.path(
    ruta_modelos,
    "28_gam_ampliado_temporal.rds"
  )
)

cat("\nModelos GAM cargados correctamente.\n")

# ------------------------------------------------------------
# 8. SELECCIONAR DATOS DE PRUEBA: 2023-2025
# ------------------------------------------------------------

prueba <- datos[
  datos$anio %in% 2023:2025,
  ,
  drop = FALSE
]

cat("\n====================================\n")
cat("DATOS DE VALIDACION TEMPORAL\n")
cat("====================================\n")

cat(
  "Registros de prueba:",
  nrow(prueba),
  "\n"
)

stopifnot(nrow(prueba) == 8110)

# ------------------------------------------------------------
# 9. GENERAR PREDICCIONES
# ------------------------------------------------------------

cat("\nGenerando predicciones GAM basico...\n")

prueba$p_basico <- predict(
  modelo_basico,
  newdata = prueba,
  type = "response"
)

cat("Generando predicciones GAM ampliado...\n")

prueba$p_ampliado <- predict(
  modelo_ampliado,
  newdata = prueba,
  type = "response"
)

# Comprobar que las predicciones sean validas
stopifnot(
  all(is.finite(prueba$p_basico)),
  all(is.finite(prueba$p_ampliado))
)

cat("Predicciones generadas correctamente.\n")

# ------------------------------------------------------------
# 10. CALIBRACION POR AÑO Y SEXO
# ------------------------------------------------------------

grupos_prueba <- split(
  seq_len(nrow(prueba)),
  interaction(
    prueba$anio,
    prueba$sexo_factor,
    drop = TRUE
  )
)

calibracion <- do.call(
  rbind,
  lapply(grupos_prueba, function(id) {
    
    d <- prueba[id, , drop = FALSE]
    
    w <- d$peso_modelo
    
    observado <- porcentaje(
      d$sin_medicacion,
      w
    )
    
    estimado_basico <- porcentaje(
      d$p_basico,
      w
    )
    
    estimado_ampliado <- porcentaje(
      d$p_ampliado,
      w
    )
    
    data.frame(
      anio = d$anio[1],
      sexo = as.character(d$sexo_factor[1]),
      registros = nrow(d),
      observado_pct = observado,
      basico_pct = estimado_basico,
      ampliado_pct = estimado_ampliado,
      error_basico_pp = estimado_basico - observado,
      error_ampliado_pp = estimado_ampliado - observado,
      stringsAsFactors = FALSE
    )
  })
)

# Ordenar tabla
calibracion <- calibracion[
  order(
    calibracion$anio,
    calibracion$sexo
  ),
  ,
  drop = FALSE
]

rownames(calibracion) <- NULL

# ------------------------------------------------------------
# 11. MOSTRAR RESULTADOS DE CALIBRACION
# ------------------------------------------------------------

cat("\n====================================\n")
cat("2. CALIBRACION POR AÑO Y SEXO\n")
cat("====================================\n")

calibracion_mostrar <- calibracion

columnas_calibracion <- c(
  "observado_pct",
  "basico_pct",
  "ampliado_pct",
  "error_basico_pp",
  "error_ampliado_pp"
)

calibracion_mostrar[columnas_calibracion] <- lapply(
  calibracion_mostrar[columnas_calibracion],
  round,
  digits = 3
)

print(
  calibracion_mostrar,
  row.names = FALSE
)

# ------------------------------------------------------------
# 12. GUARDAR RESULTADOS
# ------------------------------------------------------------

dir.create(
  ruta_modelos,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  tabla_faltantes,
  file.path(
    ruta_modelos,
    "31_faltantes_por_sexo.csv"
  ),
  row.names = FALSE
)

write.csv(
  calibracion,
  file.path(
    ruta_modelos,
    "31_calibracion_por_sexo.csv"
  ),
  row.names = FALSE
)

cat("\n====================================\n")
cat("ARCHIVOS GUARDADOS\n")
cat("====================================\n")

cat("31_faltantes_por_sexo.csv\n")
cat("31_calibracion_por_sexo.csv\n")

cat("\nSCRIPT 31 FINALIZADO CORRECTAMENTE.\n")
