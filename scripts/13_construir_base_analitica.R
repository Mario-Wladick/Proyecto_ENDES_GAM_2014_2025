
# SCRIPT 13: CONSTRUIR BASE ANALITICA
# ENDES 2014-2025
# Proyecto de Ciencia de Datos - GAM

library(dplyr)

raiz <- "."

# Variables que deseamos conservar
candidatas <- c(
  "QS23",       # Edad
  "QSSEXO",     # Sexo
  "QS24",       # Asistencia escolar
  "QS25N",      # Nivel educativo
  "QS26",       # Seguro de salud
  "QS27",       # Tipo de seguro
  "QS109",      # Diabetes
  "QS200",      # Tabaquismo
  "QS201",
  "QS202",
  "QS206",      # Alcohol
  "QS208",
  "QS210",
  "QS213C",     # Frutas
  "QS219C",     # Verduras
  "HV024",      # Region
  "HV025",      # Area urbana/rural
  "HV009",      # Miembros del hogar
  "HV040",      # Altitud
  "HV022",      # Estrato
  "HV021",      # Conglomerado
  "HHID",
  "QHCLUSTER",
  "QHNUMBER",
  "QHHOME",
  "QSNUMERO"
)

bases <- list()
resumen <- list()

for (anio in 2014:2025) {
  
  datos <- readRDS(file.path(
    raiz, "datos", "integrados",
    paste0("ENDES_integrada_", anio, ".rds")
  ))
  
  nombre_peso <- if (anio == 2014) {
    "PESO_MAY15AÑOS"
  } else if (anio == 2015) {
    "PESO15_AJUS"
  } else {
    "PESO15_AMAS"
  }
  
  # Homogeneizar los pesos decimales
  peso <- suppressWarnings(
    as.numeric(
      gsub(
        ",", ".",
        trimws(as.character(datos[[nombre_peso]])),
        fixed = TRUE
      )
    )
  )
  
  diagnostico <- trimws(as.character(datos$QS102))
  medicamentos <- trimws(as.character(datos$QS104))
  sexo <- trimws(as.character(datos$QSSEXO))
  
  edad <- suppressWarnings(
    as.numeric(as.character(datos$QS23))
  )
  
  filtro <- !is.na(diagnostico) &
    diagnostico == "1" &
    !is.na(medicamentos) &
    medicamentos %in% c("1", "2") &
    is.finite(edad) & edad >= 15 &
    !is.na(sexo) & sexo %in% c("1", "2") &
    is.finite(peso) & peso > 0
  
  # Solo conservamos personas de la cohorte
  base <- datos[filtro, , drop = FALSE]
  
  # Variables estandarizadas
  base$anio <- anio
  base$edad <- edad[filtro]
  base$sexo <- sexo[filtro]
  base$peso_muestral <- peso[filtro]
  
  # IMPORTANTE: QS104 debe codificarse como
  # respuesta negativa o positiva segun el diccionario.
  base$medicamentos_original <- medicamentos[filtro]
  
  # Crear respuesta solo si la codificacion es:
  # QS104 = 1: Si recibio/compro medicamentos
  # QS104 = 2: No recibio/compro medicamentos
  base$sin_medicacion <- ifelse(
    medicamentos[filtro] == "2", 1L, 0L
  )
  
  # Conservar columnas candidatas disponibles
  presentes <- intersect(candidatas, names(base))
  
  salida <- base[, presentes, drop = FALSE]
  
  salida$anio <- base$anio
  salida$edad <- base$edad
  salida$sexo <- base$sexo
  salida$peso_muestral <- base$peso_muestral
  salida$medicamentos_original <- base$medicamentos_original
  salida$sin_medicacion <- base$sin_medicacion
  
  # Unificar columnas: NA para variables no disponibles
  faltantes <- setdiff(candidatas, names(salida))
  
  for (variable in faltantes) {
    salida[[variable]] <- NA_character_
  }
  
  # Convertir las candidatas originales a texto
  # para combinar años sin conflictos de tipos.
  for (variable in candidatas) {
    salida[[variable]] <- as.character(salida[[variable]])
  }
  
  salida <- salida[, c(
    "anio", "edad", "sexo",
    "peso_muestral", "sin_medicacion",
    "medicamentos_original", candidatas
  )]
  
  bases[[as.character(anio)]] <- salida
  
  resumen[[as.character(anio)]] <- data.frame(
    anio = anio,
    registros = nrow(salida),
    sin_medicacion = sum(salida$sin_medicacion)
  )
  
  cat("ENDES", anio, ":", nrow(salida), "registros\n")
}

base_final <- dplyr::bind_rows(bases)
reporte <- dplyr::bind_rows(resumen)

# Verificaciones finales
stopifnot(nrow(base_final) == sum(reporte$registros))
stopifnot(!anyNA(base_final$peso_muestral))
stopifnot(all(base_final$peso_muestral > 0))

# Guardar resultados
saveRDS(
  base_final,
  file.path(
    raiz, "datos", "procesados",
    "ENDES_GAM_2014_2025_ampliada.rds"
  )
)

write.csv(
  reporte,
  file.path(
    raiz, "resultados", "inventario",
    "13_resumen_base_analitica.csv"
  ),
  row.names = FALSE
)

print(reporte)

cat("\nTOTAL DE REGISTROS:", nrow(base_final), "\n")
cat("TOTAL DE COLUMNAS:", ncol(base_final), "\n")
cat("\nBASE ANALITICA GUARDADA CORRECTAMENTE.\n")
