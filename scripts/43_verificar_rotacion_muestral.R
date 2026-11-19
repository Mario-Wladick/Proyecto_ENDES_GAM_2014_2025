
# =========================================================
# SCRIPT 43: VERIFICAR ROTACION MUESTRAL
# ENDES 2014-2025
# =========================================================

raiz <- "."

salida <- file.path(raiz, "resultados", "modelos")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

codigo <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

numero <- function(x) {
  suppressWarnings(
    as.numeric(gsub(",", ".", codigo(x), fixed = TRUE))
  )
}

nombre_peso <- function(anio) {
  if (anio == 2014) return("PESO_MAY15AÑOS")
  if (anio == 2015) return("PESO15_AJUS")
  "PESO15_AMAS"
}

# ---------------------------------------------------------
# 1. LEER CONGLOMERADOS DE CADA AÑO
# ---------------------------------------------------------

bases <- list()
resumen <- list()

for (a in 2014:2025) {
  
  cat("\nLEYENDO ENDES", a, "\n")
  
  archivo <- file.path(
    raiz, "datos", "integrados",
    paste0("ENDES_integrada_", a, ".rds")
  )
  
  d <- readRDS(archivo)
  
  columnas <- c(
    "HV024", "HV022",
    "QHCLUSTER", nombre_peso(a)
  )
  
  faltantes <- setdiff(columnas, names(d))
  
  if (length(faltantes) > 0) {
    stop(paste(
      "Faltan variables en", a,
      paste(faltantes, collapse = ", ")
    ))
  }
  
  # Conservar el identificador del departamento
  # junto al conglomerado para revisar colisiones.
  d$departamento_id <- codigo(d$HV024)
  d$psu_id <- codigo(d$QHCLUSTER)
  d$estrato_id <- codigo(d$HV022)
  
  peso <- numero(d[[nombre_peso(a)]])
  
  valido <- is.finite(peso) &
    peso > 0 &
    !is.na(d$departamento_id) &
    !is.na(d$psu_id) &
    !is.na(d$estrato_id)
  
  d <- d[valido, , drop = FALSE]
  
  # Clave candidata: departamento + conglomerado
  # NO confirma identidad fisica de la PSU.
  d$clave_psu <- paste(
    d$departamento_id,
    d$psu_id,
    sep = "_"
  )
  
  claves <- sort(unique(d$clave_psu))
  bases[[as.character(a)]] <- claves
  
  resumen[[as.character(a)]] <- data.frame(
    anio = a,
    registros_validos = nrow(d),
    psu_distintas = length(claves),
    codigos_psu = length(unique(d$psu_id)),
    estratos_originales = length(unique(d$estrato_id))
  )
}

resumen <- do.call(rbind, resumen)
rownames(resumen) <- NULL

cat("\n=====================================\n")
cat("1. RESUMEN DE IDENTIFICADORES\n")
cat("=====================================\n")

print(resumen, row.names = FALSE)

# ---------------------------------------------------------
# 2. COMPARAR PARES DE AÑOS
# ---------------------------------------------------------

comparar <- function(a, b) {
  
  x <- bases[[as.character(a)]]
  y <- bases[[as.character(b)]]
  
  comunes <- intersect(x, y)
  
  data.frame(
    anio_inicial = a,
    anio_final = b,
    psu_inicial = length(x),
    psu_final = length(y),
    claves_repetidas = length(comunes),
    porcentaje_inicial = 100 * length(comunes) / length(x),
    porcentaje_final = 100 * length(comunes) / length(y)
  )
}

# Comparaciones consecutivas
consecutivos <- do.call(
  rbind,
  lapply(2014:2024, function(a) {
    comparar(a, a + 1)
  })
)

cat("\n=====================================\n")
cat("2. SOLAPAMIENTO ENTRE AÑOS CONSECUTIVOS\n")
cat("=====================================\n")

print(
  consecutivos,
  row.names = FALSE,
  digits = 4
)

# Comparaciones utilizadas en el script 42
pares_prueba <- data.frame(
  inicial = c(2014, 2020, 2014, 2023),
  final = c(2020, 2025, 2025, 2025)
)

pruebas <- do.call(
  rbind,
  lapply(seq_len(nrow(pares_prueba)), function(i) {
    
    comparar(
      pares_prueba$inicial[i],
      pares_prueba$final[i]
    )
  })
)

cat("\n=====================================\n")
cat("3. SOLAPAMIENTO EN NUESTROS CONTRASTES\n")
cat("=====================================\n")

print(
  pruebas,
  row.names = FALSE,
  digits = 4
)

# ---------------------------------------------------------
# 3. GUARDAR RESULTADOS
# ---------------------------------------------------------

write.csv(
  resumen,
  file.path(
    salida,
    "43_resumen_conglomerados.csv"
  ),
  row.names = FALSE
)

write.csv(
  consecutivos,
  file.path(
    salida,
    "43_rotacion_consecutiva.csv"
  ),
  row.names = FALSE
)

write.csv(
  pruebas,
  file.path(
    salida,
    "43_rotacion_contrastes.csv"
  ),
  row.names = FALSE
)

cat("\nSCRIPT 43 FINALIZADO.\n")
cat(
  "NOTA: los codigos repetidos no prueban",
  "identidad de unidades muestrales.\n"
)
