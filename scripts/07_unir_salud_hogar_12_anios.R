
# PROYECTO ENDES 2014-2025
# SCRIPT 07: UNIR SALUD Y HOGAR EN LOS 12 AÑOS

library(haven)
library(readr)
library(dplyr)

raiz <- "."

origen <- file.path(raiz, "datos", "extraidos")
destino <- file.path(raiz, "datos", "integrados")
carpeta_reportes <- file.path(raiz, "resultados", "inventario")

dir.create(destino, recursive = TRUE, showWarnings = FALSE)

# Leer archivos SAV o CSV
leer_base <- function(archivo) {
  
  extension <- tolower(tools::file_ext(archivo))
  
  if (extension == "sav") {
    datos <- haven::read_sav(archivo)
  } else {
    cabecera <- readLines(archivo, n = 1, warn = FALSE)
    
    separador <- if (grepl(";", cabecera, fixed = TRUE)) {
      ";"
    } else {
      ","
    }
    
    datos <- readr::read_delim(
      archivo,
      delim = separador,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE,
      progress = FALSE
    )
    
    if (nrow(readr::problems(datos)) > 0) {
      stop("Problemas de lectura en: ", archivo)
    }
  }
  
  names(datos) <- toupper(names(datos))
  datos$HHID <- trimws(as.character(datos$HHID))
  
  return(datos)
}

# Variables de hogar que queremos incorporar
variables_hogar <- c(
  "HHID", "HV024", "HV025",
  "HV009", "HV040", "HV022"
)

reportes <- list()

for (anio in 2014:2025) {
  
  cat("\nIntegrando ENDES", anio, "...\n")
  
  carpeta <- file.path(origen, as.character(anio))
  
  archivos_salud <- list.files(
    carpeta,
    pattern = "^CSALUD01(_[0-9]{4})?\\.(sav|csv)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  archivos_hogar <- list.files(
    carpeta,
    pattern = "^RECH0(_[0-9]{4})?\\.(sav|csv)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(archivos_salud) != 1 ||
      length(archivos_hogar) != 1) {
    stop("Archivos faltantes o duplicados: ", anio)
  }
  
  salud <- leer_base(archivos_salud)
  hogar <- leer_base(archivos_hogar)
  
  faltantes <- setdiff(variables_hogar, names(hogar))
  
  if (length(faltantes) > 0) {
    stop(
      "Faltan variables en RECH0 ", anio, ": ",
      paste(faltantes, collapse = ", ")
    )
  }
  
  if (anyNA(salud$HHID) || any(salud$HHID == "") ||
      anyNA(hogar$HHID) || any(hogar$HHID == "")) {
    stop("HHID vacío: ", anio)
  }
  
  if (anyDuplicated(hogar$HHID) > 0) {
    stop("HHID duplicado en RECH0: ", anio)
  }
  
  if (!all(salud$HHID %in% hogar$HHID)) {
    stop("Registros sin hogar correspondiente: ", anio)
  }
  
  # Evitar duplicar columnas que ya existen en salud
  nuevas <- setdiff(variables_hogar, names(salud))
  nuevas <- setdiff(nuevas, "HHID")
  
  hogar_sel <- hogar %>%
    select(all_of(c("HHID", nuevas)))
  
  # Integración de muchos registros de salud a un hogar
  integrada <- salud %>%
    left_join(
      hogar_sel,
      by = "HHID",
      relationship = "many-to-one"
    )
  
  if (nrow(integrada) != nrow(salud)) {
    stop("Cambió el número de registros: ", anio)
  }
  
  # Guardar la base integrada sin alterar los originales
  archivo_salida <- file.path(
    destino,
    paste0("ENDES_integrada_", anio, ".rds")
  )
  
  saveRDS(integrada, archivo_salida, compress = "gzip")
  
  reportes[[as.character(anio)]] <- data.frame(
    anio = anio,
    registros_antes = nrow(salud),
    registros_despues = nrow(integrada),
    variables_finales = ncol(integrada),
    columnas_agregadas = length(nuevas)
  )
  
  cat(
    "Registros:", nrow(integrada),
    "| Variables:", ncol(integrada),
    "| Nuevas:", length(nuevas), "\n"
  )
  
  rm(salud, hogar, integrada, hogar_sel)
  gc(verbose = FALSE)
}

reporte <- bind_rows(reportes)

write.csv(
  reporte,
  file.path(carpeta_reportes, "reporte_integracion_12_anios.csv"),
  row.names = FALSE
)

print(reporte)

cat("\nINTEGRACION DE LOS 12 AÑOS FINALIZADA.\n")
