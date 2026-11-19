# 06_validar_claves_12_anios.R

library(haven)
library(readr)
library(dplyr)

raiz <- "."
ruta <- file.path(raiz, "datos", "extraidos")

leer_archivo <- function(archivo) {
  
  if (tolower(tools::file_ext(archivo)) == "sav") {
    datos <- haven::read_sav(archivo)
    datos$HHID <- as.character(datos$HHID)
    return(datos)
  }
  
  cabecera <- readLines(archivo, n = 1, warn = FALSE)
  
  separador <- if (
    grepl(";", cabecera, fixed = TRUE)
  ) ";" else ","
  
  readr::read_delim(
    archivo,
    delim = separador,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
}

resultados <- list()

for (anio in 2014:2025) {
  
  carpeta <- file.path(ruta, as.character(anio))
  
  archivo_salud <- list.files(
    carpeta,
    pattern = "^CSALUD01(_[0-9]{4})?\\.(sav|csv)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  archivo_hogar <- list.files(
    carpeta,
    pattern = "^RECH0(_[0-9]{4})?\\.(sav|csv)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(archivo_salud) != 1 ||
      length(archivo_hogar) != 1) {
    stop("Archivos faltantes o duplicados en ", anio)
  }
  
  salud <- leer_archivo(archivo_salud)
  hogar <- leer_archivo(archivo_hogar)
  
  names(salud) <- toupper(names(salud))
  names(hogar) <- toupper(names(hogar))
  
  if (!("HHID" %in% names(salud)) ||
      !("HHID" %in% names(hogar))) {
    stop("Falta HHID en ", anio)
  }
  
  salud$HHID <- trimws(as.character(salud$HHID))
  hogar$HHID <- trimws(as.character(hogar$HHID))
  
  claves_salud <- salud$HHID
  claves_hogar <- hogar$HHID
  
  validos_salud <- !is.na(claves_salud) & claves_salud != ""
  validos_hogar <- !is.na(claves_hogar) & claves_hogar != ""
  
  resultados[[as.character(anio)]] <- data.frame(
    anio = anio,
    registros_salud = nrow(salud),
    registros_hogar = nrow(hogar),
    hhid_vacios_salud = sum(!validos_salud),
    hhid_vacios_hogar = sum(!validos_hogar),
    hhid_duplicados_hogar = sum(
      duplicated(claves_hogar[validos_hogar])
    ),
    salud_sin_hogar = sum(
      validos_salud &
        !(claves_salud %in% claves_hogar[validos_hogar])
    )
  )
  
  cat("Año", anio, ": validación completada\n")
  
  rm(salud, hogar)
  gc(verbose = FALSE)
}

resumen <- dplyr::bind_rows(resultados)

print(resumen)

write.csv(
  resumen,
  file.path(
    raiz,
    "resultados/inventario/validacion_claves_12_anios.csv"
  ),
  row.names = FALSE
)

cat("\nVALIDACION DE CLAVES FINALIZADA.\n")