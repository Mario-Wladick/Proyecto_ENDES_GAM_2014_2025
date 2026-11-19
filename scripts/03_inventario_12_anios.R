# PROYECTO ENDES 2014-2025
# ETAPA 1: INVENTARIO DE VARIABLES DE SALUD

library(haven)
library(readr)

raiz <- "."

ruta_datos <- file.path(raiz, "datos", "extraidos")
ruta_salida <- file.path(raiz, "resultados", "inventario")

dir.create(ruta_salida, recursive = TRUE, showWarnings = FALSE)

inventarios <- list()
resumen <- list()

for (anio in 2014:2025) {
  
  cat("\nProcesando ENDES", anio, "...\n")
  
  carpeta <- file.path(ruta_datos, as.character(anio))
  
  archivos <- list.files(
    carpeta,
    pattern = "^CSALUD01(_[0-9]{4})?\\.(sav|csv)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(archivos) != 1) {
    stop(
      "Se esperaba un archivo CSALUD01 para ", anio,
      "; encontrados: ", length(archivos)
    )
  }
  
  archivo <- archivos[1]
  extension <- tolower(tools::file_ext(archivo))
  
  if (extension == "sav") {
    
    datos <- haven::read_sav(archivo)
    
  } else {
    
    # Leer todas las columnas como texto para preservar
    # identificadores y códigos originales.
    # Identificar el separador correspondiente al año
    separador <- if (anio %in% c(2020, 2022)) ";" else ","
    
    # Leer el archivo CSV
    datos <- readr::read_delim(
      archivo,
      delim = separador,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      progress = FALSE
    )
    
    # Comprobar si existen problemas de lectura
    problemas <- readr::problems(datos)
    
    if (nrow(problemas) > 0) {
      warning(
        "Se detectaron problemas de lectura en ENDES ",
        anio, ": ", nrow(problemas)
      )
    }
    
  }
  
  nombres <- names(datos)
  
  etiquetas <- vapply(
    datos,
    function(x) {
      etiqueta <- attr(x, "label")
      if (is.null(etiqueta)) {
        return(NA_character_)
      }
      as.character(etiqueta)
    },
    character(1)
  )
  
  inventarios[[as.character(anio)]] <- data.frame(
    anio = anio,
    variable = nombres,
    descripcion = etiquetas,
    tipo = vapply(datos, function(x) class(x)[1], character(1)),
    stringsAsFactors = FALSE
  )
  
  resumen[[as.character(anio)]] <- data.frame(
    anio = anio,
    registros = nrow(datos),
    variables = ncol(datos),
    formato = extension,
    stringsAsFactors = FALSE
  )
  
  cat(
    "Registros:", nrow(datos),
    "| Variables:", ncol(datos), "\n"
  )
  
  rm(datos)
  gc(verbose = FALSE)
}

diccionario <- do.call(rbind, inventarios)
resumen_final <- do.call(rbind, resumen)

# Disponibilidad de cada variable entre los 12 años
disponibilidad <- aggregate(
  anio ~ variable,
  data = diccionario,
  FUN = function(x) length(unique(x))
)

names(disponibilidad)[2] <- "numero_anios"

disponibilidad <- disponibilidad[
  order(-disponibilidad$numero_anios, disponibilidad$variable),
]

write.csv(
  diccionario,
  file.path(ruta_salida, "diccionario_2014_2025.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resumen_final,
  file.path(ruta_salida, "resumen_2014_2025.csv"),
  row.names = FALSE
)

write.csv(
  disponibilidad,
  file.path(ruta_salida, "disponibilidad_variables.csv"),
  row.names = FALSE
)

cat("\nINVENTARIO DE LOS 12 AÑOS FINALIZADO.\n")
print(resumen_final)