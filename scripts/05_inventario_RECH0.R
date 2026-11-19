# ENDES 2014-2025 - Etapa 1: inventario del modulo RECH0
# Este script SOLO LEE los archivos extraidos y guarda inventarios nuevos.
# No modifica los datos originales ni las bases procesadas.

if (!requireNamespace('haven', quietly = TRUE) ||
    !requireNamespace('readr', quietly = TRUE)) {
  stop('Instala los paquetes antes de continuar: install.packages(c("haven", "readr"))')
}

raiz <- '.'
carpeta_datos <- file.path(raiz, 'datos', 'extraidos')
carpeta_salida <- file.path(raiz, 'resultados', 'inventario')
dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)

inventarios <- list()
resumenes <- list()

for (anio in 2014:2025) {
  ruta_anio <- file.path(carpeta_datos, as.character(anio))
  encontrados <- list.files(
    ruta_anio,
    pattern = '^RECH0(_[0-9]{4})?\\.(sav|csv)$',
    recursive = TRUE, full.names = TRUE, ignore.case = TRUE
  )
  
  if (length(encontrados) == 0) {
    cat('ENDES', anio, ': RECH0 no encontrado.\n')
    resumenes[[as.character(anio)]] <- data.frame(
      anio = anio, estado = 'NO ENCONTRADO', registros = NA_integer_,
      variables = NA_integer_, archivo = NA_character_
    )
    next
  }
  if (length(encontrados) > 1) {
    stop('Hay mas de un archivo RECH0 en el anio ', anio,
         '. Revisar antes de continuar: ', paste(encontrados, collapse = ' | '))
  }
  
  archivo <- encontrados[[1]]
  extension <- tolower(tools::file_ext(archivo))
  if (extension == 'sav') {
    datos <- haven::read_sav(archivo)
  } else {
    encabezado <- readLines(archivo, n = 1, warn = FALSE)
    n_comas <- lengths(regmatches(encabezado, gregexpr(',', encabezado, fixed = TRUE)))
    n_puntoycoma <- lengths(regmatches(encabezado, gregexpr(';', encabezado, fixed = TRUE)))
    separador <- if (n_puntoycoma > n_comas) ';' else ','
    datos <- readr::read_delim(
      archivo, delim = separador,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE, progress = FALSE, name_repair = 'minimal'
    )
    incidencias <- readr::problems(datos)
    if (nrow(incidencias) > 0) {
      warning('ENDES ', anio, ': ', nrow(incidencias), ' problemas de lectura')
    }
  }
  
  etiquetas <- vapply(datos, function(x) {
    etiqueta <- attr(x, 'label')
    if (is.null(etiqueta)) NA_character_ else as.character(etiqueta)
  }, character(1))
  inventarios[[as.character(anio)]] <- data.frame(
    anio = anio,
    variable = names(datos),
    descripcion = unname(etiquetas),
    tipo = vapply(datos, function(x) class(x)[1], character(1)),
    stringsAsFactors = FALSE
  )
  resumenes[[as.character(anio)]] <- data.frame(
    anio = anio, estado = 'LEIDO', registros = nrow(datos),
    variables = ncol(datos), archivo = basename(archivo)
  )
  cat('ENDES', anio, ':', nrow(datos), 'registros y',
      ncol(datos), 'variables de RECH0.\n')
  rm(datos)
  gc(verbose = FALSE)
}

resumen <- do.call(rbind, resumenes)
diccionario <- if (length(inventarios)) do.call(rbind, inventarios) else data.frame()

write.csv(resumen, file.path(carpeta_salida, 'resumen_RECH0.csv'),
          row.names = FALSE, fileEncoding = 'UTF-8')
if (nrow(diccionario) > 0) {
  write.csv(diccionario, file.path(carpeta_salida, 'diccionario_RECH0.csv'),
            row.names = FALSE, fileEncoding = 'UTF-8')
  disponibilidad <- aggregate(anio ~ variable, diccionario,
                              FUN = function(x) length(unique(x)))
  names(disponibilidad)[2] <- 'numero_anios'
  disponibilidad <- disponibilidad[
    order(-disponibilidad$numero_anios, disponibilidad$variable), ]
  write.csv(disponibilidad,
            file.path(carpeta_salida, 'disponibilidad_RECH0.csv'),
            row.names = FALSE, fileEncoding = 'UTF-8')
}
cat('\nINVENTARIO RECH0 FINALIZADO.\n')
print(resumen)
cat('\nNOTA: si 2025 aparece sin RECH0, sera necesario localizar su fuente original\nantes de integrar variables de hogar para todos los anios.\n')
