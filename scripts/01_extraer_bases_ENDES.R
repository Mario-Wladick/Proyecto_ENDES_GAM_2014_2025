# PROYECTO ENDES 2014-2025
# ETAPA 1: EXTRAER ARCHIVOS PARA R

raiz_R <- "."

ruta_originales <- "datos/originales"

destino <- file.path(raiz_R, "datos", "extraidos")

dir.create(destino, recursive = TRUE, showWarnings = FALSE)

for (anio in 2014:2025) {
  
  carpeta <- file.path(ruta_originales, paste0("ENDES_", anio))
  
  archivos_zip <- list.files(
    carpeta,
    pattern = "\\.zip$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  destino_anio <- file.path(destino, as.character(anio))
  
  dir.create(destino_anio, recursive = TRUE, showWarnings = FALSE)
  
  for (archivo in archivos_zip) {
    
    # Consultar el contenido del ZIP
    contenido <- unzip(archivo, list = TRUE)
    
    # Identificar únicamente archivos de datos
    nombres <- contenido$Name
    
    seleccion <- grepl(
      "(CSALUD01|RECH0).*\\.(sav|csv)$",
      nombres,
      ignore.case = TRUE,
      useBytes = TRUE
    )
    
    seleccion[is.na(seleccion)] <- FALSE
    
    if (any(seleccion)) {
      
      unzip(
        archivo,
        files = nombres[seleccion],
        exdir = destino_anio
      )
      
      cat(
        "Año:", anio,
        "| ZIP:", basename(archivo),
        "| Archivos extraídos:", sum(seleccion), "\n"
      )
    }
  }
}

cat("\nExtracción finalizada.\n")