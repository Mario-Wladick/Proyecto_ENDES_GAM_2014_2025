
# PROYECTO ENDES 2014-2025
# SCRIPT 09: AUDITORIA DE VALORES Y CODIGOS

raiz <- "."

origen <- file.path(raiz, "datos", "integrados")
destino <- file.path(raiz, "resultados", "inventario")

variables <- c(
  "QS102", "QS104", "QS23", "QSSEXO",
  "QS25N", "QS26", "QS109", "QS200",
  "QS206", "HV024", "HV025", "HV009",
  "HV040", "HV022"
)

resumen <- list()
frecuencias <- list()

for (anio in 2014:2025) {
  
  cat("\nAuditando ENDES", anio, "...\n")
  
  archivo <- file.path(
    origen, paste0("ENDES_integrada_", anio, ".rds")
  )
  
  datos <- readRDS(archivo)
  
  # Cohorte preliminar: diagnostico previo de hipertension
  # y respuesta valida sobre medicamentos
  diagnostico <- trimws(as.character(datos$QS102))
  medicamentos <- trimws(as.character(datos$QS104))
  
  cohorte <- datos[
    !is.na(diagnostico) &
      !is.na(medicamentos) &
      diagnostico == "1" &
      medicamentos %in% c("1", "2"),
    ,
    drop = FALSE
  ]
  
  cat("Cohorte preliminar:", nrow(cohorte), "\n")
  
  for (variable in variables) {
    
    valores <- trimws(as.character(cohorte[[variable]]))
    
    faltantes <- is.na(valores) | valores == ""
    
    resumen[[length(resumen) + 1]] <- data.frame(
      anio = anio,
      variable = variable,
      registros = length(valores),
      presentes = sum(!faltantes),
      ausentes = sum(faltantes),
      porcentaje_presente = round(
        100 * mean(!faltantes), 2
      ),
      valores_distintos = length(
        unique(valores[!faltantes])
      )
    )
    
    # Tabla de frecuencias por codigo
    valores[faltantes] <- "<SIN_DATO>"
    
    tabla <- sort(table(valores), decreasing = TRUE)
    
    frecuencias[[length(frecuencias) + 1]] <- data.frame(
      anio = anio,
      variable = variable,
      codigo = names(tabla),
      frecuencia = as.integer(tabla)
    )
  }
  
  rm(datos, cohorte)
  gc(verbose = FALSE)
}

resumen_final <- do.call(rbind, resumen)
frecuencias_final <- do.call(rbind, frecuencias)

write.csv(
  resumen_final,
  file.path(destino, "09_resumen_calidad_variables.csv"),
  row.names = FALSE
)

write.csv(
  frecuencias_final,
  file.path(destino, "09_frecuencias_codigos.csv"),
  row.names = FALSE
)

cat("\nAUDITORIA DE CODIGOS FINALIZADA.\n")
