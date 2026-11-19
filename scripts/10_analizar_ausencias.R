
# ANALIZAR AUSENCIAS DE EDUCACION Y SEGURO
# ENDES 2014-2025

raiz <- "."

resultados <- list()

for (anio in 2014:2025) {
  
  archivo <- file.path(
    raiz, "datos", "integrados",
    paste0("ENDES_integrada_", anio, ".rds")
  )
  
  datos <- readRDS(archivo)
  
  diagnostico <- trimws(as.character(datos$QS102))
  medicamentos <- trimws(as.character(datos$QS104))
  
  cohorte <- datos[
    !is.na(diagnostico) &
      diagnostico == "1" &
      !is.na(medicamentos) &
      medicamentos %in% c("1", "2"),
    ,
    drop = FALSE
  ]
  
  educacion <- trimws(as.character(cohorte$QS25N))
  seguro <- trimws(as.character(cohorte$QS26))
  
  sin_educacion <- is.na(educacion) | educacion == ""
  sin_seguro <- is.na(seguro) | seguro == ""
  
  resultados[[as.character(anio)]] <- data.frame(
    anio = anio,
    total = nrow(cohorte),
    ambas_presentes = sum(!sin_educacion & !sin_seguro),
    ambas_ausentes = sum(sin_educacion & sin_seguro),
    solo_educacion_ausente = sum(sin_educacion & !sin_seguro),
    solo_seguro_ausente = sum(!sin_educacion & sin_seguro),
    porcentaje_ambas_ausentes = round(
      100 * mean(sin_educacion & sin_seguro), 2
    )
  )
}

reporte <- do.call(rbind, resultados)
rownames(reporte) <- NULL

print(reporte)

write.csv(
  reporte,
  file.path(
    raiz,
    "resultados/inventario/",
    "10_ausencias_educacion_seguro.csv"
  ),
  row.names = FALSE
)

cat("\nANALISIS DE AUSENCIAS FINALIZADO.\n")
