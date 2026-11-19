
# REVISAR AUSENCIAS DE QS24 Y QS26 POR SEXO
# ENDES 2014-2025

raiz <- "."

resultados <- list()

for (anio in 2014:2025) {
  
  datos <- readRDS(file.path(
    raiz, "datos", "integrados",
    paste0("ENDES_integrada_", anio, ".rds")
  ))
  
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
  
  sexo <- trimws(as.character(cohorte$QSSEXO))
  qs24 <- trimws(as.character(cohorte$QS24))
  qs26 <- trimws(as.character(cohorte$QS26))
  
  for (codigo_sexo in c("1", "2")) {
    
    grupo <- !is.na(sexo) & sexo == codigo_sexo
    
    resultados[[length(resultados) + 1]] <- data.frame(
      anio = anio,
      sexo = ifelse(codigo_sexo == "1", "Hombre", "Mujer"),
      total = sum(grupo),
      qs24_ausente = sum(grupo & (is.na(qs24) | qs24 == "")),
      qs26_ausente = sum(grupo & (is.na(qs26) | qs26 == "")),
      ambas_ausentes = sum(
        grupo &
          (is.na(qs24) | qs24 == "") &
          (is.na(qs26) | qs26 == "")
      )
    )
  }
}

reporte <- do.call(rbind, resultados)
rownames(reporte) <- NULL

print(reporte, row.names = FALSE)

write.csv(
  reporte,
  file.path(
    raiz,
    "resultados/inventario/",
    "11_ausencias_por_sexo_2014_2025.csv"
  ),
  row.names = FALSE
)

cat("\nAUDITORIA POR SEXO FINALIZADA.\n")
