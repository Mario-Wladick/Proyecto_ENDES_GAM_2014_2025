
# SCRIPT 18: VALIDAR CONSUMO DE FRUTAS Y VERDURAS
# ENDES 2014-2025

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_2014_2025_recodificada.rds"
))

variables <- c("QS213C", "QS219C")

resultados <- list()

for (variable in variables) {
  
  texto <- trimws(as.character(datos[[variable]]))
  texto[texto == ""] <- NA_character_
  
  # Conversion decimal sin alterar el archivo original
  numero <- suppressWarnings(
    as.numeric(gsub(",", ".", texto, fixed = TRUE))
  )
  
  cat("\n====================================\n")
  cat("VARIABLE:", variable, "\n")
  
  cat("\nCODIGOS MAS FRECUENTES:\n")
  print(head(sort(table(texto), decreasing = TRUE), 15))
  
  cat("\nVALORES FUERA DE 0 A 7:\n")
  
  fuera <- !is.na(numero) &
    (!is.finite(numero) |
       numero < 0 | numero > 7 |
       numero != floor(numero))
  
  print(head(sort(
    table(texto[fuera]),
    decreasing = TRUE
  ), 15))
  
  for (anio_actual in 2014:2025) {
    
    grupo <- datos$anio == anio_actual
    
    validos <- grupo &
      !is.na(numero) &
      is.finite(numero) &
      numero >= 0 &
      numero <= 7 &
      numero == floor(numero)
    
    resultados[[length(resultados) + 1]] <- data.frame(
      anio = anio_actual,
      variable = variable,
      total = sum(grupo),
      validos_0_7 = sum(validos),
      ausentes = sum(grupo & is.na(texto)),
      fuera_rango = sum(grupo & fuera),
      porcentaje_validos = round(
        100 * sum(validos) / sum(grupo),
        2
      )
    )
  }
}

reporte <- do.call(rbind, resultados)
rownames(reporte) <- NULL

cat("\nRESUMEN POR AÑO\n")
print(reporte, row.names = FALSE)

write.csv(
  reporte,
  file.path(
    raiz, "resultados", "inventario",
    "18_validacion_frutas_verduras.csv"
  ),
  row.names = FALSE
)

cat("\nVALIDACION FINALIZADA.\n")
