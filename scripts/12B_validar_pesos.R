
# SCRIPT 12B: VALIDAR FACTORES DE PONDERACION
# ENDES 2014-2025

raiz <- "."

resultados <- list()

for (anio in 2014:2025) {
  
  archivo <- file.path(
    raiz, "datos", "integrados",
    paste0("ENDES_integrada_", anio, ".rds")
  )
  
  datos <- readRDS(archivo)
  
  # Seleccionar el nombre del peso segun el año
  nombre_peso <- if (anio == 2014) {
    "PESO_MAY15AÑOS"
  } else if (anio == 2015) {
    "PESO15_AJUS"
  } else {
    "PESO15_AMAS"
  }
  
  if (!nombre_peso %in% names(datos)) {
    stop("No existe ", nombre_peso, " en ", anio)
  }
  
  diagnostico <- trimws(as.character(datos$QS102))
  medicamentos <- trimws(as.character(datos$QS104))
  sexo <- trimws(as.character(datos$QSSEXO))
  
  edad <- suppressWarnings(
    as.numeric(as.character(datos$QS23))
  )
  
  # Cohorte preliminar
  filtro <- !is.na(diagnostico) &
    diagnostico == "1" &
    !is.na(medicamentos) &
    medicamentos %in% c("1", "2") &
    !is.na(edad) &
    edad >= 15 &
    !is.na(sexo) &
    sexo %in% c("1", "2")
  
  # Convertir el peso a numerico
  # Convertir correctamente los pesos muestrales
  # Soporta coma o punto como separador decimal
  
  peso_texto <- trimws(
    as.character(datos[[nombre_peso]])
  )
  
  peso_texto <- gsub(
    ",", ".",
    peso_texto,
    fixed = TRUE
  )
  
  peso <- suppressWarnings(
    as.numeric(peso_texto)
  )
  
  # Peso valido: finito y positivo
  peso_valido <- is.finite(peso) & peso > 0
  
  resultados[[as.character(anio)]] <- data.frame(
    anio = anio,
    variable_peso = nombre_peso,
    cohorte_preliminar = sum(filtro),
    peso_valido = sum(filtro & peso_valido),
    peso_invalido = sum(filtro & !peso_valido),
    peso_minimo = if (any(filtro & peso_valido)) {
      min(peso[filtro & peso_valido])
    } else NA_real_,
    peso_maximo = if (any(filtro & peso_valido)) {
      max(peso[filtro & peso_valido])
    } else NA_real_
  )
  
  cat("Año", anio, ": validación de peso completada\n")
}

reporte <- do.call(rbind, resultados)
rownames(reporte) <- NULL

print(reporte)

cat("\nTOTAL CON PESOS VALIDOS:",
    sum(reporte$peso_valido), "\n")

cat("TOTAL CON PESOS INVALIDOS:",
    sum(reporte$peso_invalido), "\n")

write.csv(
  reporte,
  file.path(
    raiz,
    "resultados/inventario/",
    "12B_validacion_pesos.csv"
  ),
  row.names = FALSE
)

cat("\nVALIDACION DE PONDERACION FINALIZADA.\n")
