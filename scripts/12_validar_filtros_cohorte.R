
# PROYECTO ENDES 2014-2025
# SCRIPT 12: VALIDAR FILTROS DE LA COHORTE
# No modifica las bases originales

raiz <- "."

origen <- file.path(raiz, "datos", "integrados")
destino <- file.path(raiz, "resultados", "inventario")

# Identificar posibles variables de ponderacion
pesos_candidatos <- c(
  "QHWEIGHT", "QFACTOR", "QSWEIGHT"
)

resultados <- list()
pesos_encontrados <- list()

for (anio in 2014:2025) {
  
  datos <- readRDS(file.path(
    origen,
    paste0("ENDES_integrada_", anio, ".rds")
  ))
  
  # Convertir a texto antes de evaluar codigos
  diagnostico <- trimws(as.character(datos$QS102))
  medicamentos <- trimws(as.character(datos$QS104))
  sexo <- trimws(as.character(datos$QSSEXO))
  
  edad <- suppressWarnings(
    as.numeric(as.character(datos$QS23))
  )
  
  # Filtro 1: diagnostico previo de hipertension
  f1 <- !is.na(diagnostico) & diagnostico == "1"
  
  # Filtro 2: respuesta valida sobre medicamentos
  f2 <- f1 &
    !is.na(medicamentos) &
    medicamentos %in% c("1", "2")
  
  # Filtro 3: edad valida (15 años o mas)
  f3 <- f2 & !is.na(edad) & edad >= 15
  
  # Filtro 4: sexo valido
  f4 <- f3 &
    !is.na(sexo) &
    sexo %in% c("1", "2")
  
  # Localizar columnas candidatas para peso
  encontrados <- intersect(
    pesos_candidatos,
    names(datos)
  )
  
  pesos_encontrados[[as.character(anio)]] <- data.frame(
    anio = anio,
    variable_peso = if (length(encontrados) == 0)
      "NO ENCONTRADA"
    else paste(encontrados, collapse = ", ")
  )
  
  # Revisar pesos; no elegir automaticamente uno
  peso_valido <- NA_integer_
  
  if (length(encontrados) > 0) {
    for (nombre in encontrados) {
      
      peso <- suppressWarnings(
        as.numeric(as.character(datos[[nombre]]))
      )
      
      cat(
        "Año:", anio,
        "| Peso candidato:", nombre,
        "| Positivos en cohorte:",
        sum(f4 & is.finite(peso) & peso > 0),
        "\n"
      )
    }
  }
  
  resultados[[as.character(anio)]] <- data.frame(
    anio = anio,
    originales = nrow(datos),
    con_diagnostico = sum(f1),
    con_respuesta_medicamentos = sum(f2),
    con_edad_valida = sum(f3),
    con_sexo_valido = sum(f4),
    excluidos_edad = sum(f2) - sum(f3),
    excluidos_sexo = sum(f3) - sum(f4)
  )
  
  rm(datos)
  gc(verbose = FALSE)
}

reporte <- do.call(rbind, resultados)
pesos_reporte <- do.call(rbind, pesos_encontrados)

rownames(reporte) <- NULL
rownames(pesos_reporte) <- NULL

print(reporte)
print(pesos_reporte)

cat("\nTOTALES DE LOS FILTROS:\n")
print(colSums(reporte[, -1]))

write.csv(
  reporte,
  file.path(destino, "12_validacion_filtros.csv"),
  row.names = FALSE
)

write.csv(
  pesos_reporte,
  file.path(destino, "12_variables_peso.csv"),
  row.names = FALSE
)

cat("\nVALIDACION DE FILTROS FINALIZADA.\n")
