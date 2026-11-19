
# PROYECTO ENDES 2014-2025
# SCRIPT 08: VERIFICAR VARIABLES PRINCIPALES

library(dplyr)

raiz <- "."

carpeta <- file.path(raiz, "datos", "integrados")

variables <- c(
  "QS102",   # Diagnostico de hipertension
  "QS104",   # Medicamentos
  "QS23",    # Edad
  "QSSEXO",  # Sexo
  "QS25N",   # Educacion
  "QS26",    # Seguro de salud
  "QS109",   # Diabetes
  "QS200",   # Tabaquismo
  "QS206",   # Alcohol
  "HV024",   # Region
  "HV025",   # Area de residencia
  "HV009",   # Integrantes del hogar
  "HV040",   # Altitud
  "HV022"    # Estrato muestral
)

resultados <- list()

for (anio in 2014:2025) {
  
  archivo <- file.path(
    carpeta,
    paste0("ENDES_integrada_", anio, ".rds")
  )
  
  datos <- readRDS(archivo)
  
  presentes <- variables %in% names(datos)
  
  resultados[[as.character(anio)]] <- data.frame(
    anio = anio,
    variable = variables,
    disponible = presentes
  )
  
  cat(
    "ENDES", anio,
    "| Variables encontradas:",
    sum(presentes), "de", length(variables), "\n"
  )
}

reporte <- dplyr::bind_rows(resultados)

write.csv(
  reporte,
  file.path(
    raiz,
    "resultados/inventario/",
    "verificacion_variables_integradas.csv"
  ),
  row.names = FALSE
)

cat("\nVERIFICACION FINALIZADA.\n")

print(
  reporte %>%
    group_by(variable) %>%
    summarise(
      anios_disponibles = sum(disponible),
      .groups = "drop"
    )
)
