
# SCRIPT 15: AUDITAR CODIGOS DE VARIABLES
# ENDES 2014-2025

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_2014_2025_ampliada.rds"
))

variables <- c(
  "QS24", "QS25N", "QS26", "QS27",
  "QS109", "QS200", "QS201", "QS202",
  "QS206", "QS208", "QS210",
  "QS213C", "QS219C",
  "HV024", "HV025", "HV009", "HV040"
)

resultados <- list()

for (variable in variables) {
  
  for (anio_actual in 2014:2025) {
    
    grupo <- datos[
      datos$anio == anio_actual,
      ,
      drop = FALSE
    ]
    
    valores <- trimws(
      as.character(grupo[[variable]])
    )
    
    valores[is.na(valores) | valores == ""] <- NA
    
    frecuencias <- table(
      valores,
      useNA = "ifany"
    )
    
    tabla <- data.frame(
      anio = anio_actual,
      variable = variable,
      codigo = names(frecuencias),
      frecuencia = as.integer(frecuencias)
    )
    
    tabla$porcentaje <- round(
      100 * tabla$frecuencia / nrow(grupo),
      2
    )
    
    resultados[[length(resultados) + 1]] <- tabla
  }
}

reporte <- do.call(rbind, resultados)
rownames(reporte) <- NULL

# Guardar todas las frecuencias
destino <- file.path(
  raiz, "resultados", "inventario",
  "15_frecuencias_codigos_2014_2025.csv"
)

write.csv(
  reporte,
  destino,
  row.names = FALSE
)

# Mostrar posibles codigos especiales
# Son candidatos a revisar, no NA confirmados.
especiales <- reporte[
  reporte$codigo %in% c(
    "7", "8", "9", "97", "98", "99",
    "997", "998", "999"
  ),
]

cat("\nPOSIBLES CODIGOS ESPECIALES\n")
print(especiales, row.names = FALSE)

cat("\nFRECUENCIAS DE VARIABLES DE SALUD\n")

salud <- subset(
  reporte,
  variable %in% c("QS109", "QS200", "QS206")
)

print(salud, row.names = FALSE)

cat("\nREPORTE COMPLETO GUARDADO EN:\n")
cat(destino, "\n")
cat("\nAUDITORIA DE CODIGOS FINALIZADA.\n")
