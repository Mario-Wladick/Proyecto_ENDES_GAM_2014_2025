
# SCRIPT 14: AUDITAR VARIABLES CANDIDATAS
# ENDES 2014-2025 - MODELO GAM AMPLIADO

raiz <- "."

archivo <- file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_2014_2025_ampliada.rds"
)

datos <- readRDS(archivo)

# Variables candidatas a predictoras
candidatas <- c(
  "anio",
  "edad",
  "sexo",
  "QS24",
  "QS25N",
  "QS26",
  "QS27",
  "QS109",
  "QS200",
  "QS201",
  "QS202",
  "QS206",
  "QS208",
  "QS210",
  "QS213C",
  "QS219C",
  "HV024",
  "HV025",
  "HV009",
  "HV040"
)

resultados <- list()

for (variable in candidatas) {
  
  for (anio_actual in 2014:2025) {
    
    grupo <- datos[
      datos$anio == anio_actual,
      ,
      drop = FALSE
    ]
    
    valores <- trimws(
      as.character(grupo[[variable]])
    )
    
    ausentes <- is.na(valores) | valores == ""
    
    validos <- valores[!ausentes]
    
    # Valores frecuentes para revisar codigos
    frecuencias <- sort(
      table(validos),
      decreasing = TRUE
    )
    
    ejemplos <- paste(
      head(names(frecuencias), 5),
      collapse = ", "
    )
    
    resultados[[length(resultados) + 1]] <-
      data.frame(
        anio = anio_actual,
        variable = variable,
        total = length(valores),
        presentes = sum(!ausentes),
        ausentes = sum(ausentes),
        porcentaje_presentes = round(
          100 * mean(!ausentes), 2
        ),
        categorias_distintas = length(unique(validos)),
        codigos_frecuentes = ejemplos
      )
  }
}

reporte <- do.call(rbind, resultados)
rownames(reporte) <- NULL

# Resumen general por variable
resumen <- aggregate(
  cbind(presentes, ausentes, total) ~ variable,
  data = reporte,
  FUN = sum
)

resumen$porcentaje_presentes <- round(
  100 * resumen$presentes / resumen$total,
  2
)

resumen <- resumen[
  order(-resumen$porcentaje_presentes),
]

cat("\nCALIDAD GENERAL DE LAS VARIABLES\n")
print(resumen, row.names = FALSE)

cat("\nVARIABLES CON MENOS DEL 80% DE DATOS PRESENTES\n")

print(
  subset(
    resumen,
    porcentaje_presentes < 80
  ),
  row.names = FALSE
)

# Guardar reportes
destino <- file.path(
  raiz, "resultados", "inventario"
)

write.csv(
  reporte,
  file.path(
    destino,
    "14_calidad_variables_por_anio.csv"
  ),
  row.names = FALSE
)

write.csv(
  resumen,
  file.path(
    destino,
    "14_resumen_calidad_variables.csv"
  ),
  row.names = FALSE
)

cat("\nAUDITORIA DE VARIABLES FINALIZADA.\n")
