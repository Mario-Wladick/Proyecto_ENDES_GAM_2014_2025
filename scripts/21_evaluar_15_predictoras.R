
# SCRIPT 21: EVALUAR MODELO DE 15 PREDICTORAS
# ENDES 2014-2025

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_2014_2025_16_candidatas.rds"
))

# Modelo provisional de 15 predictoras
predictoras_15 <- c(
  "anio",
  "edad",
  "sexo_factor",
  "diabetes",
  "fumo_12meses",
  "alcohol_alguna_vez",
  "alcohol_12meses",
  "asistio_escuela",
  "tiene_seguro",
  "nivel_educativo",
  "region",
  "area_residencia",
  "miembros_hogar",
  "altitud",
  "dias_frutas"
)

stopifnot(
  length(predictoras_15) == 15,
  all(predictoras_15 %in% names(datos)),
  nrow(datos) == 32806
)

# Comparar registros completos
datos$completo_15 <- complete.cases(
  datos[, predictoras_15, drop = FALSE]
)

datos$completo_16 <- complete.cases(
  datos[, c(predictoras_15, "dias_verduras"),
        drop = FALSE]
)

# Resultados por año y sexo
resultados <- list()

for (anio_actual in 2014:2025) {
  
  for (sexo_actual in c("Hombre", "Mujer")) {
    
    grupo <- datos$anio == anio_actual &
      as.character(datos$sexo_factor) == sexo_actual
    
    total <- sum(grupo)
    
    resultados[[length(resultados) + 1]] <- data.frame(
      anio = anio_actual,
      sexo = sexo_actual,
      total = total,
      completos_16 = sum(
        datos$completo_16[grupo]
      ),
      completos_15 = sum(
        datos$completo_15[grupo]
      ),
      porcentaje_15 = round(
        100 * mean(datos$completo_15[grupo]),
        2
      )
    )
  }
}

reporte <- do.call(rbind, resultados)
rownames(reporte) <- NULL

cat("\nCOMPARACION POR AÑO Y SEXO\n")
print(reporte, row.names = FALSE)

cat("\nRESUMEN GENERAL\n")
cat("Base completa:", nrow(datos), "\n")
cat("Completos con 16:",
    sum(datos$completo_16), "\n")
cat("Completos con 15:",
    sum(datos$completo_15), "\n")

cat("\nDIFERENCIA POR SEXO EN 2015-2018\n")
print(
  subset(
    reporte,
    anio >= 2015 & anio <= 2018
  ),
  row.names = FALSE
)

# Guardar resultados
write.csv(
  reporte,
  file.path(
    raiz, "resultados", "inventario",
    "21_completitud_15_por_sexo_anio.csv"
  ),
  row.names = FALSE
)

# Guardar base provisional sin eliminar registros
saveRDS(
  datos,
  file.path(
    raiz, "datos", "procesados",
    "ENDES_GAM_15_predictoras_provisional.rds"
  )
)

cat("\nSCRIPT 21 FINALIZADO.\n")
