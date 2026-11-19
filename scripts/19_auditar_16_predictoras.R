
# SCRIPT 19: COMPLETAR PREDICTORAS Y AUDITAR AUSENCIAS
# ENDES 2014-2025

raiz <- "."

archivo <- file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_2014_2025_recodificada.rds"
)

datos <- readRDS(archivo)

# Convertir dias de consumo a valores numericos
convertir_dias <- function(x) {
  
  texto <- trimws(as.character(x))
  texto[is.na(texto) | texto == ""] <- NA_character_
  
  numero <- suppressWarnings(
    as.numeric(gsub(",", ".", texto, fixed = TRUE))
  )
  
  # Solo aceptar enteros entre 1 y 7
  numero[
    !is.na(numero) &
      (!is.finite(numero) |
         numero < 1 |
         numero > 7 |
         numero != floor(numero))
  ] <- NA_real_
  
  return(numero)
}

datos$dias_frutas <- convertir_dias(datos$QS213C)
datos$dias_verduras <- convertir_dias(datos$QS219C)

# Lista de 16 predictoras candidatas
predictoras <- c(
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
  "dias_frutas",
  "dias_verduras"
)

# Verificar columnas
stopifnot(all(predictoras %in% names(datos)))
stopifnot(nrow(datos) == 32806)

# Registros completos en las 16 predictoras
datos$completo_16 <- complete.cases(
  datos[, predictoras, drop = FALSE]
)

# Resumen por año
resumen <- do.call(rbind, lapply(2014:2025, function(a) {
  
  grupo <- datos$anio == a
  
  data.frame(
    anio = a,
    total = sum(grupo),
    completos_16 = sum(datos$completo_16[grupo]),
    incompletos_16 = sum(!datos$completo_16[grupo]),
    porcentaje_completos = round(
      100 * mean(datos$completo_16[grupo]), 2
    ),
    frutas_disponibles = sum(
      !is.na(datos$dias_frutas[grupo])
    ),
    verduras_disponibles = sum(
      !is.na(datos$dias_verduras[grupo])
    )
  )
}))

# Comprobar coincidencia de ausencias alimentarias
cat("\nAUSENCIAS CONJUNTAS: FRUTAS Y VERDURAS\n")

print(table(
  frutas_ausente = is.na(datos$dias_frutas),
  verduras_ausente = is.na(datos$dias_verduras)
))

cat("\nDISPONIBILIDAD CONJUNTA POR ANIO\n")
print(resumen, row.names = FALSE)

cat("\nTOTAL DE REGISTROS:", nrow(datos), "\n")
cat("PREDICTORAS CANDIDATAS:", length(predictoras), "\n")
cat("REGISTROS COMPLETOS EN LAS 16:",
    sum(datos$completo_16), "\n")

# Guardar sin sobrescribir archivos anteriores
saveRDS(
  datos,
  file.path(
    raiz, "datos", "procesados",
    "ENDES_GAM_2014_2025_16_candidatas.rds"
  )
)

write.csv(
  resumen,
  file.path(
    raiz, "resultados", "inventario",
    "19_disponibilidad_conjunta.csv"
  ),
  row.names = FALSE
)

cat("\nSCRIPT 19 FINALIZADO.\n")
