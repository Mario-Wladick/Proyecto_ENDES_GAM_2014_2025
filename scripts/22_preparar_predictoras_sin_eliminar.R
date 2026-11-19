
# SCRIPT 22: PREPARAR LAS 15 PREDICTORAS
# ENDES 2014-2025
# Estrategia preliminar de datos ausentes

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_provisional.rds"
))

# Convertir respuestas a texto limpio
limpiar <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA_character_
  x
}

# Preguntas con respuestas Si/No
# Los codigos desconocidos quedan identificados

# Recodificar preguntas binarias sin dejar NA
binaria_categoria <- function(x) {
  
  x <- limpiar(x)
  
  # Valor inicial para respuestas ausentes
  # o codigos no reconocidos
  resultado <- rep(
    "No informado",
    length(x)
  )
  
  # Respuestas validas
  resultado[!is.na(x) & x == "1"] <- "Si"
  resultado[!is.na(x) & x == "2"] <- "No"
  
  factor(
    resultado,
    levels = c(
      "No",
      "Si",
      "No informado"
    )
  )
}


# Mantener las 15 predictoras originales
datos$diabetes <- binaria_categoria(datos$QS109)
datos$fumo_12meses <- binaria_categoria(datos$QS200)
datos$alcohol_alguna_vez <- binaria_categoria(datos$QS206)
datos$asistio_escuela <- binaria_categoria(datos$QS24)
datos$tiene_seguro <- binaria_categoria(datos$QS26)

# Alcohol ultimos 12 meses:
# distinguir a quienes nunca consumieron alcohol
alguna_vez <- limpiar(datos$QS206)
ultimo_anio <- limpiar(datos$QS208)

alcohol12 <- rep("No informado", nrow(datos))

alcohol12[alguna_vez == "2" &
            (is.na(ultimo_anio) | ultimo_anio == "2")] <-
  "Nunca consumio"

alcohol12[alguna_vez == "1" &
            ultimo_anio == "2"] <- "No en ultimos 12 meses"

alcohol12[alguna_vez == "1" &
            ultimo_anio == "1"] <- "Si en ultimos 12 meses"

# Conservar contradicciones o respuestas no previstas
datos$alcohol_12meses <- factor(alcohol12)

# Nivel educativo
educacion <- limpiar(datos$QS25N)

niveles <- c(
  "Inicial",
  "Primaria",
  "Secundaria",
  "Superior no universitaria",
  "Superior universitaria",
  "Posgrado"
)

educacion_nueva <- ifelse(
  educacion %in% as.character(0:5),
  niveles[match(educacion, as.character(0:5))],
  "No informado"
)

datos$nivel_educativo <- factor(
  educacion_nueva,
  levels = c(niveles, "No informado")
)

# Dias de consumo de frutas:
# agrupaciones provisionales para evitar
# imputar arbitrariamente los valores ausentes
frutas <- datos$dias_frutas

frutas_grupo <- ifelse(
  is.na(frutas), "No informado",
  ifelse(
    frutas <= 2, "1-2 dias",
    ifelse(frutas <= 4, "3-4 dias", "5-7 dias")
  )
)

datos$frutas_grupo <- factor(
  frutas_grupo,
  levels = c(
    "1-2 dias", "3-4 dias",
    "5-7 dias", "No informado"
  )
)

# Lista final provisional: 15 predictoras
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
  "frutas_grupo"
)

# Comprobar disponibilidad conjunta
completos <- complete.cases(
  datos[, predictoras_15, drop = FALSE]
)

cat("\nVALIDACION DEL CONJUNTO DE 15 PREDICTORAS\n")
cat("Total de registros:", nrow(datos), "\n")
cat("Numero de predictoras:", length(predictoras_15), "\n")
cat("Registros completos:", sum(completos), "\n")
cat("Registros incompletos:", sum(!completos), "\n")

# Revisar nuevas categorias
for (variable in c(
  "diabetes", "fumo_12meses",
  "alcohol_12meses", "nivel_educativo",
  "tiene_seguro", "frutas_grupo"
)) {
  cat("\nVARIABLE:", variable, "\n")
  print(table(datos[[variable]], useNA = "ifany"))
}

# Guardar version nueva
saveRDS(
  datos,
  file.path(
    raiz, "datos", "procesados",
    "ENDES_GAM_15_predictoras_categorias.rds"
  )
)

cat("\nSCRIPT 22 FINALIZADO.\n")
