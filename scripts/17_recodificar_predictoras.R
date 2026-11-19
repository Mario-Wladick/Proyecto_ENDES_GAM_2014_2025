
# SCRIPT 17: RECODIFICAR PREDICTORAS PRINCIPALES
# ENDES 2014-2025

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_2014_2025_ampliada.rds"
))

# Funcion para limpiar texto
limpiar <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA_character_
  x
}

# Recodificar preguntas binarias
# 1 = Si, 2 = No, otros codigos = NA
recodificar_binaria <- function(x) {
  x <- limpiar(x)
  
  resultado <- rep(NA_integer_, length(x))
  
  resultado[x == "1"] <- 1L
  resultado[x == "2"] <- 0L
  
  resultado
}

# Convertir variables numericas
recodificar_numerica <- function(x) {
  x <- limpiar(x)
  
  suppressWarnings(
    as.numeric(gsub(",", ".", x, fixed = TRUE))
  )
}

# Crear predictoras recodificadas
datos$diabetes <- recodificar_binaria(datos$QS109)
datos$fumo_12meses <- recodificar_binaria(datos$QS200)
datos$alcohol_alguna_vez <- recodificar_binaria(datos$QS206)
datos$alcohol_12meses <- recodificar_binaria(datos$QS208)

datos$asistio_escuela <- recodificar_binaria(datos$QS24)
datos$tiene_seguro <- recodificar_binaria(datos$QS26)

# Variables numericas
datos$miembros_hogar <- recodificar_numerica(datos$HV009)
datos$altitud <- recodificar_numerica(datos$HV040)

# Variables geograficas
datos$region <- factor(limpiar(datos$HV024))
datos$area_residencia <- factor(limpiar(datos$HV025))

# Sexo
datos$sexo_factor <- factor(
  limpiar(datos$sexo),
  levels = c("1", "2"),
  labels = c("Hombre", "Mujer")
)

# Educacion: conservar codigos oficiales
educacion <- limpiar(datos$QS25N)

datos$nivel_educativo <- factor(
  educacion,
  levels = as.character(0:5),
  labels = c(
    "Inicial",
    "Primaria",
    "Secundaria",
    "Superior no universitaria",
    "Superior universitaria",
    "Posgrado"
  )
)

# Por ahora conservar frutas y verduras originales
# hasta verificar su escala y codigos completos

# Comprobar tamaño de la base
stopifnot(nrow(datos) == 32806)

# Guardar sin sobrescribir la base anterior
saveRDS(
  datos,
  file.path(
    raiz, "datos", "procesados",
    "ENDES_GAM_2014_2025_recodificada.rds"
  )
)

# Revisar disponibilidad de nuevas variables
predictoras <- c(
  "anio", "edad", "sexo_factor",
  "diabetes", "fumo_12meses",
  "alcohol_alguna_vez", "alcohol_12meses",
  "asistio_escuela", "tiene_seguro",
  "nivel_educativo", "region",
  "area_residencia", "miembros_hogar",
  "altitud"
)

resumen <- data.frame(
  variable = predictoras,
  presentes = sapply(
    datos[predictoras],
    function(x) sum(!is.na(x))
  ),
  ausentes = sapply(
    datos[predictoras],
    function(x) sum(is.na(x))
  )
)

resumen$porcentaje_presentes <- round(
  100 * resumen$presentes / nrow(datos),
  2
)

rownames(resumen) <- NULL

cat("\nPREDICTORAS RECODIFICADAS\n")
print(resumen, row.names = FALSE)

cat("\nTOTAL DE REGISTROS:", nrow(datos), "\n")
cat("TOTAL DE COLUMNAS:", ncol(datos), "\n")

write.csv(
  resumen,
  file.path(
    raiz, "resultados", "inventario",
    "17_resumen_predictoras_recodificadas.csv"
  ),
  row.names = FALSE
)

cat("\nRECODIFICACION FINALIZADA.\n")
