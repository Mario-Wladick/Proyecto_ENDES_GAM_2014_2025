
# ============================================================
# SCRIPT 42: CONTRASTES DE DIFERENCIAS ENTRE AÑOS
# PROYECTO ENDES 2014-2025
#
# Prueba de Wald aproximada
# Supuesto provisional: independencia entre años
#
# Sensibilidad:
# - survey.lonely.psu = "adjust"
# - survey.lonely.psu = "average"
# ============================================================

raiz <- "."

ruta_modelos <- file.path(
  raiz, "resultados", "modelos"
)

# ------------------------------------------------------------
# 1. CARGAR RESULTADOS ANUALES
# ------------------------------------------------------------

anuales <- read.csv(file.path(
  ruta_modelos,
  "40_estimaciones_anuales_survey.csv"
))

sensibilidad <- read.csv(file.path(
  ruta_modelos,
  "40_sensibilidad_estratos.csv"
))

cat("\n1. ARCHIVOS CARGADOS CORRECTAMENTE\n")

stopifnot(
  nrow(anuales) == 12,
  nrow(sensibilidad) == 12,
  !anyDuplicated(anuales$anio),
  !anyDuplicated(sensibilidad$anio),
  all(2014:2025 %in% anuales$anio),
  all(2014:2025 %in% sensibilidad$anio)
)

# Unir por año, evitando depender del orden de filas
datos <- merge(
  anuales[, c("anio", "porcentaje")],
  sensibilidad[, c(
    "anio", "SE_adjust", "SE_average"
  )],
  by = "anio",
  sort = TRUE
)

stopifnot(
  all(is.finite(datos$porcentaje)),
  all(is.finite(datos$SE_adjust)),
  all(is.finite(datos$SE_average)),
  all(datos$SE_adjust > 0),
  all(datos$SE_average > 0)
)

# ------------------------------------------------------------
# 2. DEFINIR COMPARACIONES
# ------------------------------------------------------------

comparaciones <- data.frame(
  inicial = c(2014, 2020, 2014, 2023),
  final = c(2020, 2025, 2025, 2025)
)

# ------------------------------------------------------------
# 3. FUNCION DE CONTRASTE
# ------------------------------------------------------------

contrastar <- function(inicial, final, metodo) {
  
  a <- datos[datos$anio == inicial, ]
  b <- datos[datos$anio == final, ]
  
  if (nrow(a) != 1 || nrow(b) != 1) {
    stop("No se encontro un año solicitado")
  }
  
  columna_se <- if (metodo == "adjust") {
    "SE_adjust"
  } else {
    "SE_average"
  }
  
  se_a <- a[[columna_se]]
  se_b <- b[[columna_se]]
  
  # Diferencia: año final menos año inicial
  diferencia <- b$porcentaje - a$porcentaje
  
  # Bajo independencia entre años
  se_diferencia <- sqrt(se_a^2 + se_b^2)
  
  # Prueba de Wald
  z <- diferencia / se_diferencia
  
  p_valor <- 2 * pnorm(-abs(z))
  
  # Intervalo de confianza aproximado del 95 %
  inferior <- diferencia - 1.96 * se_diferencia
  superior <- diferencia + 1.96 * se_diferencia
  
  data.frame(
    anio_inicial = inicial,
    anio_final = final,
    metodo = metodo,
    diferencia_pp = diferencia,
    error_estandar_pp = se_diferencia,
    z = z,
    p_valor = p_valor,
    IC_inferior = inferior,
    IC_superior = superior
  )
}

# ------------------------------------------------------------
# 4. EJECUTAR CONTRASTES
# ------------------------------------------------------------

resultados <- do.call(
  rbind,
  lapply(c("adjust", "average"), function(metodo) {
    
    do.call(
      rbind,
      lapply(
        seq_len(nrow(comparaciones)),
        function(i) {
          
          contrastar(
            comparaciones$inicial[i],
            comparaciones$final[i],
            metodo
          )
        }
      )
    )
  })
)

rownames(resultados) <- NULL

# ------------------------------------------------------------
# 5. CORRECCION POR COMPARACIONES MULTIPLES
# ------------------------------------------------------------

# Ajuste de Holm por separado para cada metodo
resultados$p_holm <- NA_real_

for (metodo in c("adjust", "average")) {
  
  indices <- which(resultados$metodo == metodo)
  
  resultados$p_holm[indices] <- p.adjust(
    resultados$p_valor[indices],
    method = "holm"
  )
}

resultados$significativo_holm <- (
  resultados$p_holm < 0.05
)

# ------------------------------------------------------------
# 6. MOSTRAR RESULTADOS
# ------------------------------------------------------------

cat("\n====================================\n")
cat("2. CONTRASTES DE WALD - METODO ADJUST\n")
cat("====================================\n")

tabla_adjust <- resultados[
  resultados$metodo == "adjust",
  ,
  drop = FALSE
]

mostrar <- tabla_adjust

columnas <- c(
  "diferencia_pp",
  "error_estandar_pp",
  "z",
  "p_valor",
  "p_holm",
  "IC_inferior",
  "IC_superior"
)

mostrar[columnas] <- lapply(
  mostrar[columnas],
  round,
  digits = 5
)

print(mostrar, row.names = FALSE)

cat("\n====================================\n")
cat("3. CONTRASTES DE WALD - METODO AVERAGE\n")
cat("====================================\n")

tabla_average <- resultados[
  resultados$metodo == "average",
  ,
  drop = FALSE
]

mostrar2 <- tabla_average

mostrar2[columnas] <- lapply(
  mostrar2[columnas],
  round,
  digits = 5
)

print(mostrar2, row.names = FALSE)

# ------------------------------------------------------------
# 7. COMPARACION DE CONCLUSIONES
# ------------------------------------------------------------

cat("\n====================================\n")
cat("4. SENSIBILIDAD DE LAS CONCLUSIONES\n")
cat("====================================\n")

comparacion <- merge(
  tabla_adjust[, c(
    "anio_inicial",
    "anio_final",
    "p_holm",
    "significativo_holm"
  )],
  tabla_average[, c(
    "anio_inicial",
    "anio_final",
    "p_holm",
    "significativo_holm"
  )],
  by = c("anio_inicial", "anio_final"),
  suffixes = c("_adjust", "_average")
)

print(comparacion, row.names = FALSE)

# ------------------------------------------------------------
# 8. GUARDAR ARCHIVOS
# ------------------------------------------------------------

write.csv(
  resultados,
  file.path(
    ruta_modelos,
    "42_contrastes_anuales.csv"
  ),
  row.names = FALSE
)

write.csv(
  comparacion,
  file.path(
    ruta_modelos,
    "42_sensibilidad_contrastes.csv"
  ),
  row.names = FALSE
)

cat("\n====================================\n")
cat("SCRIPT 42 FINALIZADO.\n")
cat("====================================\n")

cat("\nIMPORTANTE:\n")
cat(
  "Resultados aproximados bajo independencia",
  "de los estimadores anuales.\n"
)
cat(
  "La inferencia definitiva requiere validar",
  "la dependencia entre años y el diseño ENDES.\n"
)
