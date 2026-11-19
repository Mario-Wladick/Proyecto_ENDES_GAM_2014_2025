
# =====================================================
# SCRIPT 39: DISENO MUESTRAL ENDES 2025
# Primera estimacion exploratoria con survey
# =====================================================

if (!requireNamespace("survey", quietly = TRUE)) {
  install.packages("survey")
}

library(survey)

raiz <- "."

archivo <- file.path(
  raiz, "datos", "integrados",
  "ENDES_integrada_2025.rds"
)

base <- readRDS(archivo)

cat("\n1. BASE ORIGINAL INTEGRADA\n")
cat("Registros:", nrow(base), "\n")

# -----------------------------------------------------
# 1. VALIDAR VARIABLES
# -----------------------------------------------------

necesarias <- c(
  "QHCLUSTER",
  "HV022",
  "QS102",
  "QS104",
  "QS23",
  "QSSEXO",
  "PESO15_AMAS"
)

faltantes <- setdiff(necesarias, names(base))

if (length(faltantes) > 0) {
  stop(paste(
    "Variables no encontradas:",
    paste(faltantes, collapse = ", ")
  ))
}

# -----------------------------------------------------
# 2. PREPARAR VARIABLES
# -----------------------------------------------------

codigo <- function(x) {
  trimws(as.character(x))
}

numero <- function(x) {
  suppressWarnings(as.numeric(
    gsub(",", ".", codigo(x), fixed = TRUE)
  ))
}

base$psu <- codigo(base$QHCLUSTER)
base$estrato <- codigo(base$HV022)

base$peso <- numero(base$PESO15_AMAS) / 1000000

edad <- numero(base$QS23)
sexo <- codigo(base$QSSEXO)
hipertension <- codigo(base$QS102)
medicamentos <- codigo(base$QS104)

# -----------------------------------------------------
# 3. IDENTIFICAR COHORTE ANALITICA
# -----------------------------------------------------

base$elegible <- !is.na(hipertension) &
  hipertension == "1" &
  medicamentos %in% c("1", "2") &
  !is.na(edad) &
  edad >= 15 &
  sexo %in% c("1", "2")

# Respuesta: 1 = no compro medicamentos
base$sin_compra <- ifelse(
  base$elegible,
  as.numeric(medicamentos == "2"),
  NA_real_
)

# -----------------------------------------------------
# 4. FILTRAR REGISTROS SIN DISENO VALIDO
# -----------------------------------------------------

valido <- is.finite(base$peso) &
  base$peso > 0 &
  !is.na(base$psu) &
  base$psu != "" &
  !is.na(base$estrato) &
  base$estrato != ""

cat("\n2. CALIDAD DEL DISENO\n")
cat("Registros sin diseno valido:", sum(!valido), "\n")

base_diseno <- base[valido, , drop = FALSE]

cat("Registros con diseno valido:",
    nrow(base_diseno), "\n")

cat("Personas elegibles:",
    sum(base_diseno$elegible), "\n")

# -----------------------------------------------------
# 5. IDENTIFICAR ESTRATOS SOLITARIOS
# -----------------------------------------------------

pares <- unique(base_diseno[, c("estrato", "psu")])

conteo <- table(pares$estrato)

cat("\n3. ESTRATOS SOLITARIOS\n")

cat("Total de estratos:", length(conteo), "\n")
cat("Estratos con un conglomerado:", sum(conteo == 1), "\n")

# -----------------------------------------------------
# 6. CONFIGURACION PROVISIONAL
# -----------------------------------------------------

# Ajuste exploratorio para estratos con un PSU.
# No equivale a confirmar el procedimiento oficial.
# Se restaura la configuracion al terminar.

opciones_anteriores <- options(
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE
)

# -----------------------------------------------------
# 7. CONSTRUIR DISENO MUESTRAL
# -----------------------------------------------------

diseno <- survey::svydesign(
  ids = ~psu,
  strata = ~estrato,
  weights = ~peso,
  data = base_diseno,
  nest = TRUE
)

cat("\n4. DISENO CREADO\n")
cat("Registros:", nrow(base_diseno), "\n")

# -----------------------------------------------------
# 8. SELECCIONAR SUBPOBLACION
# -----------------------------------------------------

subpoblacion <- subset(
  diseno,
  elegible
)

cat("\n5. SUBPOBLACION CON HIPERTENSION\n")
cat("Registros:", sum(base_diseno$elegible), "\n")

# -----------------------------------------------------
# 9. ESTIMACION CON DISENO COMPLEJO
# -----------------------------------------------------

estimacion <- survey::svymean(
  ~sin_compra,
  design = subpoblacion,
  na.rm = TRUE
)

porcentaje <- 100 * as.numeric(coef(estimacion))

error_estandar <- 100 * as.numeric(
  SE(estimacion)
)

intervalo <- 100 * confint(estimacion)

resultado <- data.frame(
  anio = 2025,
  registros = sum(base_diseno$elegible),
  porcentaje = porcentaje,
  error_estandar = error_estandar,
  IC_inferior = intervalo[1, 1],
  IC_superior = intervalo[1, 2]
)

cat("\n6. ESTIMACION EXPLORATORIA ENDES 2025\n")

print(
  round(resultado, 4),
  row.names = FALSE
)

# -----------------------------------------------------
# 10. COMPARACION CON EL RESULTADO ANTERIOR
# -----------------------------------------------------

cat("\n7. COMPARACION DESCRIPTIVA\n")

cat("Resultado previo del script 30: 29.966 %\n")
cat("Resultado con survey:",
    round(porcentaje, 3), "%\n")

cat(
  "Diferencia en puntos porcentuales:",
  round(porcentaje - 29.966, 3),
  "\n"
)

# -----------------------------------------------------
# 11. GUARDAR RESULTADOS
# -----------------------------------------------------

destino <- file.path(
  raiz, "resultados", "modelos"
)

write.csv(
  resultado,
  file.path(
    destino,
    "39_estimacion_survey_2025.csv"
  ),
  row.names = FALSE
)

# Restaurar opciones anteriores
options(opciones_anteriores)

cat("\nSCRIPT 39 FINALIZADO.\n")
