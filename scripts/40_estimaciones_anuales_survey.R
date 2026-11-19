
# =====================================================
# SCRIPT 40: ESTIMACIONES ANUALES CON SURVEY
# ENDES 2014-2025
# =====================================================

library(survey)

raiz <- "."

destino <- file.path(raiz, "resultados", "modelos")
dir.create(destino, recursive = TRUE, showWarnings = FALSE)

# Restaurar opciones al salir
opciones_originales <- options(
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE
)

# Funcion para convertir codigos
codigo <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

# Funcion para convertir numeros
numero <- function(x) {
  suppressWarnings(as.numeric(
    gsub(",", ".", codigo(x), fixed = TRUE)
  ))
}

# Variables de ponderacion identificadas anteriormente
variable_peso <- function(anio) {
  if (anio == 2014) {
    return("PESO_MAY15AÑOS")
  }
  if (anio == 2015) {
    return("PESO15_AJUS")
  }
  "PESO15_AMAS"
}

# Analizar un año
analizar_anio <- function(anio, metodo = "adjust") {
  
  cat("\nPROCESANDO ENDES", anio, "\n")
  flush.console()
  
  archivo <- file.path(
    raiz, "datos", "integrados",
    paste0("ENDES_integrada_", anio, ".rds")
  )
  
  d <- readRDS(archivo)
  nombre_peso <- variable_peso(anio)
  
  requeridas <- c(
    "HV024", "HV022", "QHCLUSTER",
    "QS102", "QS104", "QS23", "QSSEXO",
    nombre_peso
  )
  
  faltantes <- setdiff(requeridas, names(d))
  
  if (length(faltantes) > 0) {
    stop(paste(
      "Año", anio, "- variables ausentes:",
      paste(faltantes, collapse = ", ")
    ))
  }
  
  # Identificadores
  departamento <- codigo(d$HV024)
  estrato <- codigo(d$HV022)
  
  d$psu <- codigo(d$QHCLUSTER)
  
  # Codificacion candidata de estratos
  if (anio %in% 2015:2017) {
    d$estrato_id <- paste(
      departamento, estrato, sep = "_"
    )
  } else {
    d$estrato_id <- estrato
  }
  
  d$estrato_id[
    is.na(departamento) | is.na(estrato)
  ] <- NA_character_
  
  # Ponderacion original dividida por un millon
  d$peso <- numero(d[[nombre_peso]]) / 1000000
  
  # Variables de seleccion
  hipertension <- codigo(d$QS102)
  medicamentos <- codigo(d$QS104)
  edad <- numero(d$QS23)
  sexo <- codigo(d$QSSEXO)
  
  d$elegible <- (
    !is.na(hipertension) &
      hipertension == "1" &
      medicamentos %in% c("1", "2") &
      !is.na(edad) &
      edad >= 15 &
      sexo %in% c("1", "2")
  )
  
  d$sin_compra <- ifelse(
    d$elegible,
    as.numeric(medicamentos == "2"),
    NA_real_
  )
  
  # Validar identificadores y ponderaciones
  valido <- is.finite(d$peso) &
    d$peso > 0 &
    !is.na(d$psu) &
    d$psu != "" &
    !is.na(d$estrato_id) &
    d$estrato_id != ""
  
  elegibles_sin_diseno <- sum(
    d$elegible & !valido
  )
  
  d <- d[valido, , drop = FALSE]
  
  n_elegibles <- sum(d$elegible)
  
  # Contar estratos solitarios
  pares <- unique(
    d[, c("estrato_id", "psu")]
  )
  
  n_solitarios <- sum(
    table(pares$estrato_id) == 1
  )
  
  # Construir diseño
  diseno <- svydesign(
    ids = ~psu,
    strata = ~estrato_id,
    weights = ~peso,
    data = d,
    nest = TRUE
  )
  
  # Analizar subpoblacion
  subpoblacion <- subset(diseno, elegible)
  
  # Estimacion
  estimacion <- svymean(
    ~sin_compra,
    design = subpoblacion,
    na.rm = TRUE
  )
  
  intervalo <- confint(estimacion)
  
  data.frame(
    anio = anio,
    registros_base = nrow(d),
    registros_cohorte = n_elegibles,
    elegibles_sin_diseno = elegibles_sin_diseno,
    estratos_solitarios = n_solitarios,
    metodo = metodo,
    porcentaje = 100 * as.numeric(coef(estimacion)),
    error_estandar = 100 * as.numeric(SE(estimacion)),
    IC_inferior = 100 * intervalo[1, 1],
    IC_superior = 100 * intervalo[1, 2]
  )
}

# =====================================================
# 1. CALCULO ANUAL CON AJUSTE PROVISIONAL
# =====================================================

options(survey.lonely.psu = "adjust")

resultados <- do.call(
  rbind,
  lapply(
    2014:2025,
    function(a) analizar_anio(a, "adjust")
  )
)

rownames(resultados) <- NULL

cat("\n======================================\n")
cat("ESTIMACIONES ANUALES ENDES 2014-2025\n")
cat("======================================\n")

mostrar <- resultados[, c(
  "anio",
  "registros_cohorte",
  "porcentaje",
  "error_estandar",
  "IC_inferior",
  "IC_superior"
)]

mostrar[, 3:6] <- round(
  mostrar[, 3:6], 3
)

print(mostrar, row.names = FALSE)

# Verificacion del total analitico
cat("\nTOTAL DE LA COHORTE:\n")
print(sum(resultados$registros_cohorte))

if (sum(resultados$registros_cohorte) != 32806) {
  warning(
    "El total no coincide con 32806. Revisar filtros."
  )
}

# =====================================================
# 2. SENSIBILIDAD DE ESTRATOS SOLITARIOS
# =====================================================

cat("\n======================================\n")
cat("SENSIBILIDAD: AJUSTE VS PROMEDIO\n")
cat("======================================\n")

options(survey.lonely.psu = "average")

resultados_average <- do.call(
  rbind,
  lapply(
    2014:2025,
    function(a) analizar_anio(a, "average")
  )
)

sensibilidad <- data.frame(
  anio = resultados$anio,
  porcentaje = resultados$porcentaje,
  SE_adjust = resultados$error_estandar,
  SE_average = resultados_average$error_estandar
)

sensibilidad$diferencia_SE <- (
  sensibilidad$SE_average -
    sensibilidad$SE_adjust
)

print(
  round(sensibilidad, 4),
  row.names = FALSE
)

# =====================================================
# 3. GUARDAR RESULTADOS
# =====================================================

write.csv(
  resultados,
  file.path(
    destino,
    "40_estimaciones_anuales_survey.csv"
  ),
  row.names = FALSE
)

write.csv(
  sensibilidad,
  file.path(
    destino,
    "40_sensibilidad_estratos.csv"
  ),
  row.names = FALSE
)

# Restaurar opciones
options(opciones_originales)

cat("\nSCRIPT 40 FINALIZADO.\n")
