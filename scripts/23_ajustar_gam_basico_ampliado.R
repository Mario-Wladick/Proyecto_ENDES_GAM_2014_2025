
# SCRIPT 23: GAM BASICO Y AMPLIADO
# ENDES 2014-2025
# Primera comparacion exploratoria

if (!requireNamespace("mgcv", quietly = TRUE)) {
  install.packages("mgcv")
}

library(mgcv)

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

# Verificaciones
stopifnot(nrow(datos) == 32806)
stopifnot(all(datos$sin_medicacion %in% c(0, 1)))

# Revisar rangos numericos
cat("\nRESUMEN DE VARIABLES NUMERICAS\n")
print(summary(datos[, c(
  "edad", "miembros_hogar", "altitud"
)]))

# Comprobar pesos muestrales
stopifnot(
  all(is.finite(datos$peso_muestral)),
  all(datos$peso_muestral > 0)
)

# Normalizar pesos dentro de cada año.
# Uso exploratorio; NO reemplaza el diseño muestral.
datos$peso_modelo <- ave(
  datos$peso_muestral,
  datos$anio,
  FUN = function(x) x / mean(x)
)

# Separar entrenamiento y prueba (80/20)
# Estratificacion por año y respuesta
set.seed(2026)

datos$grupo <- interaction(
  datos$anio,
  datos$sin_medicacion,
  drop = TRUE
)

indices <- split(seq_len(nrow(datos)), datos$grupo)

entrenamiento_id <- sort(unlist(lapply(indices, function(id) {
  sample(id, size = floor(0.80 * length(id)))
}), use.names = FALSE))

entrenamiento <- datos[entrenamiento_id, , drop = FALSE]
prueba <- datos[-entrenamiento_id, , drop = FALSE]

cat("\nENTRENAMIENTO:", nrow(entrenamiento), "\n")
cat("PRUEBA:", nrow(prueba), "\n")

# MODELO 1: GAM BASICO
formula_basica <- sin_medicacion ~
  s(anio, k = 8) +
  s(edad, k = 10) +
  sexo_factor

cat("\nAJUSTANDO GAM BASICO...\n")

gam_basico <- mgcv::gam(
  formula_basica,
  data = entrenamiento,
  family = binomial(link = "logit"),
  weights = peso_modelo,
  method = "REML"
)

# MODELO 2: GAM AMPLIADO
formula_ampliada <- sin_medicacion ~
  s(anio, k = 8) +
  s(edad, k = 10) +
  sexo_factor +
  diabetes +
  fumo_12meses +
  alcohol_alguna_vez +
  alcohol_12meses +
  asistio_escuela +
  tiene_seguro +
  nivel_educativo +
  region +
  area_residencia +
  s(miembros_hogar, k = 6) +
  s(altitud, k = 8) +
  frutas_grupo

cat("\nAJUSTANDO GAM AMPLIADO...\n")

gam_ampliado <- mgcv::gam(
  formula_ampliada,
  data = entrenamiento,
  family = binomial(link = "logit"),
  weights = peso_modelo,
  method = "REML"
)

# Predicciones sobre las mismas personas
p_basico <- predict(
  gam_basico, newdata = prueba,
  type = "response"
)

p_ampliado <- predict(
  gam_ampliado, newdata = prueba,
  type = "response"
)

# Metricas ponderadas de evaluacion
evaluar <- function(y, p, w) {
  
  stopifnot(all(is.finite(p)))
  
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  
  c(
    logloss = weighted.mean(
      -(y * log(p) + (1-y) * log(1-p)), w
    ),
    brier = weighted.mean(
      (y-p)^2, w
    )
  )
}

metricas <- rbind(
  GAM_basico = evaluar(
    prueba$sin_medicacion,
    p_basico,
    prueba$peso_modelo
  ),
  GAM_ampliado = evaluar(
    prueba$sin_medicacion,
    p_ampliado,
    prueba$peso_modelo
  )
)

cat("\nRESULTADOS DE EVALUACION\n")
print(round(metricas, 6))

cat("\nRESUMEN GAM BASICO\n")
print(summary(gam_basico))

cat("\nRESUMEN GAM AMPLIADO\n")
print(summary(gam_ampliado))

# Guardar modelos y resultados
destino <- file.path(
  raiz, "resultados", "modelos"
)

dir.create(
  destino,
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  gam_basico,
  file.path(destino, "23_gam_basico.rds")
)

saveRDS(
  gam_ampliado,
  file.path(destino, "23_gam_ampliado.rds")
)

write.csv(
  metricas,
  file.path(destino, "23_comparacion_gam.csv")
)

cat("\nSCRIPT 23 FINALIZADO.\n")
