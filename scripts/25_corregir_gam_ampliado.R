
# SCRIPT 25: GAM AMPLIADO CORREGIDO
# ENDES 2014-2025

library(mgcv)

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

# Recuperar verduras del script 19 si fuese necesario
if (!"dias_verduras" %in% names(datos)) {
  stop("No se encontro dias_verduras")
}

# Crear categorias provisionales
verduras <- datos$dias_verduras

datos$verduras_grupo <- ifelse(
  is.na(verduras), "No informado",
  ifelse(verduras <= 2, "1-2 dias",
         ifelse(verduras <= 4, "3-4 dias", "5-7 dias")
  )
)

datos$verduras_grupo <- factor(
  datos$verduras_grupo,
  levels = c(
    "1-2 dias", "3-4 dias",
    "5-7 dias", "No informado"
  )
)

# Verificar 15 predictoras
predictoras <- c(
  "anio", "edad", "sexo_factor",
  "diabetes", "fumo_12meses",
  "alcohol_12meses", "asistio_escuela",
  "tiene_seguro", "nivel_educativo",
  "region", "area_residencia",
  "miembros_hogar", "altitud",
  "frutas_grupo", "verduras_grupo"
)

stopifnot(
  length(predictoras) == 15,
  nrow(datos) == 32806,
  all(complete.cases(datos[, predictoras])),
  all(is.finite(datos$peso_muestral)),
  all(datos$peso_muestral > 0)
)

# Recrear la particion del script 23
set.seed(2026)

datos$grupo <- interaction(
  datos$anio,
  datos$sin_medicacion,
  drop = TRUE
)

indices <- split(seq_len(nrow(datos)), datos$grupo)

id_entrenamiento <- sort(unlist(
  lapply(indices, function(id) {
    sample(id, size = floor(0.80 * length(id)))
  }),
  use.names = FALSE
))

# Pesos normalizados por año
datos$peso_modelo <- ave(
  datos$peso_muestral,
  datos$anio,
  FUN = function(x) x / mean(x)
)

entrenamiento <- datos[id_entrenamiento, , drop = FALSE]
prueba <- datos[-id_entrenamiento, , drop = FALSE]

# Formula corregida: 15 predictoras
# Se retiro alcohol_alguna_vez
formula_corregida <- sin_medicacion ~
  s(anio, k = 8) +
  s(edad, k = 10) +
  sexo_factor +
  diabetes +
  fumo_12meses +
  alcohol_12meses +
  asistio_escuela +
  tiene_seguro +
  nivel_educativo +
  region +
  area_residencia +
  s(miembros_hogar, k = 6) +
  s(altitud, k = 8) +
  frutas_grupo +
  verduras_grupo

cat("\nAJUSTANDO GAM CORREGIDO...\n")

gam_corregido <- mgcv::gam(
  formula_corregida,
  data = entrenamiento,
  family = binomial(link = "logit"),
  weights = peso_modelo,
  method = "REML"
)

cat("\nRANGO DEL MODELO\n")
cat("Rango:", gam_corregido$rank, "\n")
cat("Coeficientes:", length(coef(gam_corregido)), "\n")

cat("\nRESUMEN DEL MODELO\n")
print(summary(gam_corregido))

# Predicciones
p <- predict(
  gam_corregido,
  newdata = prueba,
  type = "response"
)

p <- pmin(pmax(p, 1e-12), 1 - 1e-12)

y <- prueba$sin_medicacion
w <- prueba$peso_modelo

logloss <- weighted.mean(
  -(y * log(p) + (1-y) * log(1-p)),
  w
)

brier <- weighted.mean((y-p)^2, w)

cat("\nRESULTADOS DEL GAM CORREGIDO\n")
cat("Logloss:", round(logloss, 6), "\n")
cat("Brier:", round(brier, 6), "\n")

cat("\nCONCURVIDAD\n")
print(mgcv::concurvity(gam_corregido, full = TRUE))

# Guardar
destino <- file.path(raiz, "resultados", "modelos")

saveRDS(
  gam_corregido,
  file.path(destino, "25_gam_ampliado_corregido.rds")
)

write.csv(
  data.frame(
    modelo = "GAM ampliado corregido",
    logloss = logloss,
    brier = brier,
    rango = gam_corregido$rank,
    coeficientes = length(coef(gam_corregido))
  ),
  file.path(destino, "25_resultados_gam_corregido.csv"),
  row.names = FALSE
)

cat("\nSCRIPT 25 FINALIZADO.\n")
