
# SCRIPT 26: COMPROBAR GAM CON QUASIBINOMIAL
# ENDES 2014-2025

library(mgcv)

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

# Crear variable de verduras
verduras <- datos$dias_verduras

datos$verduras_grupo <- factor(
  ifelse(
    is.na(verduras), "No informado",
    ifelse(
      verduras <= 2, "1-2 dias",
      ifelse(verduras <= 4, "3-4 dias", "5-7 dias")
    )
  ),
  levels = c(
    "1-2 dias", "3-4 dias",
    "5-7 dias", "No informado"
  )
)

# Pesos normalizados por año
datos$peso_modelo <- ave(
  datos$peso_muestral,
  datos$anio,
  FUN = function(x) x / mean(x)
)

# Reproducir division entrenamiento/prueba
set.seed(2026)

grupos <- interaction(
  datos$anio,
  datos$sin_medicacion,
  drop = TRUE
)

indices <- split(seq_len(nrow(datos)), grupos)

id_entrenamiento <- sort(unlist(
  lapply(indices, function(id) {
    sample(id, floor(0.80 * length(id)))
  }),
  use.names = FALSE
))

entrenamiento <- datos[id_entrenamiento, , drop = FALSE]
prueba <- datos[-id_entrenamiento, , drop = FALSE]

# Recuperar formula del GAM corregido
modelo_anterior <- readRDS(file.path(
  raiz, "resultados", "modelos",
  "25_gam_ampliado_corregido.rds"
))

formula_modelo <- formula(modelo_anterior)

# Ajustar con quasibinomial
cat("\nAJUSTANDO GAM QUASIBINOMIAL...\n")

gam_quasi <- mgcv::gam(
  formula_modelo,
  data = entrenamiento,
  family = quasibinomial(link = "logit"),
  weights = peso_modelo,
  method = "REML"
)

# Comprobar rango
cat("\nRANGO DEL MODELO\n")
cat("Rango:", gam_quasi$rank, "\n")
cat("Coeficientes:", length(coef(gam_quasi)), "\n")

# Predicciones sobre el mismo conjunto
p_quasi <- predict(
  gam_quasi,
  newdata = prueba,
  type = "response"
)

p_anterior <- predict(
  modelo_anterior,
  newdata = prueba,
  type = "response"
)

evaluar <- function(y, p, w) {
  
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

comparacion <- rbind(
  GAM_binomial = evaluar(
    prueba$sin_medicacion,
    p_anterior,
    prueba$peso_modelo
  ),
  GAM_quasibinomial = evaluar(
    prueba$sin_medicacion,
    p_quasi,
    prueba$peso_modelo
  )
)

cat("\nCOMPARACION DE RESULTADOS\n")
print(round(comparacion, 6))

cat("\nDIFERENCIA MAXIMA ENTRE PREDICCIONES\n")
print(max(abs(p_quasi - p_anterior)))

cat("\nRESUMEN QUASIBINOMIAL\n")
print(summary(gam_quasi))

# Guardar resultados
destino <- file.path(raiz, "resultados", "modelos")

saveRDS(
  gam_quasi,
  file.path(destino, "26_gam_quasibinomial.rds")
)

write.csv(
  comparacion,
  file.path(destino, "26_comparacion_familias.csv")
)

cat("\nSCRIPT 26 FINALIZADO.\n")
