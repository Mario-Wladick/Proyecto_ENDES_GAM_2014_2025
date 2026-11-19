
# SCRIPT 28: VALIDACION TEMPORAL DEL GAM
# ENDES 2014-2025

library(mgcv)

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

# Preparar verduras igual que en el script 25
v <- datos$dias_verduras

datos$verduras_grupo <- factor(
  ifelse(
    is.na(v), "No informado",
    ifelse(v <= 2, "1-2 dias",
           ifelse(v <= 4, "3-4 dias", "5-7 dias"))
  ),
  levels = c(
    "1-2 dias", "3-4 dias",
    "5-7 dias", "No informado"
  )
)

# Separacion cronologica
entrenamiento <- datos[datos$anio <= 2022, ]
prueba <- datos[datos$anio >= 2023, ]

# Normalizar pesos por año en cada conjunto
normalizar <- function(df) {
  df$peso_modelo <- ave(
    df$peso_muestral, df$anio,
    FUN = function(x) x / mean(x)
  )
  df
}

entrenamiento <- normalizar(entrenamiento)
prueba <- normalizar(prueba)

cat("\nENTRENAMIENTO:", nrow(entrenamiento))
cat("\nPRUEBA:", nrow(prueba), "\n")

# Dos formulas comparables
formula_basica <- sin_medicacion ~
  s(anio, k = 7) +
  s(edad, k = 10) +
  sexo_factor

formula_ampliada <- sin_medicacion ~
  s(anio, k = 7) +
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

# Entrenar ambos modelos solo hasta 2022
basico <- mgcv::gam(
  formula_basica,
  data = entrenamiento,
  family = quasibinomial(link = "logit"),
  weights = peso_modelo,
  method = "REML"
)

ampliado <- mgcv::gam(
  formula_ampliada,
  data = entrenamiento,
  family = quasibinomial(link = "logit"),
  weights = peso_modelo,
  method = "REML"
)

# Predecir años 2023-2025
p_basico <- predict(
  basico, newdata = prueba, type = "response"
)

p_ampliado <- predict(
  ampliado, newdata = prueba, type = "response"
)

# Metricas ponderadas
evaluar <- function(y, p, w) {
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  
  c(
    logloss = weighted.mean(
      -(y * log(p) + (1-y) * log(1-p)), w
    ),
    brier = weighted.mean((y-p)^2, w)
  )
}

comparacion <- rbind(
  GAM_basico = evaluar(
    prueba$sin_medicacion,
    p_basico, prueba$peso_modelo
  ),
  GAM_ampliado = evaluar(
    prueba$sin_medicacion,
    p_ampliado, prueba$peso_modelo
  )
)

cat("\nVALIDACION TEMPORAL 2023-2025\n")
print(round(comparacion, 6))

# Evaluacion por año
por_anio <- do.call(rbind, lapply(
  2023:2025, function(a) {
    
    i <- prueba$anio == a
    
    mb <- evaluar(
      prueba$sin_medicacion[i],
      p_basico[i], prueba$peso_modelo[i]
    )
    
    ma <- evaluar(
      prueba$sin_medicacion[i],
      p_ampliado[i], prueba$peso_modelo[i]
    )
    
    data.frame(
      anio = a,
      registros = sum(i),
      logloss_basico = mb["logloss"],
      logloss_ampliado = ma["logloss"],
      brier_basico = mb["brier"],
      brier_ampliado = ma["brier"]
    )
  }
))

rownames(por_anio) <- NULL

cat("\nRESULTADOS POR AÑO\n")
print(por_anio, row.names = FALSE)

# Guardar resultados
destino <- file.path(raiz, "resultados", "modelos")

saveRDS(
  basico,
  file.path(destino, "28_gam_basico_temporal.rds")
)

saveRDS(
  ampliado,
  file.path(destino, "28_gam_ampliado_temporal.rds")
)

write.csv(
  comparacion,
  file.path(destino, "28_comparacion_temporal.csv")
)

write.csv(
  por_anio,
  file.path(destino, "28_resultados_por_anio.csv"),
  row.names = FALSE
)

cat("\nSCRIPT 28 FINALIZADO.\n")
