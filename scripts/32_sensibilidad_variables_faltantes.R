
# ==========================================================
# SCRIPT 32: ANALISIS DE SENSIBILIDAD
# PROYECTO ENDES 2014-2025
#
# Comparar:
# GAM original: 15 predictoras
# GAM sensibilidad: 12 predictoras
#
# Entrenamiento: 2014-2022
# Evaluacion: 2023-2025
# ==========================================================

library(mgcv)

raiz <- "."

# 1. CARGAR DATOS

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

# 2. RECREAR VERDURAS

v <- datos$dias_verduras

datos$verduras_grupo <- factor(
  ifelse(
    is.na(v), "No informado",
    ifelse(
      v <= 2, "1-2 dias",
      ifelse(v <= 4, "3-4 dias", "5-7 dias")
    )
  ),
  levels = c(
    "1-2 dias",
    "3-4 dias",
    "5-7 dias",
    "No informado"
  )
)

# 3. NORMALIZAR PESOS POR AÑO

stopifnot(
  all(is.finite(datos$peso_muestral)),
  all(datos$peso_muestral > 0)
)

datos$peso_modelo <- ave(
  datos$peso_muestral,
  datos$anio,
  FUN = function(x) x / mean(x)
)

# 4. SEPARACION TEMPORAL

entrenamiento <- datos[
  datos$anio <= 2022,
  ,
  drop = FALSE
]

prueba <- datos[
  datos$anio >= 2023,
  ,
  drop = FALSE
]

cat("\nENTRENAMIENTO:", nrow(entrenamiento))
cat("\nPRUEBA:", nrow(prueba), "\n")

# 5. CARGAR GAM ORIGINAL DE 15 PREDICTORAS

gam_15 <- readRDS(file.path(
  raiz, "resultados", "modelos",
  "28_gam_ampliado_temporal.rds"
))

# 6. FORMULA DE SENSIBILIDAD: 12 PREDICTORAS

formula_12 <- sin_medicacion ~
  s(anio, k = 7) +
  s(edad, k = 10) +
  sexo_factor +
  diabetes +
  fumo_12meses +
  alcohol_12meses +
  region +
  area_residencia +
  s(miembros_hogar, k = 6) +
  s(altitud, k = 8) +
  frutas_grupo +
  verduras_grupo

cat("\nAJUSTANDO MODELO DE SENSIBILIDAD...\n")
flush.console()

gam_12 <- mgcv::gam(
  formula_12,
  data = entrenamiento,
  family = quasibinomial(link = "logit"),
  weights = peso_modelo,
  method = "REML"
)

cat("\nMODELO DE SENSIBILIDAD AJUSTADO.\n")
cat(
  "Rango:",
  gam_12$rank,
  "/",
  length(coef(gam_12)),
  "\n"
)

# 7. PREDICCIONES

prueba$p15 <- predict(
  gam_15,
  newdata = prueba,
  type = "response"
)

prueba$p12 <- predict(
  gam_12,
  newdata = prueba,
  type = "response"
)

stopifnot(
  all(is.finite(prueba$p15)),
  all(is.finite(prueba$p12))
)

# 8. METRICAS

metricas <- function(y, p, w) {
  
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  
  c(
    logloss = weighted.mean(
      -(y * log(p) + (1 - y) * log(1 - p)),
      w
    ),
    brier = weighted.mean((y - p)^2, w),
    observado_pct = 100 * weighted.mean(y, w),
    predicho_pct = 100 * weighted.mean(p, w),
    error_pp = 100 * (
      weighted.mean(p, w) -
        weighted.mean(y, w)
    )
  )
}

# 9. COMPARACION GLOBAL

comparacion <- rbind(
  GAM_15 = metricas(
    prueba$sin_medicacion,
    prueba$p15,
    prueba$peso_modelo
  ),
  GAM_12 = metricas(
    prueba$sin_medicacion,
    prueba$p12,
    prueba$peso_modelo
  )
)

cat("\n1. COMPARACION GLOBAL 2023-2025\n")
print(round(comparacion, 4))

# 10. COMPARACION POR AÑO Y SEXO

grupos <- split(
  seq_len(nrow(prueba)),
  interaction(
    prueba$anio,
    prueba$sexo_factor,
    drop = TRUE
  )
)

resultados <- do.call(
  rbind,
  lapply(grupos, function(id) {
    
    d <- prueba[id, , drop = FALSE]
    
    m15 <- metricas(
      d$sin_medicacion,
      d$p15,
      d$peso_modelo
    )
    
    m12 <- metricas(
      d$sin_medicacion,
      d$p12,
      d$peso_modelo
    )
    
    data.frame(
      anio = d$anio[1],
      sexo = as.character(d$sexo_factor[1]),
      registros = nrow(d),
      observado_pct = unname(m15["observado_pct"]),
      predicho_15_pct = unname(m15["predicho_pct"]),
      predicho_12_pct = unname(m12["predicho_pct"]),
      error_15_pp = unname(m15["error_pp"]),
      error_12_pp = unname(m12["error_pp"]),
      logloss_15 = unname(m15["logloss"]),
      logloss_12 = unname(m12["logloss"])
    )
  })
)

resultados <- resultados[
  order(resultados$anio, resultados$sexo),
  ,
  drop = FALSE
]

rownames(resultados) <- NULL

# Redondear solo columnas numericas de resultados
mostrar <- resultados

columnas <- setdiff(
  names(mostrar),
  c("anio", "sexo", "registros")
)

mostrar[columnas] <- lapply(
  mostrar[columnas],
  round,
  digits = 3
)

cat("\n2. RESULTADOS POR AÑO Y SEXO\n")
print(mostrar, row.names = FALSE)

# 11. GUARDAR ARCHIVOS

destino <- file.path(
  raiz, "resultados", "modelos"
)

dir.create(
  destino,
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  gam_12,
  file.path(destino, "32_gam_sensibilidad_12.rds")
)

write.csv(
  comparacion,
  file.path(destino, "32_comparacion_global.csv")
)

write.csv(
  resultados,
  file.path(destino, "32_comparacion_sexo_anio.csv"),
  row.names = FALSE
)

cat("\nSCRIPT 32 FINALIZADO.\n")
