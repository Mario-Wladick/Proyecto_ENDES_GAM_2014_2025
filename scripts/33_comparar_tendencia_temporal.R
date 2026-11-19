
# ====================================================
# SCRIPT 33: COMPARACION DE TENDENCIAS TEMPORALES
# ENDES 2014-2025
#
# Modelo A: GAM con año suavizado
# Modelo B: GAM con año lineal
#
# Ambos utilizan 15 predictoras
# ====================================================

library(mgcv)

raiz <- "."

# 1. CARGAR DATOS

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

# 2. RECREAR VARIABLE VERDURAS

v <- datos$dias_verduras

datos$verduras_grupo <- factor(
  ifelse(
    is.na(v),
    "No informado",
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

# 3. PESOS NORMALIZADOS POR AÑO

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

cat("\nENTRENAMIENTO:", nrow(entrenamiento), "\n")
cat("PRUEBA:", nrow(prueba), "\n")

# 5. CARGAR MODELO CON AÑO SUAVIZADO

modelo_suave <- readRDS(file.path(
  raiz, "resultados", "modelos",
  "28_gam_ampliado_temporal.rds"
))

# 6. DEFINIR MODELO CON AÑO LINEAL
# Se conservan las mismas 15 predictoras

formula_lineal <- sin_medicacion ~
  anio +
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

cat("\nAJUSTANDO MODELO CON AÑO LINEAL...\n")
flush.console()

modelo_lineal <- mgcv::gam(
  formula_lineal,
  data = entrenamiento,
  family = quasibinomial(link = "logit"),
  weights = peso_modelo,
  method = "REML"
)

cat("\nMODELO LINEAL AJUSTADO.\n")

cat(
  "Rango:",
  modelo_lineal$rank,
  "/",
  length(coef(modelo_lineal)),
  "\n"
)

# 7. GENERAR PREDICCIONES

prueba$p_suave <- predict(
  modelo_suave,
  newdata = prueba,
  type = "response"
)

prueba$p_lineal <- predict(
  modelo_lineal,
  newdata = prueba,
  type = "response"
)

stopifnot(
  all(is.finite(prueba$p_suave)),
  all(is.finite(prueba$p_lineal))
)

# 8. FUNCION DE EVALUACION

evaluar <- function(y, p, w) {
  
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  
  observado <- weighted.mean(y, w)
  predicho <- weighted.mean(p, w)
  
  c(
    logloss = weighted.mean(
      -(y * log(p) + (1-y) * log(1-p)),
      w
    ),
    brier = weighted.mean((y-p)^2, w),
    observado_pct = 100 * observado,
    predicho_pct = 100 * predicho,
    error_pp = 100 * (predicho - observado)
  )
}

# 9. COMPARACION GLOBAL

comparacion <- rbind(
  GAM_anio_suave = evaluar(
    prueba$sin_medicacion,
    prueba$p_suave,
    prueba$peso_modelo
  ),
  GAM_anio_lineal = evaluar(
    prueba$sin_medicacion,
    prueba$p_lineal,
    prueba$peso_modelo
  )
)

cat("\n1. COMPARACION GLOBAL 2023-2025\n")
print(round(comparacion, 4))

# 10. COMPARACION POR AÑO

resultados <- do.call(
  rbind,
  lapply(2023:2025, function(a) {
    
    d <- prueba[
      prueba$anio == a,
      ,
      drop = FALSE
    ]
    
    suave <- evaluar(
      d$sin_medicacion,
      d$p_suave,
      d$peso_modelo
    )
    
    lineal <- evaluar(
      d$sin_medicacion,
      d$p_lineal,
      d$peso_modelo
    )
    
    data.frame(
      anio = a,
      registros = nrow(d),
      observado_pct = unname(
        suave["observado_pct"]
      ),
      predicho_suave_pct = unname(
        suave["predicho_pct"]
      ),
      predicho_lineal_pct = unname(
        lineal["predicho_pct"]
      ),
      error_suave_pp = unname(
        suave["error_pp"]
      ),
      error_lineal_pp = unname(
        lineal["error_pp"]
      ),
      logloss_suave = unname(
        suave["logloss"]
      ),
      logloss_lineal = unname(
        lineal["logloss"]
      )
    )
  })
)

cat("\n2. COMPARACION POR AÑO\n")

mostrar <- resultados

columnas <- setdiff(
  names(mostrar),
  c("anio", "registros")
)

mostrar[columnas] <- lapply(
  mostrar[columnas],
  round,
  digits = 3
)

print(mostrar, row.names = FALSE)

# 11. GUARDAR RESULTADOS

destino <- file.path(
  raiz, "resultados", "modelos"
)

saveRDS(
  modelo_lineal,
  file.path(
    destino,
    "33_gam_anio_lineal.rds"
  )
)

write.csv(
  comparacion,
  file.path(
    destino,
    "33_comparacion_global.csv"
  )
)

write.csv(
  resultados,
  file.path(
    destino,
    "33_comparacion_por_anio.csv"
  ),
  row.names = FALSE
)

cat("\nSCRIPT 33 FINALIZADO.\n")
