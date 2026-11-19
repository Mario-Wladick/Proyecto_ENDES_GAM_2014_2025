
# SCRIPT 29: CALIBRACION TEMPORAL GAM
# ENDES 2014-2025

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

# Recrear variable de verduras
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
    "1-2 dias", "3-4 dias",
    "5-7 dias", "No informado"
  )
)

# Cargar modelos entrenados hasta 2022
basico <- readRDS(file.path(
  raiz, "resultados", "modelos",
  "28_gam_basico_temporal.rds"
))

ampliado <- readRDS(file.path(
  raiz, "resultados", "modelos",
  "28_gam_ampliado_temporal.rds"
))

# Solo años reservados para evaluación
prueba <- datos[
  datos$anio %in% 2023:2025,
  ,
  drop = FALSE
]

# Pesos normalizados dentro de cada año
prueba$peso_modelo <- ave(
  prueba$peso_muestral,
  prueba$anio,
  FUN = function(x) x / mean(x)
)

# Probabilidades estimadas
prueba$p_basico <- predict(
  basico,
  newdata = prueba,
  type = "response"
)

prueba$p_ampliado <- predict(
  ampliado,
  newdata = prueba,
  type = "response"
)

stopifnot(
  all(is.finite(prueba$p_basico)),
  all(is.finite(prueba$p_ampliado))
)

# Calibracion por año
resultados <- do.call(rbind, lapply(
  2023:2025,
  function(a) {
    
    d <- prueba[
      prueba$anio == a,
      ,
      drop = FALSE
    ]
    
    w <- d$peso_modelo
    
    observado <- weighted.mean(
      d$sin_medicacion, w
    )
    
    pred_basico <- weighted.mean(
      d$p_basico, w
    )
    
    pred_ampliado <- weighted.mean(
      d$p_ampliado, w
    )
    
    data.frame(
      anio = a,
      registros = nrow(d),
      observado_pct = 100 * observado,
      basico_pct = 100 * pred_basico,
      ampliado_pct = 100 * pred_ampliado,
      error_basico_pp = 100 * (pred_basico - observado),
      error_ampliado_pp = 100 * (pred_ampliado - observado)
    )
  }
))

rownames(resultados) <- NULL

cat("\nCALIBRACION TEMPORAL 2023-2025\n")
print(
  round(resultados, 3),
  row.names = FALSE
)

# Guardar tabla
destino <- file.path(raiz, "resultados", "modelos")

write.csv(
  resultados,
  file.path(
    destino,
    "29_calibracion_temporal.csv"
  ),
  row.names = FALSE
)

# Grafico comparativo
ruta_grafico <- file.path(
  raiz, "resultados", "graficos",
  "29_calibracion_temporal.png"
)

png(ruta_grafico, width = 1500, height = 950, res = 170)

ylim <- range(
  resultados[, c(
    "observado_pct",
    "basico_pct",
    "ampliado_pct"
  )],
  finite = TRUE
)

plot(
  resultados$anio,
  resultados$observado_pct,
  type = "b",
  pch = 16,
  col = "black",
  lwd = 3,
  ylim = ylim,
  xaxt = "n",
  xlab = "Año",
  ylab = "Proporcion ponderada (%)",
  main = "ENDES: observado y predicho"
)

axis(1, at = 2023:2025)

lines(
  resultados$anio,
  resultados$basico_pct,
  type = "b",
  pch = 17,
  col = "steelblue",
  lwd = 2
)

lines(
  resultados$anio,
  resultados$ampliado_pct,
  type = "b",
  pch = 15,
  col = "darkgreen",
  lwd = 2
)

legend(
  "topleft",
  legend = c(
    "Observado",
    "GAM basico",
    "GAM ampliado"
  ),
  col = c("black", "steelblue", "darkgreen"),
  pch = c(16, 17, 15),
  lty = 1,
  lwd = 2,
  bty = "n"
)

dev.off()

cat("\nSCRIPT 29 FINALIZADO.\n")
