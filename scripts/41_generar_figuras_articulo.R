
# ======================================================
# SCRIPT 41: FIGURAS PARA EL ARTICULO CIENTIFICO
# ENDES 2014-2025
# ======================================================

raiz <- "."

ruta_modelos <- file.path(
  raiz, "resultados", "modelos"
)

ruta_graficos <- file.path(
  raiz, "resultados", "graficos"
)

dir.create(
  ruta_graficos,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------
# 1. CARGAR RESULTADOS
# ------------------------------------------------------

endes <- read.csv(file.path(
  ruta_modelos,
  "40_estimaciones_anuales_survey.csv"
))

curva_anio <- read.csv(file.path(
  ruta_graficos,
  "27_efecto_anio.csv"
))

curva_edad <- read.csv(file.path(
  ruta_graficos,
  "27_efecto_edad.csv"
))

stopifnot(
  nrow(endes) == 12,
  all(2014:2025 %in% endes$anio)
)

# ------------------------------------------------------
# 2. FIGURA 1: EVOLUCION DESCRIPTIVA
# ------------------------------------------------------

png(
  file.path(
    ruta_graficos,
    "41_figura_1_evolucion_ENDES.png"
  ),
  width = 1800,
  height = 1100,
  res = 200
)

par(mar = c(5, 5, 4, 2))

plot(
  endes$anio,
  endes$porcentaje,
  type = "n",
  ylim = range(
    endes$IC_inferior,
    endes$IC_superior
  ),
  xaxt = "n",
  xlab = "Año",
  ylab = "Porcentaje ponderado (%)",
  main = "Ausencia reportada de compra de medicamentos"
)

axis(1, at = 2014:2025, las = 2)

polygon(
  c(endes$anio, rev(endes$anio)),
  c(endes$IC_inferior, rev(endes$IC_superior)),
  col = adjustcolor(
    "steelblue",
    alpha.f = 0.18
  ),
  border = NA
)

lines(
  endes$anio,
  endes$porcentaje,
  lwd = 3,
  col = "steelblue4"
)

points(
  endes$anio,
  endes$porcentaje,
  pch = 16,
  col = "steelblue4"
)

grid()

dev.off()

# ------------------------------------------------------
# 3. FUNCION PARA DIBUJAR EFECTOS GAM
# ------------------------------------------------------

graficar_gam <- function(
    tabla,
    nombre,
    titulo,
    eje_x
) {
  
  png(
    file.path(ruta_graficos, nombre),
    width = 1800,
    height = 1100,
    res = 200
  )
  
  par(mar = c(5, 5, 4, 2))
  
  plot(
    tabla$valor,
    tabla$efecto,
    type = "n",
    ylim = range(
      tabla$inferior,
      tabla$superior
    ),
    xlab = eje_x,
    ylab = "Efecto parcial (logit)",
    main = titulo
  )
  
  polygon(
    c(tabla$valor, rev(tabla$valor)),
    c(tabla$inferior, rev(tabla$superior)),
    col = adjustcolor(
      "steelblue",
      alpha.f = 0.20
    ),
    border = NA
  )
  
  abline(
    h = 0,
    lty = 2,
    col = "gray50"
  )
  
  lines(
    tabla$valor,
    tabla$efecto,
    col = "steelblue4",
    lwd = 3
  )
  
  grid()
  
  dev.off()
}

# ------------------------------------------------------
# 4. FIGURA 2: EFECTO TEMPORAL DEL GAM
# ------------------------------------------------------

graficar_gam(
  curva_anio,
  "41_figura_2_GAM_anio.png",
  "GAM: efecto parcial del año",
  "Año"
)

# ------------------------------------------------------
# 5. FIGURA 3: EFECTO DE EDAD DEL GAM
# ------------------------------------------------------

graficar_gam(
  curva_edad,
  "41_figura_3_GAM_edad.png",
  "GAM: efecto parcial de la edad",
  "Edad (años)"
)

# ------------------------------------------------------
# 6. TABLA DE RESULTADOS DESCRIPTIVOS
# ------------------------------------------------------

tabla_final <- endes[, c(
  "anio",
  "registros_cohorte",
  "porcentaje",
  "error_estandar",
  "IC_inferior",
  "IC_superior"
)]

names(tabla_final) <- c(
  "Anio",
  "Registros",
  "Porcentaje",
  "Error_estandar",
  "IC_inferior",
  "IC_superior"
)

write.csv(
  tabla_final,
  file.path(
    ruta_modelos,
    "41_tabla_resultados_articulo.csv"
  ),
  row.names = FALSE
)

cat("\nFIGURAS GENERADAS CORRECTAMENTE\n")
cat("41_figura_1_evolucion_ENDES.png\n")
cat("41_figura_2_GAM_anio.png\n")
cat("41_figura_3_GAM_edad.png\n")

cat("\nSCRIPT 41 FINALIZADO.\n")
