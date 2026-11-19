
# SCRIPT 27: CURVAS GAM DE AÑO Y EDAD
# ENDES 2014-2025

library(mgcv)

raiz <- "."

modelo <- readRDS(file.path(
  raiz, "resultados", "modelos",
  "26_gam_quasibinomial.rds"
))

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

destino <- file.path(raiz, "resultados", "graficos")
dir.create(destino, recursive = TRUE, showWarnings = FALSE)

# Preparar la variable de verduras como en el modelo
verduras <- datos$dias_verduras

datos$verduras_grupo <- factor(
  ifelse(
    is.na(verduras), "No informado",
    ifelse(verduras <= 2, "1-2 dias",
           ifelse(verduras <= 4, "3-4 dias", "5-7 dias"))
  ),
  levels = c(
    "1-2 dias", "3-4 dias",
    "5-7 dias", "No informado"
  )
)

# Funcion para obtener efectos parciales
obtener_curva <- function(variable, valores, termino) {
  
  # Una fila de referencia para las otras variables
  nuevos <- datos[rep(1L, length(valores)), , drop = FALSE]
  
  nuevos[[variable]] <- valores
  
  pred <- predict(
    modelo,
    newdata = nuevos,
    type = "terms",
    terms = termino,
    se.fit = TRUE
  )
  
  efecto <- as.numeric(pred$fit[, 1])
  error <- as.numeric(pred$se.fit[, 1])
  
  data.frame(
    valor = valores,
    efecto = efecto,
    inferior = efecto - 1.96 * error,
    superior = efecto + 1.96 * error
  )
}

# Curva temporal
curva_anio <- obtener_curva(
  "anio", 2014:2025, "s(anio)"
)

# Curva por edad
curva_edad <- obtener_curva(
  "edad", seq(15, 97, by = 1), "s(edad)"
)

# Funcion de dibujo
dibujar_curva <- function(tabla, titulo, eje_x, archivo) {
  
  png(
    filename = file.path(destino, archivo),
    width = 1600,
    height = 1000,
    res = 180
  )
  
  par(mar = c(5, 5, 4, 2))
  
  rango_y <- range(
    tabla$inferior,
    tabla$superior,
    finite = TRUE
  )
  
  plot(
    tabla$valor,
    tabla$efecto,
    type = "n",
    ylim = rango_y,
    xlab = eje_x,
    ylab = "Efecto parcial (escala logit)",
    main = titulo,
    cex.main = 1.15
  )
  
  polygon(
    c(tabla$valor, rev(tabla$valor)),
    c(tabla$inferior, rev(tabla$superior)),
    col = adjustcolor("steelblue", alpha.f = 0.20),
    border = NA
  )
  
  abline(h = 0, lty = 2, col = "gray50")
  
  lines(
    tabla$valor,
    tabla$efecto,
    col = "steelblue4",
    lwd = 3
  )
  
  dev.off()
}

dibujar_curva(
  curva_anio,
  "GAM: efecto parcial del año",
  "Año",
  "27_curva_GAM_anio.png"
)

dibujar_curva(
  curva_edad,
  "GAM: efecto parcial de la edad",
  "Edad (años)",
  "27_curva_GAM_edad.png"
)

# Guardar las tablas numericas
write.csv(
  curva_anio,
  file.path(destino, "27_efecto_anio.csv"),
  row.names = FALSE
)

write.csv(
  curva_edad,
  file.path(destino, "27_efecto_edad.csv"),
  row.names = FALSE
)

cat("\nCURVA TEMPORAL\n")
print(curva_anio, row.names = FALSE)

cat("\nGRAFICOS GENERADOS:\n")
cat("27_curva_GAM_anio.png\n")
cat("27_curva_GAM_edad.png\n")

cat("\nSCRIPT 27 FINALIZADO.\n")
