
# ==============================================
# PIPELINE DE PUBLICACION
# ENDES 2014-2025
# ==============================================

cat("\nINICIANDO REPRODUCCION DEL INFORME\n")

# 1. Verificar archivos necesarios
archivos <- c(
  "estimaciones_anuales.csv",
  "contrastes_exploratorios.csv",
  "27_efecto_anio.csv",
  "27_efecto_edad.csv",
  "28_comparacion_temporal.csv",
  "28_resultados_por_anio.csv",
  "29_calibracion_temporal.csv"
)

rutas <- file.path("datos/publicos", archivos)

verificacion <- data.frame(
  archivo = archivos,
  existe = file.exists(rutas)
)

print(verificacion)

if (!all(verificacion$existe)) {
  stop("Faltan resultados publicos.")
}

# 2. Verificar consistencia de resultados
anuales <- read.csv(rutas[1])
calibracion <- read.csv(rutas[7])
modelos <- read.csv(rutas[5])

stopifnot(
  identical(sort(as.integer(anuales$anio)), 2014:2025),
  identical(sort(as.integer(calibracion$anio)), 2023:2025),
  nrow(modelos) == 2
)

cat("\nRESULTADOS VALIDADOS\n")

# 3. Comprobar Quarto
quarto <- Sys.which("quarto")

if (!nzchar(quarto)) {
  stop("No se encontro Quarto instalado.")
}

# 4. Generar el informe HTML
cat("\nGENERANDO INFORME QUARTO\n")

estado <- system2(
  quarto,
  args = c(
    "render",
    "informe_ENDES.qmd",
    "--to",
    "html"
  )
)

if (estado != 0) {
  stop("No se pudo generar el informe.")
}

stopifnot(file.exists("informe_ENDES.html"))

cat("\nINFORME GENERADO CORRECTAMENTE\n")
cat("Archivo: informe_ENDES.html\n")
cat("\nPIPELINE DE PUBLICACION FINALIZADO\n")
