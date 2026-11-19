# PROYECTO ENDES 2014-2025
# ETAPA 1: CREAR ESTRUCTURA DEL PROYECTO

raiz <- "."

carpetas <- c(
  "datos/procesados",
  "scripts",
  "resultados/inventario",
  "resultados/graficos",
  "resultados/modelos",
  "informes",
  "logs"
)

for (carpeta in carpetas) {
  dir.create(
    file.path(raiz, carpeta),
    recursive = TRUE,
    showWarnings = FALSE
  )
}

cat("Estructura del proyecto R creada correctamente.\n")