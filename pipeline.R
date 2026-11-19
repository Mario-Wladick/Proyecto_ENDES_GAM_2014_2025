
# =====================================================
# PIPELINE ENDES 2014-2025
# Proyecto reproducible con Quarto
# ETAPA 1: Verificar la estructura del proyecto
# =====================================================

cat("\nINICIANDO PIPELINE ENDES 2014-2025\n")

# Verificar que estamos en la carpeta principal
archivos_necesarios <- c(
  "_quarto.yml",
  "scripts",
  "datos",
  "resultados"
)

faltantes <- archivos_necesarios[
  !file.exists(archivos_necesarios)
]

if (length(faltantes) > 0) {
  stop(
    "No estamos en la raiz del proyecto Quarto. ",
    "Faltan: ",
    paste(faltantes, collapse = ", ")
  )
}

cat("\n1. Directorio del proyecto:\n")
print(getwd())

# Contar scripts disponibles
scripts <- list.files(
  "scripts",
  pattern = "\\.R$",
  full.names = TRUE
)

cat("\n2. Scripts disponibles:\n")
cat(length(scripts), "\n")

# Verificar carpetas
carpetas <- c(
  "datos/originales",
  "datos/procesados",
  "scripts",
  "resultados/modelos",
  "resultados/graficos",
  "informe",
  "docs"
)

cat("\n3. Verificacion de carpetas:\n")

print(data.frame(
  carpeta = carpetas,
  existe = dir.exists(carpetas)
))

# Comprobar si existen datos para las etapas
cat("\n4. Archivos de datos disponibles:\n")

datos <- list.files(
  "datos",
  recursive = TRUE,
  full.names = TRUE
)

cat(length(datos), "archivos encontrados\n")

cat("\nETAPA 1 FINALIZADA CORRECTAMENTE\n")
cat("Todavia no se han ejecutado los modelos.\n")
