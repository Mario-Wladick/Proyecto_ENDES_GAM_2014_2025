# PROYECTO ENDES 2014-2025
# ETAPA 1: EXPLORAR VARIABLES DEL AÑO 2014

library(haven)

# Ubicación de la base original extraída
archivo <- paste0(
  "datos/extraidos/",
  "2014/441-Modulo414/CSALUD01.sav"
)

# Leer la base
datos_2014 <- read_sav(archivo)

# Mostrar número de registros
cat("Registros:", nrow(datos_2014), "\n")

# Mostrar número de variables
cat("Variables:", ncol(datos_2014), "\n")

# Obtener nombres y descripciones
diccionario <- data.frame(
  variable = names(datos_2014),
  descripcion = sapply(
    datos_2014,
    function(x) {
      etiqueta <- attr(x, "label")
      if (is.null(etiqueta)) NA_character_
      else as.character(etiqueta)
    }
  ),
  row.names = NULL
)

# Mostrar primeras variables
print(head(diccionario, 20))

# Guardar inventario
salida <- paste0(
  "./",
  "resultados/inventario/variables_2014.csv"
)

write.csv(
  diccionario,
  salida,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nInventario de 2014 generado correctamente.\n")