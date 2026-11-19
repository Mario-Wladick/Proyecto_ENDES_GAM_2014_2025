
# ====================================================
# SCRIPT 34: AUDITORIA DEL DISENO MUESTRAL
# ENDES 2014-2025
# ====================================================

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

cat("\n1. REGISTROS DE LA BASE\n")
cat("Total:", nrow(datos), "\n")

# Variables de identificacion y diseno
candidatas <- c(
  "anio",
  "HHID",
  "QHCLUSTER",
  "HV021",
  "HV022",
  "peso_muestral"
)

cat("\n2. DISPONIBILIDAD DE VARIABLES\n")

print(data.frame(
  variable = candidatas,
  existe = candidatas %in% names(datos)
), row.names = FALSE)

# Revisar cantidad de datos faltantes
disponibles <- intersect(
  candidatas,
  names(datos)
)

cat("\n3. VALORES AUSENTES\n")

for (v in disponibles) {
  
  x <- as.character(datos[[v]])
  
  ausentes <- is.na(x) | trimws(x) == ""
  
  cat(
    v, ":",
    sum(ausentes),
    "ausentes\n"
  )
}

# Resumen de pesos por año
cat("\n4. PESOS MUESTRALES POR AÑO\n")

resumen_pesos <- do.call(
  rbind,
  lapply(sort(unique(datos$anio)), function(a) {
    
    d <- datos[
      datos$anio == a,
      ,
      drop = FALSE
    ]
    
    w <- d$peso_muestral
    
    data.frame(
      anio = a,
      registros = nrow(d),
      peso_min = min(w, na.rm = TRUE),
      peso_mediana = median(w, na.rm = TRUE),
      peso_max = max(w, na.rm = TRUE),
      peso_total = sum(w, na.rm = TRUE)
    )
  })
)

print(
  resumen_pesos,
  row.names = FALSE,
  digits = 5
)

# Verificar posibles identificadores de conglomerado
cat("\n5. CONGLOMERADOS Y ESTRATOS POR AÑO\n")

resumen_diseno <- do.call(
  rbind,
  lapply(sort(unique(datos$anio)), function(a) {
    
    d <- datos[
      datos$anio == a,
      ,
      drop = FALSE
    ]
    
    contar <- function(nombre) {
      
      if (!nombre %in% names(d)) {
        return(NA_integer_)
      }
      
      x <- as.character(d[[nombre]])
      x <- x[!is.na(x) & trimws(x) != ""]
      
      length(unique(x))
    }
    
    data.frame(
      anio = a,
      registros = nrow(d),
      conglomerados_QHCLUSTER = contar("QHCLUSTER"),
      conglomerados_HV021 = contar("HV021"),
      estratos_HV022 = contar("HV022")
    )
  })
)

print(
  resumen_diseno,
  row.names = FALSE
)

# Guardar resultados
destino <- file.path(
  raiz, "resultados", "modelos"
)

write.csv(
  resumen_pesos,
  file.path(destino, "34_resumen_pesos.csv"),
  row.names = FALSE
)

write.csv(
  resumen_diseno,
  file.path(destino, "34_resumen_diseno.csv"),
  row.names = FALSE
)

cat("\nSCRIPT 34 FINALIZADO.\n")
