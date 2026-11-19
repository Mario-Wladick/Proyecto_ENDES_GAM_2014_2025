
# ============================================================
# SCRIPT 38: IDENTIFICAR ESTRATOS CON UN CONGLOMERADO
# PROYECTO ENDES 2014-2025
# ============================================================

raiz <- "."

destino <- file.path(
  raiz, "resultados", "modelos"
)

dir.create(
  destino,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 1. FUNCION PARA ANALIZAR CADA AÑO
# ------------------------------------------------------------

analizar_anio <- function(anio) {
  
  archivo <- file.path(
    raiz,
    "datos", "integrados",
    paste0("ENDES_integrada_", anio, ".rds")
  )
  
  datos <- readRDS(archivo)
  
  necesarias <- c("HV024", "HV022", "QHCLUSTER")
  
  if (!all(necesarias %in% names(datos))) {
    stop(paste("Faltan variables en el año", anio))
  }
  
  departamento <- trimws(as.character(datos$HV024))
  estrato <- trimws(as.character(datos$HV022))
  conglomerado <- trimws(as.character(datos$QHCLUSTER))
  
  validos <- !is.na(departamento) &
    !is.na(estrato) &
    !is.na(conglomerado) &
    departamento != "" &
    estrato != "" &
    conglomerado != ""
  
  if (!all(validos)) {
    warning(paste(
      anio, ": registros sin identificadores de diseño"
    ))
  }
  
  departamento <- departamento[validos]
  estrato <- estrato[validos]
  conglomerado <- conglomerado[validos]
  
  # Identificador candidato de estrato
  # No implica confirmacion oficial
  estrato_id <- if (anio %in% 2015:2017) {
    paste(departamento, estrato, sep = "_")
  } else {
    estrato
  }
  
  base <- unique(data.frame(
    departamento = departamento,
    estrato_id = estrato_id,
    conglomerado = conglomerado,
    stringsAsFactors = FALSE
  ))
  
  # Numero de conglomerados distintos por estrato
  conteo <- aggregate(
    conglomerado ~ estrato_id,
    data = base,
    FUN = function(x) length(unique(x))
  )
  
  names(conteo)[2] <- "n_conglomerados"
  
  solitarios <- conteo[
    conteo$n_conglomerados == 1,
    ,
    drop = FALSE
  ]
  
  # Obtener departamento y numero de registros
  if (nrow(solitarios) > 0) {
    
    detalle <- do.call(
      rbind,
      lapply(solitarios$estrato_id, function(e) {
        
        seleccion <- estrato_id == e
        
        data.frame(
          anio = anio,
          estrato = e,
          departamento = paste(
            sort(unique(departamento[seleccion])),
            collapse = ","
          ),
          conglomerado = paste(
            unique(conglomerado[seleccion]),
            collapse = ","
          ),
          registros = sum(seleccion),
          stringsAsFactors = FALSE
        )
      })
    )
    
  } else {
    
    detalle <- data.frame(
      anio = integer(0),
      estrato = character(0),
      departamento = character(0),
      conglomerado = character(0),
      registros = integer(0)
    )
  }
  
  resumen <- data.frame(
    anio = anio,
    registros_base = nrow(datos),
    estratos = nrow(conteo),
    estratos_solitarios = nrow(detalle),
    conglomerados = length(unique(conglomerado))
  )
  
  list(
    resumen = resumen,
    detalle = detalle
  )
}

# ------------------------------------------------------------
# 2. REVISAR LOS DOCE AÑOS
# ------------------------------------------------------------

resultados <- lapply(2014:2025, function(a) {
  
  cat("\nANALIZANDO AÑO:", a, "\n")
  flush.console()
  
  analizar_anio(a)
})

# ------------------------------------------------------------
# 3. CONSOLIDAR RESULTADOS
# ------------------------------------------------------------

resumen <- do.call(
  rbind,
  lapply(resultados, function(x) x$resumen)
)

detalle <- do.call(
  rbind,
  lapply(resultados, function(x) x$detalle)
)

rownames(resumen) <- NULL
rownames(detalle) <- NULL

cat("\n========================================\n")
cat("1. RESUMEN ANUAL\n")
cat("========================================\n")

print(resumen, row.names = FALSE)

cat("\n========================================\n")
cat("2. DETALLE DE ESTRATOS SOLITARIOS\n")
cat("========================================\n")

print(detalle, row.names = FALSE)

# ------------------------------------------------------------
# 4. COMPARAR CODIGOS ENTRE 2021-2025
# ------------------------------------------------------------

recientes <- detalle[
  detalle$anio >= 2021,
  ,
  drop = FALSE
]

if (nrow(recientes) > 0) {
  
  presencia <- table(
    Estrato = recientes$estrato,
    Anio = recientes$anio
  )
  
  cat("\n========================================\n")
  cat("3. REPETICION DE CODIGOS 2021-2025\n")
  cat("========================================\n")
  
  print(presencia)
  
  cat("\nCODIGOS QUE APARECEN EN LOS CINCO AÑOS:\n")
  
  repetidos <- rownames(presencia)[
    rowSums(presencia > 0) == 5
  ]
  
  print(repetidos)
  
  cat(
    "\nTotal de codigos repetidos:",
    length(repetidos), "\n"
  )
}

# ------------------------------------------------------------
# 5. GUARDAR RESULTADOS
# ------------------------------------------------------------

write.csv(
  resumen,
  file.path(destino, "38_resumen_estratos.csv"),
  row.names = FALSE
)

write.csv(
  detalle,
  file.path(destino, "38_detalle_estratos_solitarios.csv"),
  row.names = FALSE
)

cat("\nSCRIPT 38 FINALIZADO.\n")
