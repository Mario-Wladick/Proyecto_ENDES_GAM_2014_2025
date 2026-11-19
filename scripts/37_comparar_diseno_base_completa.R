
# ============================================================
# SCRIPT 37: COMPARAR ESTRUCTURA MUESTRAL
# BASE INTEGRADA VS COHORTE ANALITICA
# ENDES 2014-2025
# ============================================================

raiz <- "."

# 1. CARGAR COHORTE

cohorte <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

stopifnot(nrow(cohorte) == 32806)

# 2. FUNCION PARA ANALIZAR ESTRUCTURA
# Esta codificacion es candidata, no definitiva.

analizar_diseno <- function(d, anio) {
  
  necesarias <- c(
    "HV024", "HV022", "QHCLUSTER"
  )
  
  if (!all(necesarias %in% names(d))) {
    stop(paste(
      "Faltan variables de diseno en", anio
    ))
  }
  
  departamento <- trimws(as.character(d$HV024))
  estrato <- trimws(as.character(d$HV022))
  conglomerado <- trimws(as.character(d$QHCLUSTER))
  
  valido <- !is.na(departamento) &
    !is.na(estrato) &
    !is.na(conglomerado) &
    departamento != "" &
    estrato != "" &
    conglomerado != ""
  
  departamento <- departamento[valido]
  estrato <- estrato[valido]
  conglomerado <- conglomerado[valido]
  
  # Candidato de estratificacion
  if (anio %in% 2015:2017) {
    estrato_id <- paste(
      departamento, estrato, sep = "_"
    )
  } else {
    estrato_id <- estrato
  }
  
  pares <- unique(data.frame(
    estrato = estrato_id,
    conglomerado = conglomerado,
    stringsAsFactors = FALSE
  ))
  
  # Conglomerados distintos por estrato
  conteos <- table(pares$estrato)
  
  # Verificar si una clave de conglomerado
  # aparece asociada a multiples estratos.
  colisiones <- table(pares$conglomerado)
  
  data.frame(
    registros = nrow(d),
    registros_con_diseno = sum(valido),
    conglomerados = length(unique(conglomerado)),
    estratos = length(conteos),
    estratos_solitarios = sum(conteos == 1),
    estratos_con_2_o_mas = sum(conteos >= 2),
    conglomerados_varios_estratos =
      sum(colisiones > 1)
  )
}

# 3. RECORRER LOS DOCE AÑOS

resultados <- list()

for (a in 2014:2025) {
  
  cat("\nPROCESANDO ENDES", a, "...\n")
  flush.console()
  
  archivo <- file.path(
    raiz,
    "datos", "integrados",
    paste0("ENDES_integrada_", a, ".rds")
  )
  
  if (!file.exists(archivo)) {
    stop(paste("Archivo no encontrado:", archivo))
  }
  
  # Base integrada original
  base_completa <- readRDS(archivo)
  
  # Cohorte de personas seleccionadas
  subgrupo <- cohorte[
    cohorte$anio == a,
    ,
    drop = FALSE
  ]
  
  resumen_completo <- analizar_diseno(
    base_completa, a
  )
  
  resumen_cohorte <- analizar_diseno(
    subgrupo, a
  )
  
  resultados[[as.character(a)]] <- data.frame(
    anio = a,
    
    registros_base = resumen_completo$registros,
    registros_cohorte = resumen_cohorte$registros,
    
    sin_diseno_base =
      resumen_completo$registros -
      resumen_completo$registros_con_diseno,
    
    sin_diseno_cohorte =
      resumen_cohorte$registros -
      resumen_cohorte$registros_con_diseno,
    
    estratos_base = resumen_completo$estratos,
    estratos_cohorte = resumen_cohorte$estratos,
    
    solitarios_base =
      resumen_completo$estratos_solitarios,
    
    solitarios_cohorte =
      resumen_cohorte$estratos_solitarios,
    
    conglomerados_base =
      resumen_completo$conglomerados,
    
    conglomerados_cohorte =
      resumen_cohorte$conglomerados,
    
    conflictos_base =
      resumen_completo$conglomerados_varios_estratos,
    
    conflictos_cohorte =
      resumen_cohorte$conglomerados_varios_estratos
  )
}

# 4. UNIR RESULTADOS

tabla <- do.call(rbind, resultados)
rownames(tabla) <- NULL

cat("\n====================================\n")
cat("COMPARACION DE ESTRATOS\n")
cat("====================================\n")

print(
  tabla[, c(
    "anio",
    "registros_base",
    "registros_cohorte",
    "estratos_base",
    "estratos_cohorte",
    "solitarios_base",
    "solitarios_cohorte"
  )],
  row.names = FALSE
)

cat("\n====================================\n")
cat("COMPROBACION DE IDENTIFICADORES\n")
cat("====================================\n")

print(
  tabla[, c(
    "anio",
    "sin_diseno_base",
    "sin_diseno_cohorte",
    "conglomerados_base",
    "conglomerados_cohorte",
    "conflictos_base",
    "conflictos_cohorte"
  )],
  row.names = FALSE
)

# 5. GUARDAR RESULTADOS

destino <- file.path(
  raiz, "resultados", "modelos"
)

dir.create(
  destino,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  tabla,
  file.path(
    destino,
    "37_comparacion_diseno_completo.csv"
  ),
  row.names = FALSE
)

cat("\nSCRIPT 37 FINALIZADO.\n")
