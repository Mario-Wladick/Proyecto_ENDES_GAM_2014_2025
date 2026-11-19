
# ===========================================================
# SCRIPT 36: AUDITORIA DE ESTRATOS Y CONGLOMERADOS
# PROYECTO ENDES 2014-2025
# ===========================================================

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

# -----------------------------------------------------------
# 1. VERIFICAR VARIABLES
# -----------------------------------------------------------

necesarias <- c(
  "anio",
  "HV024",
  "HV022",
  "QHCLUSTER",
  "peso_muestral"
)

stopifnot(
  all(necesarias %in% names(datos)),
  nrow(datos) == 32806
)

# -----------------------------------------------------------
# 2. PREPARAR IDENTIFICADORES COMO TEXTO
# -----------------------------------------------------------

datos$departamento <- trimws(
  as.character(datos$HV024)
)

datos$estrato_original <- trimws(
  as.character(datos$HV022)
)

datos$conglomerado_original <- trimws(
  as.character(datos$QHCLUSTER)
)

# Candidatos de identificacion
# Solo para diagnostico: NO se consideran definitivos.

datos$estrato_departamento <- interaction(
  datos$departamento,
  datos$estrato_original,
  drop = TRUE
)

datos$estrato_anio_departamento <- interaction(
  datos$anio,
  datos$departamento,
  datos$estrato_original,
  drop = TRUE
)

# -----------------------------------------------------------
# 3. COMPARAR FORMAS DE IDENTIFICAR ESTRATOS
# -----------------------------------------------------------

cat("\n1. COMPARACION DE ESTRATOS POR AÑO\n")

resumen <- do.call(
  rbind,
  lapply(2014:2025, function(a) {
    
    d <- datos[
      datos$anio == a,
      ,
      drop = FALSE
    ]
    
    data.frame(
      anio = a,
      registros = nrow(d),
      departamentos = length(unique(d$departamento)),
      estratos_HV022 = length(
        unique(d$estrato_original)
      ),
      estratos_departamento_HV022 = length(
        unique(d$estrato_departamento)
      ),
      conglomerados = length(
        unique(d$conglomerado_original)
      )
    )
  })
)

print(resumen, row.names = FALSE)

# -----------------------------------------------------------
# 4. COMPROBAR SI UN CONGLOMERADO APARECE
#    EN MAS DE UN ESTRATO DEPARTAMENTAL
# -----------------------------------------------------------

cat("\n2. CONSISTENCIA CONGLOMERADO-ESTRATO\n")

consistencia <- do.call(
  rbind,
  lapply(2014:2025, function(a) {
    
    d <- datos[
      datos$anio == a,
      ,
      drop = FALSE
    ]
    
    pares <- unique(data.frame(
      conglomerado = d$conglomerado_original,
      estrato = as.character(
        d$estrato_departamento
      )
    ))
    
    conteo <- table(pares$conglomerado)
    
    data.frame(
      anio = a,
      conglomerados = length(conteo),
      conglomerados_en_varios_estratos = sum(
        conteo > 1
      )
    )
  })
)

print(consistencia, row.names = FALSE)

# -----------------------------------------------------------
# 5. CONTAR CONGLOMERADOS POR ESTRATO
#    DEPARTAMENTAL EN LA COHORTE
# -----------------------------------------------------------

cat("\n3. ESTRATOS CON UN SOLO CONGLOMERADO\n")

solitarios <- do.call(
  rbind,
  lapply(2014:2025, function(a) {
    
    d <- datos[
      datos$anio == a,
      ,
      drop = FALSE
    ]
    
    pares <- unique(data.frame(
      estrato = as.character(
        d$estrato_departamento
      ),
      conglomerado = d$conglomerado_original
    ))
    
    conteo <- table(pares$estrato)
    
    data.frame(
      anio = a,
      estratos_observados = length(conteo),
      estratos_un_conglomerado = sum(
        conteo == 1
      ),
      estratos_dos_o_mas = sum(
        conteo >= 2
      )
    )
  })
)

print(solitarios, row.names = FALSE)

# -----------------------------------------------------------
# 6. DETALLE DE LOS AÑOS 2015-2017
# -----------------------------------------------------------

cat("\n4. ESTRATOS POR DEPARTAMENTO 2015-2017\n")

for (a in 2015:2017) {
  
  d <- datos[
    datos$anio == a,
    ,
    drop = FALSE
  ]
  
  tabla <- aggregate(
    estrato_original ~ departamento,
    data = d,
    FUN = function(x) {
      length(unique(x))
    }
  )
  
  names(tabla)[2] <- "codigos_estrato"
  
  cat("\nAÑO:", a, "\n")
  print(tabla, row.names = FALSE)
}

# -----------------------------------------------------------
# 7. GUARDAR RESULTADOS
# -----------------------------------------------------------

destino <- file.path(
  raiz,
  "resultados",
  "modelos"
)

dir.create(
  destino,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  resumen,
  file.path(
    destino,
    "36_comparacion_estratos.csv"
  ),
  row.names = FALSE
)

write.csv(
  consistencia,
  file.path(
    destino,
    "36_consistencia_conglomerados.csv"
  ),
  row.names = FALSE
)

write.csv(
  solitarios,
  file.path(
    destino,
    "36_estratos_solitarios.csv"
  ),
  row.names = FALSE
)

cat("\nSCRIPT 36 FINALIZADO.\n")
