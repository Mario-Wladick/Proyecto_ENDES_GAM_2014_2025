
# ==================================================
# SCRIPT 35: VERIFICAR ESTRATOS ENDES 2014-2025
# ==================================================

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

cat("\n1. CODIGOS DE HV022 POR AÑO\n")

for (a in 2014:2025) {
  
  d <- datos[datos$anio == a, , drop = FALSE]
  
  codigos <- sort(unique(as.character(d$HV022)))
  
  cat("\nAÑO:", a, "\n")
  cat("Numero de codigos:", length(codigos), "\n")
  
  if (length(codigos) <= 60) {
    print(codigos)
  } else {
    cat("Primeros 20 codigos:\n")
    print(head(codigos, 20))
  }
}

# 2. RELACION ENTRE HV022 Y REGION
cat("\n2. ESTRATOS Y REGIONES, 2015-2017\n")

for (a in 2015:2017) {
  
  d <- datos[datos$anio == a, , drop = FALSE]
  
  cat("\nAÑO:", a, "\n")
  
  print(table(
    HV022 = d$HV022,
    Region = d$HV024
  ))
}

# 3. NUMERO DE CONGLOMERADOS POR ESTRATO
cat("\n3. CONGLOMERADOS POR ESTRATO, 2015-2017\n")

for (a in 2015:2017) {
  
  d <- datos[datos$anio == a, , drop = FALSE]
  
  pares <- unique(data.frame(
    estrato = as.character(d$HV022),
    conglomerado = as.character(d$QHCLUSTER)
  ))
  
  conteo <- aggregate(
    conglomerado ~ estrato,
    data = pares,
    FUN = length
  )
  
  cat("\nAÑO:", a, "\n")
  print(conteo, row.names = FALSE)
}

# 4. COMPROBAR ESCALA DE LOS PESOS
cat("\n4. PESOS DIVIDIDOS POR UN MILLON\n")

datos$peso_millon <- datos$peso_muestral / 1000000

resumen <- aggregate(
  peso_millon ~ anio,
  data = datos,
  FUN = function(x) c(
    minimo = min(x),
    mediana = median(x),
    maximo = max(x)
  )
)

print(resumen, row.names = FALSE)

cat("\nSCRIPT 35 FINALIZADO.\n")
