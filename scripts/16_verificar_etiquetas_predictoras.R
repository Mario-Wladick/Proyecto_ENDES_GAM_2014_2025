
# SCRIPT 16: VERIFICAR ETIQUETAS ORIGINALES
# ENDES 2015 Y 2019

raiz <- "."

variables <- c(
  "QS104",
  "QS109",
  "QS200",
  "QS201",
  "QS202",
  "QS206",
  "QS208",
  "QS210",
  "QS213C",
  "QS219C",
  "QS24",
  "QS25N",
  "QS26",
  "QS27"
)

for (anio in c(2015, 2019)) {
  
  datos <- readRDS(file.path(
    raiz, "datos", "integrados",
    paste0("ENDES_integrada_", anio, ".rds")
  ))
  
  cat("\n============================\n")
  cat("ENDES", anio, "\n")
  cat("============================\n")
  
  for (variable in variables) {
    
    if (!variable %in% names(datos)) {
      cat("\n", variable, ": NO DISPONIBLE\n")
      next
    }
    
    x <- datos[[variable]]
    
    cat("\nVARIABLE:", variable, "\n")
    
    descripcion <- attr(x, "label")
    etiquetas <- attr(x, "labels")
    
    cat("PREGUNTA:\n")
    print(
      if (is.null(descripcion))
        "Sin descripcion"
      else descripcion
    )
    
    cat("CODIGOS Y RESPUESTAS:\n")
    print(
      if (is.null(etiquetas))
        "Sin etiquetas"
      else etiquetas
    )
  }
}

cat("\nREVISION DE ETIQUETAS FINALIZADA.\n")
