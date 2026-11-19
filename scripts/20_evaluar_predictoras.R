
# SCRIPT 20: EVALUAR DISPONIBILIDAD DE PREDICTORAS
# ENDES 2014-2025

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_2014_2025_16_candidatas.rds"
))

predictoras <- c(
  "anio",
  "edad",
  "sexo_factor",
  "diabetes",
  "fumo_12meses",
  "alcohol_alguna_vez",
  "alcohol_12meses",
  "asistio_escuela",
  "tiene_seguro",
  "nivel_educativo",
  "region",
  "area_residencia",
  "miembros_hogar",
  "altitud",
  "dias_frutas",
  "dias_verduras"
)

# Base con las 16 variables
completos_16 <- complete.cases(
  datos[, predictoras, drop = FALSE]
)

n_completos_16 <- sum(completos_16)

resultados <- list()

# Excluir una predictora cada vez
for (variable in predictoras) {
  
  seleccion <- setdiff(predictoras, variable)
  
  completos <- complete.cases(
    datos[, seleccion, drop = FALSE]
  )
  
  resultados[[variable]] <- data.frame(
    variable_excluida = variable,
    predictoras_restantes = length(seleccion),
    registros_completos = sum(completos),
    registros_recuperados = sum(completos) - n_completos_16,
    porcentaje_muestra = round(
      100 * mean(completos), 2
    )
  )
}

reporte <- do.call(rbind, resultados)
rownames(reporte) <- NULL

reporte <- reporte[
  order(-reporte$registros_recuperados),
]

cat("\nIMPACTO DE EXCLUIR UNA PREDICTORA\n")
print(reporte, row.names = FALSE)

cat("\nBASE ORIGINAL:", nrow(datos), "\n")
cat("COMPLETOS CON 16:", n_completos_16, "\n")

# Composicion por sexo y año de casos completos
sexo <- as.character(datos$sexo_factor)

composicion <- do.call(rbind, lapply(
  2014:2025,
  function(a) {
    
    do.call(rbind, lapply(
      c("Hombre", "Mujer"),
      function(s) {
        
        grupo <- datos$anio == a &
          !is.na(sexo) & sexo == s
        
        data.frame(
          anio = a,
          sexo = s,
          total = sum(grupo),
          completos_16 = sum(
            grupo & completos_16
          ),
          porcentaje_completos = round(
            100 * sum(grupo & completos_16) /
              sum(grupo), 2
          )
        )
      }
    ))
  }
))

cat("\nCOMPOSICION POR SEXO Y AÑO\n")
print(composicion, row.names = FALSE)

destino <- file.path(
  raiz, "resultados", "inventario"
)

write.csv(
  reporte,
  file.path(
    destino, "20_impacto_excluir_predictoras.csv"
  ),
  row.names = FALSE
)

write.csv(
  composicion,
  file.path(
    destino, "20_completitud_por_sexo_anio.csv"
  ),
  row.names = FALSE
)

cat("\nSCRIPT 20 FINALIZADO.\n")
