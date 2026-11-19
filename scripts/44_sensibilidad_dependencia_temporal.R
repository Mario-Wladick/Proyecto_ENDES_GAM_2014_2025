
# ===================================================
# SCRIPT 44: SENSIBILIDAD A DEPENDENCIA TEMPORAL
# ENDES 2014-2025
# ===================================================

raiz <- "."

ruta <- file.path(raiz, "resultados", "modelos")

datos <- read.csv(file.path(
  ruta, "40_sensibilidad_estratos.csv"
))

# Comparaciones originales
pares <- data.frame(
  inicial = c(2014, 2020, 2014, 2023),
  final = c(2020, 2025, 2025, 2025)
)

# Escenarios HIPOTETICOS, no estimados
correlaciones <- c(-0.3, 0, 0.3, 0.6)

calcular <- function(a, b, rho, metodo) {
  
  x <- datos[datos$anio == a, ]
  y <- datos[datos$anio == b, ]
  
  columna <- if (metodo == "adjust") {
    "SE_adjust"
  } else {
    "SE_average"
  }
  
  se1 <- x[[columna]]
  se2 <- y[[columna]]
  
  diferencia <- y$porcentaje - x$porcentaje
  
  # Varianza con covarianza hipotetica
  var_diferencia <- (
    se1^2 + se2^2 - 2 * rho * se1 * se2
  )
  
  se_diferencia <- sqrt(var_diferencia)
  
  z <- diferencia / se_diferencia
  p <- 2 * pnorm(-abs(z))
  
  data.frame(
    inicial = a,
    final = b,
    metodo = metodo,
    rho = rho,
    diferencia_pp = diferencia,
    error_estandar = se_diferencia,
    p_valor = p
  )
}

resultados <- do.call(
  rbind,
  lapply(c("adjust", "average"), function(m) {
    do.call(
      rbind,
      lapply(correlaciones, function(r) {
        do.call(
          rbind,
          lapply(seq_len(nrow(pares)), function(i) {
            calcular(
              pares$inicial[i],
              pares$final[i],
              r,
              m
            )
          })
        )
      })
    )
  })
)

# Holm dentro de cada escenario y metodo
resultados$p_holm <- NA_real_

for (m in unique(resultados$metodo)) {
  for (r in correlaciones) {
    
    indices <- which(
      resultados$metodo == m &
        resultados$rho == r
    )
    
    resultados$p_holm[indices] <- p.adjust(
      resultados$p_valor[indices],
      method = "holm"
    )
  }
}

resultados$significativo <- resultados$p_holm < 0.05

cat("\n1. SENSIBILIDAD - METODO ADJUST\n")

mostrar <- resultados[
  resultados$metodo == "adjust",
  c("inicial", "final", "rho", "p_holm",
    "significativo")
]

print(mostrar, row.names = FALSE, digits = 5)

cat("\n2. SENSIBILIDAD - METODO AVERAGE\n")

mostrar2 <- resultados[
  resultados$metodo == "average",
  c("inicial", "final", "rho", "p_holm",
    "significativo")
]

print(mostrar2, row.names = FALSE, digits = 5)

# Evaluar estabilidad en los escenarios examinados
resumen <- aggregate(
  significativo ~ inicial + final + metodo,
  data = resultados,
  FUN = function(x) {
    if (all(x)) return("Si en todos")
    if (!any(x)) return("No en ninguno")
    "Depende del escenario"
  }
)

cat("\n3. ESTABILIDAD DE CONCLUSIONES\n")
print(resumen, row.names = FALSE)

write.csv(
  resultados,
  file.path(
    ruta, "44_sensibilidad_temporal.csv"
  ),
  row.names = FALSE
)

write.csv(
  resumen,
  file.path(
    ruta, "44_resumen_estabilidad.csv"
  ),
  row.names = FALSE
)

cat("\nSCRIPT 44 FINALIZADO.\n")
cat(
  "Las correlaciones son escenarios",
  "hipoteticos, no estimaciones del diseno.\n"
)
