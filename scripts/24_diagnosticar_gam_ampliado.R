
# SCRIPT 24: DIAGNOSTICAR MODELO GAM AMPLIADO
# ENDES 2014-2025

raiz <- "."

datos <- readRDS(file.path(
  raiz, "datos", "procesados",
  "ENDES_GAM_15_predictoras_categorias.rds"
))

modelo <- readRDS(file.path(
  raiz, "resultados", "modelos",
  "23_gam_ampliado.rds"
))

cat("\n1. RELACION ENTRE VARIABLES DE ALCOHOL\n")

print(table(
  Alguna_vez = datos$alcohol_alguna_vez,
  Ultimos_12_meses = datos$alcohol_12meses,
  useNA = "ifany"
))

cat("\n2. DISPONIBILIDAD DE CATEGORIAS\n")

variables <- c(
  "diabetes",
  "fumo_12meses",
  "alcohol_alguna_vez",
  "alcohol_12meses",
  "asistio_escuela",
  "tiene_seguro",
  "nivel_educativo",
  "frutas_grupo"
)

for (variable in variables) {
  cat("\n", variable, "\n")
  print(table(datos[[variable]], useNA = "ifany"))
}

cat("\n3. RANGO DEL MODELO\n")

cat("Rango estimado:", modelo$rank, "\n")
cat("Numero de coeficientes:", length(coef(modelo)), "\n")

cat("\n4. COEFICIENTES NO ESTIMABLES O NO FINITOS\n")

coeficientes <- coef(modelo)

print(coeficientes[!is.finite(coeficientes)])

cat("\n5. CONCURVIDAD ENTRE COMPONENTES\n")

resultado <- mgcv::concurvity(
  modelo,
  full = TRUE
)

print(resultado)

cat("\nDIAGNOSTICO FINALIZADO.\n")
