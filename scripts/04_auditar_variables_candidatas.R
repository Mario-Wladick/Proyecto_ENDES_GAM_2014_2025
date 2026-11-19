# ENDES 2014-2025 - Auditoria inicial de variables candidatas
# Este script NO modifica los archivos originales ni ajusta modelos.
# El porcentaje reportado es de valores presentes; aun se requiere
# revisar codigos especiales (no sabe, no corresponde, etc.).

if (!requireNamespace("haven", quietly = TRUE) ||
    !requireNamespace("readr", quietly = TRUE)) {
  stop("Instala los paquetes haven y readr con install.packages().")
}

raiz <- "."
entrada <- file.path(raiz, "datos", "extraidos")
salida <- file.path(raiz, "resultados", "inventario")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

candidatas <- c(
  "QS23", "QSSEXO", "QS24", "QS25N", "QS26", "QS27",
  "QS103U", "QS103C", "QS109", "QS200", "QS201", "QS202",
  "QS206", "QS208", "QS210", "QS213C", "QS219C"
)

resultado <- list()
resumen <- list()
for (anio in 2014:2025) {
  cat("\nRevisando", anio, "...\n")
  archivos <- list.files(file.path(entrada, as.character(anio)),
                         pattern = "^CSALUD01(_[0-9]{4})?\\.(sav|csv)$",
                         recursive = TRUE, full.names = TRUE,
                         ignore.case = TRUE)
  if (length(archivos) != 1) {
    stop("No se encontro exactamente un archivo de salud en ", anio)
  }
  archivo <- archivos[[1]]
  if (tolower(tools::file_ext(archivo)) == "sav") {
    datos <- haven::read_sav(archivo)
  } else {
    sep <- if (anio %in% c(2020, 2022)) ";" else ","
    datos <- readr::read_delim(
      archivo, delim = sep,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE, progress = FALSE
    )
    errores <- readr::problems(datos)
    if (nrow(errores) > 0) {
      stop("Problemas de lectura en ", anio, ": ", nrow(errores))
    }
  }
  if (!all(c("QS102", "QS104") %in% names(datos))) {
    stop("No estan QS102 y QS104 en ", anio)
  }
  # Se filtra solo la cohorte preliminar de diagnostico y respuesta valida.
  diagnostico <- suppressWarnings(as.numeric(as.character(datos[["QS102"]])))
  medicamentos <- suppressWarnings(as.numeric(as.character(datos[["QS104"]])))
  cohorte <- !is.na(diagnostico) & diagnostico == 1 &
    !is.na(medicamentos) & medicamentos %in% c(1, 2)
  n <- sum(cohorte)
  resumen[[as.character(anio)]] <- data.frame(
    anio = anio, registros_originales = nrow(datos),
    cohorte_preliminar = n
  )
  for (variable in candidatas) {
    existe <- variable %in% names(datos)
    if (existe) {
      x <- trimws(as.character(datos[[variable]][cohorte]))
      presente <- !is.na(x) & nzchar(x)
      distintos <- length(unique(x[presente]))
      presentes <- sum(presente)
    } else {
      distintos <- NA_integer_
      presentes <- 0L
    }
    resultado[[length(resultado) + 1L]] <- data.frame(
      anio = anio, variable = variable, existe = existe,
      n_cohorte = n, n_con_dato = presentes,
      pct_con_dato = if (n > 0) round(100 * presentes / n, 2) else NA_real_,
      valores_distintos = distintos
    )
  }
  rm(datos)
  invisible(gc(verbose = FALSE))
}

revision <- do.call(rbind, resultado)
resumen <- do.call(rbind, resumen)
write.csv(revision, file.path(salida, "auditoria_variables_candidatas.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(resumen, file.path(salida, "resumen_cohorte_preliminar.csv"),
          row.names = FALSE)
cat("\nAUDITORIA FINALIZADA.\n")
print(resumen)
cat("\nImportante: revisar codigos especiales y saltos de preguntas antes de elegir variables.\n")
