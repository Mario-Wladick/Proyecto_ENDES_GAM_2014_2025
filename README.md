# Análisis ENDES 2014–2025 mediante GAM

## Descripción

Investigación sobre la ausencia reportada de compra
de medicamentos para controlar la presión alta en
personas de 15 años o más con diagnóstico previo
reportado de hipertensión en el Perú, utilizando
microdatos de ENDES 2014–2025.

## Objetivo

Analizar la evolución temporal de la ausencia
reportada de compra mediante modelos aditivos
generalizados (GAM).

## Datos

- Fuente: INEI, ENDES 2014–2025.
- Muestra analítica: 32 806 registros.
- Variable respuesta: QS104 = 2 (no compró).
- Modelo básico: año, edad y sexo.
- Modelo ampliado: 15 predictoras.

La ausencia reportada de compra no equivale
necesariamente a ausencia de tratamiento o
falta de adherencia.

## Resultados principales

| Modelo | Log-loss | Brier Score |
|---|---:|---:|
| GAM básico | 0.5397 | 0.1799 |
| GAM ampliado | 0.5264 | 0.1743 |

Validación temporal: entrenamiento 2014–2022
y evaluación 2023–2025.

El GAM ampliado mejora las métricas promedio,
aunque subestima las proporciones observadas
en los años recientes.

## Estructura del proyecto

- `informe_ENDES.qmd`: informe académico Quarto.
- `datos/publicos/`: resultados estadísticos agregados.
- `scripts/`: scripts R del análisis.
- `pipeline_publicacion.R`: genera el informe HTML.
- `pipeline.R`: verificaciones iniciales.
- `assets/`: recursos gráficos.

## Reproducir el informe

Se requiere R, Quarto y el paquete knitr.

Desde la carpeta principal del proyecto:

```r
source("pipeline_publicacion.R")
```

Esto genera `informe_ENDES.html` utilizando
los resultados agregados disponibles.

La reproducción del análisis completo desde
los microdatos originales requiere obtener
los módulos ENDES del INEI y ejecutar las
etapas de preparación y modelado.

## Fuente de datos

INEI:
https://proyectos.inei.gob.pe/microdatos/

## Universidad

Universidad Nacional del Altiplano – Puno.

Escuela Profesional de Ingeniería Estadística
e Informática.

Curso: Ciencia de Datos, 2026.

## Publicacion en Zenodo

Este proyecto esta archivado en Zenodo.

**DOI de la version v1.0.0:**
https://doi.org/10.5281/zenodo.24383832

**DOI general del proyecto:**
https://doi.org/10.5281/zenodo.24383831

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24383832.svg)](https://doi.org/10.5281/zenodo.24383832)
