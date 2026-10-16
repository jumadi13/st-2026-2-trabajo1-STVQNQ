# Trabajo 1 — Series de Tiempo (STVQNQ)

**Universidad Nacional de Colombia, sede Medellín · 2026-II**  
Equipo: STVQNQ  
Autores: Juan Manuel Diaz Larrea y Sofia Valentina Lopez Ramirez

## Objetivo

Describir tres series, estudiar el operador de rezago y la dependencia temporal, comparar métodos de pronóstico y formular recomendaciones con métricas de evaluación por orígenes móviles. Las organizaciones y cifras del caso son simuladas, según el enunciado.

## Estructura del repositorio

- `datos/`: tres CSV originales asignados; no deben modificarse manualmente.
- `R/`: funciones de lectura, gráficos, operadores, dependencia, métodos y evaluación.
- `analisis/correr-todo.R`: flujo reproducible que calcula resultados, tablas, gráficos, metadatos de sesión, nota ejecutiva y renderiza el informe HTML si Quarto está disponible.
- `analisis/*.csv`: resultados derivados que se regeneran con el flujo.
- `analisis/resultados.rds`: objetos completos del análisis.
- `figs/`: figuras creadas por el flujo.
- `informe/informe.qmd`: fuente del informe técnico.
- `informe/informe.html`: informe renderizado; debe regenerarse después de ejecutar el flujo.
- `informe/generar-nota.R`: script para generar la nota ejecutiva PDF de una página.
- `informe/nota-interesados.pdf`: nota ejecutiva generada.
- `sesion-info.txt`: información de la sesión de R usada en la última ejecución.

## Requisitos

- R 4.3 o posterior.
- Paquetes base de R; el análisis utiliza funciones de `stats` y gráficos base.
- Quarto instalado y disponible en el `PATH` para regenerar el informe HTML. El flujo de análisis y la nota PDF se ejecutan aunque Quarto no esté instalado, pero el HTML existente quedará sin actualizar y se mostrará una advertencia.

## Ejecución completa

Abra R/RStudio en la **raíz** del repositorio (la carpeta que contiene `R/`, `datos/`, `analisis/` e `informe/`) y ejecute:

```r
source("analisis/correr-todo.R")
```

El script valida la estructura, lee las tres series, ejecuta controles de implementación, ajusta y evalúa métodos, y regenera los CSV de resultados, `analisis/resultados.rds`, las figuras de `figs/`, `sesion-info.txt` y `informe/nota-interesados.pdf`. Si Quarto está instalado, también ejecuta `quarto render informe/informe.qmd` y actualiza el HTML. Para renderizarlo manualmente desde la raíz:

```sh
quarto render informe/informe.qmd
```

## Archivos derivados y reproducibilidad

Los CSV de `analisis/` son productos derivados. No edite manualmente los resultados para corregir cifras; corrija el código fuente y vuelva a ejecutar el flujo. El flujo debe crear todas las tablas y figuras referenciadas por `informe/informe.qmd`, incluidas las curvas MSE (`figs/s1_curvas_MSE.png`, `figs/s2_curvas_MSE.png` y `figs/s3_curvas_MSE.png`).

La evaluación compara RMSE, MAD y MAPE por horizontes y orígenes móviles. Los parámetros se seleccionan usando el tramo inicial de entrenamiento; los resultados de prueba no deben utilizarse para elegir parámetros. Las bandas de pronóstico son heurísticas y no constituyen garantías probabilísticas.

## Entregables

1. Informe técnico reproducible (`informe/informe.qmd` y su HTML renderizado).
2. Nota ejecutiva de una página (`informe/nota-interesados.pdf`).
3. Código, datos originales, tablas derivadas y figuras necesarias para auditar el análisis.

## Uso de IA

Se utilizó asistencia de IA como apoyo para estructurar el repositorio, implementar y revisar métodos, comparar resultados y redactar partes y correciones del informe. 
