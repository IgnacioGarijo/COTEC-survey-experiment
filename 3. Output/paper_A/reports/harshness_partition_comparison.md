# Comparación interna: descomposición con harshness

Fecha: 29 de septiembre de 2026. Análisis solicitado por los autores; no se ha cambiado el manuscrito.

## Diseño de la comparación

Misma muestra del modelo actual: 18.608 decisiones de 2.157 profesores. Mismos seis atributos del alumno, interacción suspensos × competencias, características profesionales/demográficas, creencias y contexto de asignación. Se conserva el indicador `hb` que ya utiliza el paper, calculado con las respuestas originales y referencias de otros profesores ante la misma tarjeta; no se recalcula restringiendo las referencias a la muestra de casos completos.

1. **Actual:** modelo lineal con intercepto aleatorio de profesor. Se reutiliza el modelo guardado y se comprueba que la descomposición reproduce exactamente la tabla vigente.
2. **Comparador:** regresión lineal con los mismos bloques observados, sin intercepto aleatorio ni harshness.
3. **Alternativa solicitada:** regresión lineal con esos bloques y `hb`, sin intercepto aleatorio. Harshness sustituye al componente docente residual en la presentación.

La alternativa reparte el R² ordinario dentro de muestra mediante Shapley: se calcula el incremento de ajuste de cada bloque en todos los órdenes posibles de incorporación y se promedian los incrementos. Así, las contribuciones suman el ajuste total y no se cuenta dos veces su información compartida. Este reparto coincide con proyectar el predictor completo de OLS sobre cada subconjunto de bloques, siguiendo la lógica del reparto actual. No es una incorporación secuencial con un orden elegido arbitrariamente.

## Resultados

Porcentaje de variación, ponderando cada decisión por igual:

| Componente | Modelo actual | OLS sin harshness | OLS con harshness |
|---|---:|---:|---:|
| Atributos del alumno | 48,36 | 48,36 | 48,42 |
| Características profesionales/demográficas | 0,34 | 0,34 | 0,18 |
| Creencias y actitudes | 0,35 | 0,35 | 0,18 |
| Contexto de asignación | 0,11 | 0,11 | 0,06 |
| Componente docente residual | 8,12 | — | — |
| Harshness | — | — | 13,29 |
| Variación residual | 42,72 | 50,84 | 37,88 |
| Total recogido por el modelo | **57,28** | **49,16** | **62,12** |

Las pequeñas discrepancias en las sumas impresas se deben al redondeo. Los cálculos sin redondear suman 100 %.

Añadir harshness al comparador OLS aumenta el R² en **12,95 puntos porcentuales**, que equivalen al **25,48 % de la variación que quedaba sin recoger**. Su cuota Shapley es **13,29 %**, ligeramente distinta porque reparte la información compartida promediando todos los órdenes de incorporación. Las características docentes y las creencias reducen su cuota al compartir información con harshness; esto no implica que sus asociaciones hayan desaparecido.

El coeficiente de harshness es 0,9969. Un aumento de 10 puntos porcentuales en harshness se asocia con unos 9,97 puntos adicionales en el valor ajustado de la recomendación, manteniendo constantes los demás regresores. Es una asociación dentro de muestra entre decisiones y un indicador construido a partir de ellas.

## Interpretación y límites de la comparación

La alternativa da una respuesta descriptiva clara: el conjunto de variables y el resumen de severidad calculado en el estudio reconstruye el **62,12 % de la variación observada de las decisiones**. No es un porcentaje de decisiones acertadas ni una evaluación de predicción fuera de muestra. Se puede presentar como descomposición del ajuste dentro de muestra, conservando harshness como medida central del paper.

El 13,29 % de harshness y el 8,12 % del componente docente residual no estiman exactamente lo mismo. Harshness incorpora tanto diferencias comunes al profesor como fluctuaciones particulares de las respuestas que se promedian para construirlo; el modelo actual separa un componente común y un residuo específico bajo sus supuestos. La diferencia entre ambos porcentajes no identifica por sí sola cuánto es ruido.

Los denominadores también difieren ligeramente: el modelo actual normaliza por la suma de la varianza del predictor fijo y las dos varianzas estimadas (0,2499433); OLS utiliza la varianza observada de la decisión (0,2493424). El comparador OLS sin harshness permite aislar el incremento por incorporar el indicador con exactamente el mismo denominador, muestra y método de ajuste.

Como diagnóstico del modelo lineal, el 21,73 % de los valores ajustados de la alternativa cae fuera de [0,1]. La regresión sirve aquí para una descomposición de variación; sus valores ajustados no deben presentarse como probabilidades individuales calibradas. El R² ajustado convencional es 62,06 %, pero su penalización por número de regresores no corrige la reutilización de respuestas para construir harshness.

Esta comparación ofrece estimaciones puntuales, no nuevos errores estándar ni contrastes. Si se adoptase como resultado definitivo con incertidumbre, el procedimiento de remuestreo tendría que recalcular harshness y sus referencias, no tratarlo como un dato externo fijo.

## Reproducción

Desde la raíz del repositorio: `Rscript "2. Code/37. A_harshness_partition_comparison.R"`.

Entradas: `analysis_objects.rds` y `variance_models.rds` en esta carpeta; tabla vigente `../tables/variance_partition.csv`. Salidas: `harshness_partition_comparison.csv`, `harshness_partition_diagnostics.csv` y `R_session_harshness_partition.txt`. El script comprueba la identidad de muestra y respuestas, la reproducción del resultado vigente y la suma de componentes.
