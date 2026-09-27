# Paper A: revisión integrada del 27 de septiembre de 2026

Documento principal: `paper_A_teacher_decisions.pdf`. Fuente editable: `paper_A_teacher_decisions.tex`, con `preamble.tex`, `references_A.tex` y `tables/`. Esta versión conserva los originales y el paper B. Azul: texto literal conservado; naranja: material revisado; rojo: material o análisis nuevo. Los colores estadísticos de los gráficos no indican procedencia.

## Cambios y estructura

Se aplicaron los dos archivos de notas, dando prioridad a las decisiones posteriores cuando sustituían propuestas previas. La correspondencia completa consta en `PLAN_revision_A_2026-09-27.md`, en la raíz del proyecto.

- Marco conceptual: diagnóstico de dificultad y valoración de beneficios y costes de repetir, con información docente observada y no observada. Incluye los seis atributos; no pretende estimar estructuralmente esas valoraciones.
- Resultados en tres bloques: comparaciones dentro de profesor; harshness, desacuerdo y terciles entre profesores; caracterización, predicción y descomposición de la variación.
- Recursos: ítem «demasiados recursos» en escala 0–10. Edad y experiencia permanecen; antigüedad en el centro se elimina. Se recalcularon asociaciones, predicción, bootstrap y sensibilidades con estas decisiones.
- Apéndice A único: flujo, disponibilidad y pérdida de respuestas, tiempos y comparación poblacional univariante en un panel de cinco gráficos. El Excel suministrado se trata como referencia regional, explicitando periodo y diferencias de cobertura.
- Apéndice B: construcción de harshness, mitades aleatorias con gráfico, permutación secundaria, medidas alternativas e incertidumbre. Los terciles y sus regresiones descriptivas están en el cuerpo.
- Apéndice C: detalles de predicción y descomposición. Se retiraron los gráficos de relaciones docentes redundantes; la comprobación de secuencias queda brevemente integrada en A.

## Resultados que afectan a la interpretación

La muestra general contiene 2.705 docentes y 22.124 respuestas; la muestra común de caracterización y descomposición, 2.157 docentes. La correlación media de harshness entre mitades es 0,483 en 1.000 particiones. Su intervalo de particiones describe sensibilidad a la división de respuestas, no incertidumbre muestral.

La predicción de harshness con OLS completo alcanza un R² fuera de muestra del 3,72 %; ridge, 3,74 %, y el bosque ajustado, 2,25 %. La asociación con recursos excesivos se mantiene; la de esfuerzo tiene un intervalo que incluye cero al incorporar recursos. Se actualizó la interpretación en consecuencia.

La descomposición lineal asigna 48,36 % a atributos del alumno, 0,34 % a características profesionales/demográficas, 0,35 % a creencias, 0,11 % al contexto de asignación, 8,12 % al componente docente residual y 42,72 % al residual de decisiones. El reparto entre bloques usa una regla de proyección Shapley explícita sobre el predictor completo; no es una partición causal ni un resultado predictivo fuera de muestra. Tampoco introduce harshness como predictor de las respuestas que lo construyen.

El componente docente es una varianza de interceptos aleatorios ajustada por covariables, no la varianza de los efectos fijos estimados. Su magnitud depende del modelo: la sensibilidad logística en escala observada da 12,36 %. El manuscrito informa esta diferencia y las limitaciones de la aproximación lineal para una respuesta binaria. No se presenta el 8,12 % como una cantidad universal ni como un rasgo permanente.

## Reproducción y comprobación

Ejecutar desde la raíz, conservando los datos y objetos históricos de entrada:

1. `Rscript "2. Code/27. revise_A_analysis.R"`
2. `Rscript "2. Code/28. revised_A_prediction.R"`
3. `Rscript "2. Code/29. A_variance_decomposition.R"`
4. `Rscript "2. Code/30. A_participation.R"`
5. `Rscript "2. Code/31. A_population_aggregates.R"`
6. `Rscript "2. Code/36. A_logit_observed_variance.R"`
7. `python "2. Code/32. A_revision_figures.py"`
8. `python "2. Code/33. A_revision_tables.py"`
9. `python "2. Code/34. finalize_A_sources.py"`
10. Desde esta carpeta, ejecutar `pdflatex -interaction=nonstopmode -halt-on-error paper_A_teacher_decisions.tex` dos veces.
11. Desde la raíz: `python "2. Code/35. audit_A_revision.py"`.

Los scripts 27–29 incluyen remuestreo y validación y pueden tardar. El modelo logístico requiere `glmmTMB`. Las sesiones de R y las huellas de los insumos están en `3. Output/paper_A_2026-09-27/reports/`. Los objetos de trabajo con datos individuales quedan excluidos de Git; sí se conservan los resultados agregados y los scripts. El texto del manuscrito se edita directamente: los scripts regeneran resultados, tablas y gráficos, no la argumentación completa.

La auditoría comprueba tamaños muestrales, conservación del flujo, suma de componentes, convergencia, covariables, referencias a todas las tablas y figuras, bibliografía y compilación. Se revisaron además las páginas renderizadas. No quedaron referencias sin resolver ni desbordamientos de LaTeX.
