# Paper A: revisión integrada del 27 de septiembre de 2026

Documento principal CORREGIDO: `paper_A_teacher_decisions_final.pdf`. El archivo sin el sufijo `_final` es la entrega intermedia, basada en las notas de autores antiguas, y ya no es la entrega vigente. Fuente editable: `paper_A_teacher_decisions.tex`, con `preamble.tex`, `references_A.tex` y `tables/`. Esta versión conserva los originales y el paper B. Azul: texto literal conservado; naranja: material revisado; rojo: material o análisis nuevo. Los colores se comparan con el manuscrito de 21 páginas de `two_papers_2026-09-22`, commit `b73fa3a`, no con la entrega intermedia. Los colores estadísticos de los gráficos no indican procedencia.

## Cambios y estructura

Se aplicaron A-001–A-015 y las notas de autores después de que se guardase la versión correcta. Las respuestas antiguas al informe editorial no se usan como instrucciones de esta ronda. La correspondencia vigente consta en `PLAN_correccion_notas_autores_2026-09-27.md`; el plan anterior documenta la entrega intermedia. Se utiliza APA autor-año, se omiten las denominaciones geográficas identificadoras y se presentan coeficientes con estrellas y SE. Se retiró el material solicitado, se amplió la explicación de los cuestionarios y se simplificaron resultados y métodos.

- Marco conceptual: diagnóstico de dificultad y valoración de beneficios y costes de repetir, con información docente observada y no observada. Incluye los seis atributos; no pretende estimar estructuralmente esas valoraciones.
- Resultados en tres bloques: comparaciones dentro de profesor; harshness, desacuerdo y terciles entre profesores; caracterización, predicción y descomposición de la variación.
- Recursos: ítem «demasiados recursos» en escala 0–10. Edad y experiencia permanecen; antigüedad en el centro se elimina. Se recalcularon asociaciones, predicción, bootstrap y sensibilidades con estas decisiones.
- Apéndice A único: flujo, disponibilidad y pérdida de respuestas, tiempos y comparación poblacional univariante en un panel de cinco gráficos. El Excel suministrado se trata como referencia regional, explicitando periodo y diferencias de cobertura.
- Apéndice B: construcción de harshness, mitades aleatorias con gráfico, medidas alternativas y SE bootstrap. La discusión de varianza permutada se ha eliminado conforme a la nueva petición de retirar la antigua 5.4. Los terciles y sus regresiones descriptivas están en el cuerpo.
- Apéndice C: detalles de predicción y descomposición. Se retiraron los gráficos de relaciones docentes redundantes; la comprobación de secuencias queda brevemente integrada en A.

## Resultados que afectan a la interpretación

La muestra general contiene 2.705 docentes y 22.124 respuestas; la muestra común de caracterización y descomposición, 2.157 docentes. La correlación media de harshness entre mitades es 0,483 en 1.000 particiones. Su intervalo de particiones describe sensibilidad a la división de respuestas, no incertidumbre muestral.

La predicción de harshness con OLS completo alcanza un R² fuera de muestra del 3,72 %; ridge, 3,74 %, y el bosque ajustado, 2,25 %. La asociación con recursos excesivos se mantiene; la de esfuerzo no alcanza el umbral del 5 % al incorporar recursos. Se actualizó la interpretación en consecuencia.

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
10. Desde esta carpeta, ejecutar `pdflatex -jobname=paper_A_teacher_decisions_final -interaction=nonstopmode -halt-on-error paper_A_teacher_decisions.tex` dos veces.
11. Desde la raíz: `python "2. Code/35. audit_A_revision.py"`.

Los scripts 27–29 incluyen remuestreo y validación y pueden tardar. El modelo logístico requiere `glmmTMB`. Las sesiones de R y las huellas de los insumos están en `3. Output/paper_A_2026-09-27/reports/`. Los objetos de trabajo con datos individuales quedan excluidos de Git; sí se conservan los resultados agregados y los scripts. El texto del manuscrito se edita directamente: los scripts regeneran resultados, tablas y gráficos, no la argumentación completa.

El script 37 documenta la transformación de la entrega intermedia a las notas correctas y verifica los bloques azules contra la referencia histórica; no debe ejecutarse para futuras ediciones manuales, porque reconstruye el texto desde aquella entrega intermedia. `provenance_review.json` registra la referencia y la huella de las notas actuales.

La auditoría comprueba tamaños muestrales, conservación del flujo, suma de componentes, convergencia, covariables, referencias a todas las tablas y figuras, bibliografía APA, privacidad textual y compilación. Se revisaron además las 22 páginas renderizadas, incluidas las etiquetas provinciales anónimas. No quedaron referencias sin resolver ni desbordamientos de LaTeX.

Las estrellas se aplican a coeficientes y diferencias para los que existe un contraste: p < 0,05 / 0,01 / 0,001. Los contrastes de interacciones conservan Holm. HC3 y bootstrap usan la aproximación normal indicada; las diferencias predictivas, el contraste corregido con 24 grados de libertad. Las tasas descriptivas, las métricas predictivas y los componentes de varianza no reciben estrellas sin una hipótesis definida. La figura de descomposición muestra ±1 SE bootstrap, no IC del 95 %.
