# Dos manuscritos nuevos

- **Paper A:** `paper_A_teacher_decisions.pdf` y su fuente `.tex`. Información del alumno, desacuerdo entre docentes y límites de predicción con observables.
- **Paper B:** `paper_B_policy_scenarios.pdf` y su fuente `.tex`. Escenarios de políticas, expresión de preferencias, recordatorio de alineamiento y recomendaciones.

Los originales no se han sobrescrito. Los análisis aprobados se cerraron antes de redactar los manuscritos. Ambos borradores están en inglés, conservan la autoría del documento anterior y declaran el uso del mismo estudio.

## Colores

Azul: texto conservado literalmente del manuscrito original. Naranja: contenido anterior reformulado o resultados anteriores recalculados. Rojo: contenido o análisis añadido. Las figuras indican procedencia mediante marco y pie, sin atribuir significado de procedencia a sus colores internos. `provenance_blue.json` verifica los diez pasajes azules contra el original, normalizando únicamente espacios. La autoría también se conserva en azul. Los colores son provisionales y están definidos en `preamble.tex`.

## Resultados y evaluación editorial

Consultar `../../3. Output/two_papers_2026-09-21/reports/cierre_resultados_y_criterio_editorial.md` para el registro de resultados, cambios de interpretación, decisiones de los coautores y valoración de las mejoras necesarias para aspirar a revistas de primera línea. `decision_log.md` relaciona cada respuesta con su implementación. Las tablas numéricas completas están en `../../3. Output/two_papers_2026-09-21/tables/`.

## Reproducción

Desde la raíz del proyecto, ejecutar en orden con R 4.4 y las bibliotecas indicadas en cada archivo:

1. `2. Code/9. two_papers_analysis.R`: muestra, atributos, caracterización, políticas, selección, orden y bootstrap.
2. `2. Code/10. two_papers_prediction.R`: validación por docentes y SHAP fuera de muestra.
3. `2. Code/11. two_papers_sensitivity.R`: alineamiento previo, abandono por política, duración, secuencia y rankings.
4. `2. Code/12. two_papers_validation.R`: comprobaciones numéricas y figuras de presentación.
5. `2. Code/13. build_two_papers_tables.py`: tablas LaTeX, referencias por manuscrito y comprobación de texto azul.

Después, en esta carpeta, ejecutar `pdflatex -interaction=nonstopmode -halt-on-error paper_A_teacher_decisions.tex` dos veces y lo mismo para `paper_B_policy_scenarios.tex`. `2. Code/14. render_two_papers.py` renderiza las páginas para inspección. No se modifica ni se reemplaza el código del análisis anterior.

La revisión visual y los manifiestos de resultados están documentados en `../../3. Output/two_papers_2026-09-21/reports/`. Las fuentes requieren las imágenes de esa carpeta de resultados; no mover únicamente el `.tex` sin sus dependencias.

Estos son borradores completos para revisión de coautores. Antes de una remisión deben verificarse los datos administrativos del estudio que no están acreditados en los archivos consultados: aprobación ética, financiación, fechas definitivas de campo, declaraciones de autoría y condiciones de acceso a los datos. No se han inventado ni afirmado esas certificaciones.
