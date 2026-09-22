# Dos manuscritos: segunda versión

22 de septiembre de 2026. Los archivos del manuscrito original y de la primera versión del 21 de septiembre se conservan sin cambios.

- **Paper A:** `paper_A_teacher_decisions.pdf` y `.tex`. La narrativa conecta criterios académicos compartidos, conflicto entre suspensos y competencias, concentración del desacuerdo dentro de docentes y limitada predicción fuera de muestra.
- **Paper B:** `paper_B_policy_scenarios.pdf` y `.tex`. La narrativa diferencia las respuestas de las recomendaciones medias, los criterios académicos y el apoyo declarado a los escenarios de política.

Los dos artículos son autónomos, declaran que comparten datos y distinguen sus aportaciones. Cuerpo, referencias y anexos están en cada PDF. Se mantiene el mismo formato tipográfico de la versión previa; el desarrollo adicional corresponde a contenido, no a aumentar la letra.

## Qué se ha incorporado

En A: permutaciones con exposiciones fijas; cuatro categorías académicas y tres interacciones; recuperación de las tres columnas OLS, terciles descriptivos, gráficos continuos y los cuatro índices Ha–Hd; bosque con ajuste anidado, ridge, comparación pareada con incertidumbre aproximada por solapamiento, predicción X1 con su muestra disponible y diagnósticos SHAP direccionales y por permutación.

En B: seis contrastes de pendientes académicas y tres pruebas conjuntas, con sus familias Holm; comprobaciones con efectos fijos de tarjeta y pesos por docente; distribución completa de favoritas y menos favoritas y 18 contrastes; bootstrap de 1.999 muestras que reconstruye referentes y reestima pesos; sensibilidad gradual de resultados ausentes; precisión de todas las comparaciones originales de alineamiento y de los contrastes con ranking previo.

Las ampliaciones se ejecutaron y se congelaron antes de componer los nuevos manuscritos. No se ha recogido información nueva ni modificado la muestra principal. Los análisis adicionales se presentan como posteriores al plan inicial.

## Resultados que cambian el énfasis

- El desacuerdo sobre un mismo perfil alcanza el 49,1% cuando hay muchos suspensos pero competencias adecuadas. La varianza del índice entre docentes es 2,11 veces la referencia de permutación. Eso acredita estructura dentro de la tarea, no estabilidad temporal.
- La validación anidada da R² de 3,06% para OLS completo, 3,01% para ridge y 1,48% para el bosque. Las ganancias predictivas siguen siendo pequeñas.
- El recordatorio reduce aproximadamente cinco puntos la pendiente de suspensos. Es una señal exploratoria estable a las especificaciones comprobadas; Holm individual = 0,031 y Holm de la prueba conjunta = 0,058. Se muestran ambos, sin seleccionar solo el favorable.
- La equivalencia ±3 puntos de Revelation–Policy no se mantiene en el bootstrap ampliado. Awareness–Revelation sí mantiene ese resultado bajo las especificaciones principales evaluadas. Esto no demuestra equivalencia de todos los subgrupos ni de los resultados ausentes.

## Colores

Azul: pasajes literales del manuscrito original; naranja: elaboración modificada de su material; rojo: texto o análisis nuevo respecto al original. La comparación es con el manuscrito original, no con la primera versión de los dos papers. `provenance_blue.json` comprueba los diez pasajes azules del cuerpo; los nombres de autor mantienen el azul. Los colores dentro de gráficos representan datos y no procedencia.

## Reproducibilidad

Resultados: `3. Output/two_papers_2026-09-22/`. Scripts nuevos: 15–21 en `2. Code/`. Los scripts 15 y 16 leen los objetos locales producidos por la primera batería, especialmente el script 9. El script 17 verifica muestra, índices, ponderaciones y predicciones; el 21 da formato a figuras a partir de CSV; el 18 genera tablas y el 19 compone ambos textos a partir de material preservado y secciones nuevas. Compilar cada `.tex` dos veces con pdfLaTeX desde esta carpeta; el script 20 renderiza los PDF para inspección.

El manifiesto `results_frozen_manifest.csv` conserva las huellas de las tablas cerradas antes de la redacción. Los objetos RDS con observaciones individuales permanecen excluidos de Git. En `criterio_y_decisiones.md` se documentan familias de pruebas, selección, ajuste predictivo y límites de cada inferencia.

La comprobación editorial no convierte estos borradores en una garantía de aceptación. Para enviar a una revista deben completarse con los registros del estudio las declaraciones de ética, financiación, conflictos de interés y disponibilidad de datos; no se han inventado datos administrativos de ese tipo.
