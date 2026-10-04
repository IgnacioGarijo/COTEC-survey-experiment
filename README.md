# COTEC survey experiment

Repositorio de trabajo con los manuscritos y las presentaciones vigentes. Las versiones anteriores están en el historial de Git, no en carpetas duplicadas.

## Documentos actuales

- `4. Latex/paper_A/paper_A_teacher_decisions.pdf`: decisiones docentes; fuente `.tex` en la misma carpeta.
- `4. Latex/paper_A/supporting_analyses.pdf`: análisis conservados fuera del manuscrito (mitades, bootstrap de coeficientes y predicción); fuente `.tex` en la misma carpeta.
- `4. Latex/paper_B/paper_B_policy_scenarios.pdf`: políticas; fuente `.tex` en la misma carpeta.
- `5. Presentation/COTEC/index.html`: presentación COTEC; fuente `ppt_total.qmd`.
- `5. Presentation/Prelim results/index.html`: resultados preliminares; fuente `ppt.qmd`.
- `5. Presentation/SEET2026/presentacion_seet.pdf`: presentación SEET; fuente `.tex`.
- `4. Latex/Pre_registration.pdf`: documento de diseño y cuestionario, conservado como fuente.

Los colores del paper A comparan ahora el texto con la versión editada por los autores guardada en `4458742` (4 de octubre de 2026): azul para contenido conservado, naranja para modificaciones y rojo para adiciones. El paper B y las presentaciones mantienen sus especificaciones propias.

## Estructura

- `1. Data/`: fuentes originales y base limpia, locales y excluidas de Git. Incluye `teacher_population.xlsx` y `cotec_presentation_clean.parquet`; ya no dependen de Descargas ni de otra carpeta de trabajo.
- `2. Code/`: preparación de datos, análisis compartidos, análisis actuales de A y B, construcción de tablas/figuras y comprobaciones.
- `3. Output/shared/`: cálculos compartidos necesarios para los dos papers.
- `3. Output/paper_A/` y `paper_B/`: resultados agregados vigentes y objetos locales necesarios para recalcularlos.
- `3. Output/population/`: extracción de la referencia poblacional y auditoría de celdas.
- `3. Output/figures/` y `tables/`: recursos utilizados por las presentaciones históricas.
- `4. Latex/paper_A/` y `paper_B/`: una fuente canónica y un PDF vigente por paper.
- `5. Presentation/`: fuentes, estilos, recursos y salida vigente de cada presentación.

`2. Code/artifact_manifest.json` enumera los resultados que deben conservarse. Los scripts compartidos limitan sus exportaciones a ese conjunto para no recrear salidas descartadas.

## Reproducir los documentos desde los resultados guardados

Desde la raíz, con Python, R y LaTeX disponibles:

```powershell
python -m pip install -r "2. Code/requirements.txt"
python "2. Code/run_pipeline.py"
```

Esto reconstruye la comparación poblacional, el flujo de participación, figuras y tablas; compila los dos papers y el documento de análisis conservados dos veces; y comprueba dependencias, muestras y referencias. No reescribe la argumentación del manuscrito. Para regenerar solo tablas y figuras, añadir `--no-pdf`. Si un PDF está abierto en una aplicación que lo bloquea, cerrarlo antes de compilar.

Para reconstruir también las tres presentaciones desde sus fuentes y recursos, añadir `--presentations`. Requiere Quarto y los paquetes R utilizados por las diapositivas. El programa admite `RSCRIPT_PATH`, `PDFLATEX_PATH` y `QUARTO_PATH` si no están en PATH. Los archivos HTML con tablas incrustadas mantienen referencias relativas a `3. Output/tables/`; no se debe distribuir el HTML de resultados preliminares aislado de esos recursos.

## Reestimar los análisis de los papers

```powershell
python "2. Code/run_pipeline.py" --analysis
```

Orden: 9 → 11 → 15 → 27 → 28 → 29 → 30 → 31 → 36 → 49, seguido de tablas, figuras y compilación. Incluye validación anidada, mitades y remuestreo y puede tardar. Los datos individuales y los objetos RDS de análisis permanecen fuera de Git. Los scripts 9/11/15 generan las transformaciones y resultados compartidos; no son borradores anteriores del manuscrito.

Si es necesario reconstruir los parquet desde el Excel de encuesta, ejecutar antes `Rscript "2. Code/1.limpieza_datos.R"` desde la raíz. `0. main.R` define las rutas, funciones y vectores de atributos. Los datos originales no se eliminan ni se sobreescriben en la limpieza del repositorio.

El proyecto conserva `renv.lock`; las versiones utilizadas en los análisis recientes están en los archivos `R_session*.txt` de los resultados. La pipeline actual requiere, entre otros, arrow, dplyr, tidyr, fixest, sandwich, ggplot2, jsonlite, ranger, glmnet, fastshap, lme4 y glmmTMB. No se instalan ni actualizan automáticamente paquetes durante la ejecución.

Los scripts 2 y 3 y `presentation_supplement.R` se conservan porque son fuentes de las figuras y tablas de las presentaciones. El suplemento utiliza objetos de una sesión de análisis y no es una pipeline autónoma. Las presentaciones se recompilan con sus recursos guardados; no se sustituyen sus cifras por las del paper A. El script 2 utiliza la instantánea histórica local específica de COTEC.

## Comprobaciones y recuperación

```powershell
Rscript --vanilla "2. Code/check_analysis.R"
python "2. Code/check_repository.py"
```

La descomposición vigente se reproduce con `49. A_final_decomposition.R`: seis atributos, 15 interacciones, harshness observado y seis pendientes aleatorias correlacionadas, sin intercepto aleatorio ni ramas experimentales. El bootstrap reconstruye las referencias en cada remuestra docente; sus salidas se distinguen de las del modelo anterior.

Los PDF vigentes se incluyen en Git. Para recuperaciones anteriores a la limpieza del 29/09/2026, usar la etiqueta local `pre-cleanup-2026-09-29`; conserva también las notas y revisiones que faltaban por versionar. Borrar archivos de la versión de trabajo no elimina las versiones anteriores de los commits. La limpieza no reescribe el historial ni realiza un push.

Los archivos auxiliares de LaTeX, caches, resultados con datos individuales y `tmp/` quedan ignorados. No crear nuevas carpetas fechadas para cada revisión: actualizar los archivos canónicos y hacer commits.
